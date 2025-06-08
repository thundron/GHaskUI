{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module GHaskUI.Platform.Windows (
    WindowsPlatform(..), -- Export the data constructor
    createBasicWindow,   -- Export if it's meant to be used directly (likely not for external API)
    runMessageLoop,      -- Export if it's meant to be used directly (likely not for external API)
    -- Other specific Windows exports if any
    ) where

-- Consolidated Imports:
import Foreign.C.String (CString, withCString)
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, IntPtr, Ptr, nullPtr, intPtrToPtr, ptrToIntPtr)
import Foreign.Storable (Storable(..))
import GHC.Stable (StablePtr, newStablePtr, deRefStablePtr, freeStablePtr, castStablePtrToPtr, castPtrToStablePtr)

import Control.Monad (unless, void, when)
-- import Control.Monad () -- Likely not needed if specific functions are imported

import Data.Bits ((.&.), (.|.), shiftR)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
-- import Data.Int () -- Likely not needed for instances if not used directly
import Data.Maybe (isJust)
import Data.Word (Word16, Word32)


import GHaskUI.Core (ButtonHandle(..), Event(..), EventHandler, LabelHandle(..), Point, Size, SomePlatform(SomePlatform), UICreator(..), WindowHandle(..), unGHaskUIApp)

-- Helper Win32 type aliases
type HWND = Ptr ()
type HMENU = Ptr ()
type HINSTANCE = Ptr ()
type HICON = Ptr ()
type HCURSOR = Ptr ()
type HBRUSH = Ptr ()
type LPVOID = Ptr ()
type LPCSTR = Ptr CChar

type UINT = CUInt
type ATOM = Word16
type WORD = Word16
type DWORD = Word32
type BOOL = CInt

-- For 64-bit compatibility and consistency with modern WinAPI definitions:
type WPARAM    = CUIntPtr
type LPARAM    = CIntPtr
type LRESULT   = CIntPtr
type LONG_PTR  = CIntPtr

-- Window Style constants
ws_OVERLAPPED, ws_CAPTION, ws_SYSMENU, ws_THICKFRAME, ws_MINIMIZEBOX, ws_MAXIMIZEBOX :: DWORD
ws_OVERLAPPED       = 0x00000000
ws_CAPTION          = 0x00C00000
ws_SYSMENU          = 0x00080000
ws_THICKFRAME       = 0x00040000 -- Same as WS_SIZEBOX
ws_MINIMIZEBOX      = 0x00020000
ws_MAXIMIZEBOX      = 0x00010000

-- Other constants
colorWindow :: CInt
colorWindow = 5 -- COLOR_WINDOW

swShowNormal :: CInt
swShowNormal = 1

idcArrowLPCSTR :: CString
idcArrowLPCSTR = intPtrToPtr 32512 -- IDC_ARROW as LPCSTR ( MAKEINTRESOURCE(32512) )

wmDestroy, wmClose, wmCommand, wmGetMinMaxInfo :: UINT
wmDestroy = 0x0002
wmClose   = 0x0010
wmCommand = 0x0111
wmGetMinMaxInfo = 0x0024

bnClicked :: WORD
bnClicked = 0 -- BN_CLICKED

-- Helper for loword/hiword
-- Note: WPARAM is CUIntPtr (pointer-sized), WORD is Word16.
-- These helpers assume the relevant data is in the lower bits, which is typical for WinAPI.
loword :: WPARAM -> WORD
loword w = fromIntegral (w .&. 0xFFFF)

hiword :: WPARAM -> WORD
hiword w = fromIntegral ((w `shiftR` 16) .&. 0xFFFF)

-- The FFI imports and function definitions will follow after this line.

-- Structure for MINMAXINFO
data POINT_STRUCT = POINT_STRUCT { ptx :: CLong, pty :: CLong }

instance Storable POINT_STRUCT where
  sizeOf _ = sizeOf (undefined :: CLong) * 2
  alignment _ = alignment (undefined :: CLong)
  peek ptr = POINT_STRUCT <$> peekByteOff ptr 0 <*> peekByteOff ptr (sizeOf (undefined :: CLong))
  poke ptr (POINT_STRUCT x y) = pokeByteOff ptr 0 x >> pokeByteOff ptr (sizeOf (undefined :: CLong)) y

data MINMAXINFO = MINMAXINFO
  { ptReserved     :: POINT_STRUCT -- Reserved; do not use. Included for FFI struct layout compatibility.
  , ptMaxSize      :: POINT_STRUCT -- Maximum maximized size. Included for FFI struct layout compatibility.
  , ptMaxPosition  :: POINT_STRUCT -- Position of maximized window. Included for FFI struct layout compatibility.
  , ptMinTrackSize :: POINT_STRUCT -- Minimum tracking size.
  , ptMaxTrackSize :: POINT_STRUCT -- Maximum tracking size.
  }

-- | A dummy function to ensure all MINMAXINFO fields are considered "used"
-- by GHC, preventing -Wunused-top-binds warnings for FFI struct fields
-- that are necessary for memory layout but not directly used in Haskell logic.
_touchMinMaxInfoFields :: MINMAXINFO -> ()
_touchMinMaxInfoFields info =
  let _ = ptReserved info
      _ = ptMaxSize info
      _ = ptMaxPosition info
  in ()

instance Storable MINMAXINFO where
  sizeOf _ = sizeOf (undefined :: POINT_STRUCT) * 5
  alignment _ = alignment (undefined :: POINT_STRUCT)
  peek ptr = MINMAXINFO <$>
    peekByteOff ptr (sizeOfPOINT * 0) <*>
    peekByteOff ptr (sizeOfPOINT * 1) <*>
    peekByteOff ptr (sizeOfPOINT * 2) <*>
    peekByteOff ptr (sizeOfPOINT * 3) <*>
    peekByteOff ptr (sizeOfPOINT * 4)
    where sizeOfPOINT = sizeOf (undefined :: POINT_STRUCT)
  poke ptr (MINMAXINFO p1 p2 p3 p4 p5) = do
    pokeByteOff ptr (sizeOfPOINT * 0) p1
    pokeByteOff ptr (sizeOfPOINT * 1) p2
    pokeByteOff ptr (sizeOfPOINT * 2) p3
    pokeByteOff ptr (sizeOfPOINT * 3) p4
    pokeByteOff ptr (sizeOfPOINT * 4) p5
    where sizeOfPOINT = sizeOf (undefined :: POINT_STRUCT)


-- Window Styles for Controls
wsChild :: DWORD
wsChild = 0x40000000

wsVisible :: DWORD
wsVisible = 0x10000000

wmQuit :: UINT
wmQuit = 0x0012

-- Button Styles
bsPushButton :: DWORD
bsPushButton = 0x00000000 :: DWORD

-- Static Control Styles (DWORD)
ss_RIGHT :: DWORD
ss_RIGHT       = 0x00000002

-- FFI Imports for MessageBoxA

-- FFI Imports for window creation
#ifdef mingw32_HOST_OS
foreign import stdcall "kernel32.dll GetModuleHandleA"
  getModuleHandleA :: LPCSTR -> IO HINSTANCE
#endif

#ifdef mingw32_HOST_OS
foreign import stdcall "user32.dll RegisterClassExA"
  registerClassExA :: Ptr WNDCLASSEXA -> IO ATOM
#endif

#ifdef mingw32_HOST_OS
foreign import stdcall "user32.dll CreateWindowExA"
  cCreateWindowExA :: DWORD -> LPCSTR -> LPCSTR -> DWORD -> CInt -> CInt -> CInt -> CInt -> HWND -> HMENU -> HINSTANCE -> LPVOID -> IO HWND
#endif

#ifdef mingw32_HOST_OS
foreign import stdcall "user32.dll ShowWindow"
  showWindow :: HWND -> CInt -> IO BOOL
#endif

#ifdef mingw32_HOST_OS
foreign import stdcall "user32.dll UpdateWindow"
  updateWindow :: HWND -> IO BOOL
#endif

#ifdef mingw32_HOST_OS
foreign import stdcall "user32.dll DefWindowProcA"
  c_DefWindowProcA :: HWND -> UINT -> WPARAM -> LPARAM -> IO LRESULT
#endif

#ifdef mingw32_HOST_OS
foreign import stdcall "user32.dll SetWindowTextA"
  c_SetWindowTextA :: HWND -> LPCSTR -> IO BOOL
#endif

#ifdef mingw32_HOST_OS
foreign import stdcall "user32.dll PostQuitMessage"
  postQuitMessage :: CInt -> IO ()
#endif

#ifdef mingw32_HOST_OS
foreign import stdcall "user32.dll LoadCursorA"
  loadCursorA :: HINSTANCE -> LPCSTR -> IO HCURSOR
#endif

#ifdef mingw32_HOST_OS
foreign import stdcall "kernel32.dll GetLastError"
  getLastError :: IO DWORD
#endif

-- Window Class Structure (WNDCLASSEXA)
-- Ensure fields are in the correct order and have correct types/sizes for Storable.
data WNDCLASSEXA = WNDCLASSEXA
  { wcexCbSize        :: UINT      -- Size of this structure, in bytes.
  , wcexStyle         :: UINT      -- Class styles.
  , wcexLpfnWndProc   :: FunPtr (HWND -> UINT -> WPARAM -> LPARAM -> IO LRESULT) -- Pointer to the window procedure.
  , wcexCbClsExtra    :: CInt      -- Amount of extra data allocated for this class structure.
  , wcexCbWndExtra    :: CInt      -- Amount of extra data allocated for each window of this class.
  , wcexHInstance     :: HINSTANCE -- Handle to the instance that contains the window procedure for the class.
  , wcexHIcon         :: HICON     -- Handle to the class icon.
  , wcexHCursor       :: HCURSOR   -- Handle to the class cursor.
  , wcexHbrBackground :: HBRUSH    -- Handle to the class background brush.
  , wcexLpszMenuName  :: LPCSTR    -- Pointer to a null-terminated string that specifies the resource name of the class menu.
  , wcexLpszClassName :: LPCSTR    -- Pointer to a null-terminated string or is an atom.
  , wcexHIconSm       :: HICON     -- Handle to a small icon that is associated with the window class.
  }

instance Storable WNDCLASSEXA where
  -- Size for x64. UINT=4, FunPtr=8, CInt=4, HINSTANCE/HICON/HCURSOR/HBRUSH/LPCSTR=8
  -- Total size: 4+4+8+4+4+8+8+8+8+8+8+8 = 80 bytes
  sizeOf _ = 4 + 4 + 8 + 4 + 4 + 8 + 8 + 8 + 8 + 8 + 8 + 8 -- = 80 bytes
  alignment _ = 8 -- Max alignment is pointer alignment on x64
  peek ptr = do
    cbSizeValue        <- peekByteOff ptr 0   :: IO UINT
    styleValue         <- peekByteOff ptr 4   :: IO UINT
    lpfnWndProcValue   <- peekByteOff ptr 8   :: IO (FunPtr (HWND -> UINT -> WPARAM -> LPARAM -> IO LRESULT))
    cbClsExtraValue    <- peekByteOff ptr 16  :: IO CInt
    cbWndExtraValue    <- peekByteOff ptr 20  :: IO CInt
    hInstanceValue     <- peekByteOff ptr 24  :: IO HINSTANCE
    hIconValue         <- peekByteOff ptr 32  :: IO HICON
    hCursorValue       <- peekByteOff ptr 40  :: IO HCURSOR
    hbrBackgroundValue <- peekByteOff ptr 48  :: IO HBRUSH
    lpszMenuNameValue  <- peekByteOff ptr 56  :: IO LPCSTR
    lpszClassNameValue <- peekByteOff ptr 64  :: IO LPCSTR
    hIconSmValue       <- peekByteOff ptr 72  :: IO HICON
    return $ WNDCLASSEXA
      { wcexCbSize        = cbSizeValue
      , wcexStyle         = styleValue
      , wcexLpfnWndProc   = lpfnWndProcValue
      , wcexCbClsExtra    = cbClsExtraValue
      , wcexCbWndExtra    = cbWndExtraValue
      , wcexHInstance     = hInstanceValue
      , wcexHIcon         = hIconValue
      , wcexHCursor       = hCursorValue
      , wcexHbrBackground = hbrBackgroundValue
      , wcexLpszMenuName  = lpszMenuNameValue
      , wcexLpszClassName = lpszClassNameValue
      , wcexHIconSm       = hIconSmValue
      }
  poke ptr WNDCLASSEXA{..} = do
    pokeByteOff ptr 0  wcexCbSize
    pokeByteOff ptr 4  wcexStyle
    pokeByteOff ptr 8  wcexLpfnWndProc
    pokeByteOff ptr 16 wcexCbClsExtra
    pokeByteOff ptr 20 wcexCbWndExtra
    pokeByteOff ptr 24 wcexHInstance
    pokeByteOff ptr 32 wcexHIcon
    pokeByteOff ptr 40 wcexHCursor
    pokeByteOff ptr 48 wcexHbrBackground
    pokeByteOff ptr 56 wcexLpszMenuName
    pokeByteOff ptr 64 wcexLpszClassName
    pokeByteOff ptr 72 wcexHIconSm

-- FFI import for DestroyWindow
#ifdef mingw32_HOST_OS
foreign import stdcall "user32.dll DestroyWindow"
  destroyWindow :: HWND -> IO BOOL
#endif

-- Helper to convert LPARAM to HWND (for button handles in WM_COMMAND)
lParamToHwnd :: LPARAM -> HWND
lParamToHwnd = intPtrToPtr . fromIntegral -- LPARAM is LONG_PTR, HWND is Ptr ()

-- Context to be stored in window's user data
data WindowContext = WindowContext
  { eventQueue :: IORef [Event]
  , minSizeRef :: IORef (Maybe Size) -- Stores minimum window size
  , maxSizeRef :: IORef (Maybe Size) -- Stores maximum window size
  }

-- Constants for GetWindowLongPtrA/SetWindowLongPtrA
gwlpUserdata :: CInt
gwlpUserdata = -21

-- FFI Imports for Get/SetWindowLongPtrA
#ifdef mingw32_HOST_OS
foreign import stdcall "user32.dll GetWindowLongPtrA"
  getWindowLongPtrA :: HWND -> CInt -> IO LONG_PTR
#endif

#ifdef mingw32_HOST_OS
foreign import stdcall "user32.dll SetWindowLongPtrA"
  setWindowLongPtrA :: HWND -> CInt -> LONG_PTR -> IO LONG_PTR
#endif

-- Our Window Procedure (must be exported to be callable from C)
windowProc :: HWND -> UINT -> WPARAM -> LPARAM -> IO LRESULT
windowProc hwnd uMsg wParam lParam = do
  -- putStrLn $ "WindowProc Message: " ++ show uMsg ++ " wParam: " ++ show wParam ++ " lParam: " ++ show lParam
  case uMsg of
    _ | uMsg == wmCommand -> do
        let notificationCode = hiword wParam
        let buttonHwnd = lParamToHwnd lParam -- HWND of the control

        if notificationCode == bnClicked && buttonHwnd /= nullPtr then do -- Check if it's a button click from one of our controls
            let controlId = loword wParam -- This is the ID we assigned
            maybeCtxPtr <- getWindowLongPtrA hwnd gwlpUserdata
            if maybeCtxPtr /= 0 then do -- Check if pointer is not null
                let stablePtr = castPtrToStablePtr (intPtrToPtr (fromIntegral maybeCtxPtr)) :: StablePtr WindowContext
                actualCtx <- deRefStablePtr stablePtr -- Dereference to get WindowContext
                let newEvent = ButtonClickEvent (fromIntegral controlId :: Int) -- Use the actual control ID
                atomicModifyIORef' (eventQueue actualCtx) (\es -> (newEvent : es, ()))
                -- putStrLn "Button 1001 clicked! Event queued."
            else
                -- putStrLn "Error: No context pointer in windowProc for button click!"
                return ()
            return 0 -- Message processed
        else
            c_DefWindowProcA hwnd uMsg wParam lParam -- Pass other commands to default

    _ | uMsg == wmDestroy -> do
        -- putStrLn "WM_DESTROY received."
        maybeCtxPtr <- getWindowLongPtrA hwnd gwlpUserdata
        if maybeCtxPtr /= 0 then do
            let stablePtr = castPtrToStablePtr (intPtrToPtr (fromIntegral maybeCtxPtr)) :: StablePtr WindowContext
            -- It's good practice to ensure you're freeing the correct StablePtr.
            -- For instance, you might want to deRefStablePtr and check something if needed,
            -- but direct free is common if you trust the stored pointer.
            freeStablePtr stablePtr
            -- putStrLn "Freed StablePtr from GWLP_USERDATA."
        else
            -- putStrLn "WM_DESTROY: No StablePtr found in GWLP_USERDATA to free."
            return ()
        postQuitMessage 0
        return 0

    _ | uMsg == wmClose -> do
        -- putStrLn "WM_CLOSE received, attempting to destroy window."
        _ <- destroyWindow hwnd
        return 0

    _ | uMsg == wmGetMinMaxInfo -> do
        -- putStrLn "WM_GETMINMAXINFO received."
        maybeCtxPtr <- getWindowLongPtrA hwnd gwlpUserdata
        if maybeCtxPtr /= 0 then do
            let stablePtr = castPtrToStablePtr (intPtrToPtr (fromIntegral maybeCtxPtr)) :: StablePtr WindowContext
            actualCtx <- deRefStablePtr stablePtr
            maybeMinSize <- readIORef (minSizeRef actualCtx)
            maybeMaxSize <- readIORef (maxSizeRef actualCtx)

            -- Only proceed if there's at least a min or max size to set
            if isJust maybeMinSize || isJust maybeMaxSize then do
                let pMinMaxInfo = intPtrToPtr (fromIntegral lParam) :: Ptr MINMAXINFO
                minMaxInfo <- peek pMinMaxInfo
                let updatedMinMaxInfo1 = case maybeMinSize of
                                            Just (minW, minH) -> 
                                                let newMinTrackSize = POINT_STRUCT { ptx = fromIntegral minW, pty = fromIntegral minH }
                                                in minMaxInfo { ptMinTrackSize = newMinTrackSize }
                                            Nothing -> minMaxInfo
                
                let updatedMinMaxInfo2 = case maybeMaxSize of
                                            Just (maxW, maxH) -> 
                                                let newMaxTrackSize = POINT_STRUCT { ptx = fromIntegral maxW, pty = fromIntegral maxH }
                                                in updatedMinMaxInfo1 { ptMaxTrackSize = newMaxTrackSize }
                                            Nothing -> updatedMinMaxInfo1

                poke pMinMaxInfo updatedMinMaxInfo2
                -- putStrLn $ "WM_GETMINMAXINFO: Min: " ++ show maybeMinSize ++ ", Max: " ++ show maybeMaxSize
                return 0 -- Indicate message processed
            else
                c_DefWindowProcA hwnd uMsg wParam lParam -- No min or max size set, default behavior
        else
            c_DefWindowProcA hwnd uMsg wParam lParam -- No context, default behavior

    _ -> c_DefWindowProcA hwnd uMsg wParam lParam

foreign export ccall "ghaskui_windowProc"
  windowProc_exported :: HWND -> UINT -> WPARAM -> LPARAM -> IO LRESULT
windowProc_exported :: HWND -> UINT -> WPARAM -> LPARAM -> IO LRESULT -- Explicit signature
windowProc_exported = windowProc -- Keep export separate from main definition for clarity if needed

-- Get a FunPtr to our exported Haskell windowProc.
-- The GHC user guide explains that `&symbol_name` can be used for exported symbols.
-- We need to refer to the exported C symbol name, which is "ghaskui_windowProc".
foreign import ccall "&ghaskui_windowProc"
  windowProc_ptr :: FunPtr (HWND -> UINT -> WPARAM -> LPARAM -> IO LRESULT)

-- | Creates a basic native Windows window.
createBasicWindow :: IORef [Event] -- ^ Event queue
                  -> String    -- ^ Class name for the window.
                  -> String    -- ^ Title for the window.
                  -> Point     -- ^ Position (x, y)
                  -> Size      -- ^ Size (width, height)
                  -> Bool      -- ^ Is resizable?
                  -> Bool      -- ^ Has maximize button?
                  -> IO (Maybe WindowHandle)   -- ^ Returns the handle to the created window.
createBasicWindow eventQueueRef classNameStr titleStr (x, y) (w, h) isResizable hasMaximizeBtn = withCString classNameStr $ \cClassName ->
  withCString titleStr $ \cTitle -> do
    hInst <- getModuleHandleA nullPtr -- Get current application instance handle
    when (hInst == nullPtr) $ ioError (userError "GetModuleHandleA failed.")

    hCurs <- loadCursorA nullPtr idcArrowLPCSTR
    when (hCurs == nullPtr) $ ioError (userError "LoadCursorA for IDC_ARROW failed.")

    let wc = WNDCLASSEXA
          { wcexCbSize        = fromIntegral (sizeOf (undefined :: WNDCLASSEXA))
          , wcexStyle         = 0 -- CS_HREDRAW | CS_VREDRAW would be 3. For now, 0.
          , wcexLpfnWndProc   = windowProc_ptr
          , wcexCbClsExtra    = 0
          , wcexCbWndExtra    = fromIntegral (sizeOf (undefined :: StablePtr ())) -- Reserve space for StablePtr
          , wcexHInstance     = hInst
          , wcexHIcon         = nullPtr -- No application icon for now
          , wcexHCursor       = hCurs
          , wcexHbrBackground = intPtrToPtr (fromIntegral (colorWindow + 1) :: IntPtr) -- Standard window background
          , wcexLpszMenuName  = nullPtr -- No menu
          , wcexLpszClassName = cClassName
          , wcexHIconSm       = nullPtr -- No small icon for now
          }

    atom <- alloca $ \pwc -> do -- Use alloca for the WNDCLASSEXA structure
        poke pwc wc
        registerClassExA pwc
    when (atom == 0) $ ioError (userError "RegisterClassExA failed.")

    let baseStyle = ws_OVERLAPPED .|. ws_CAPTION .|. ws_SYSMENU .|. ws_MINIMIZEBOX
    let styleWithResize = if isResizable then baseStyle .|. ws_THICKFRAME else baseStyle
    let currentDwStyle = if hasMaximizeBtn then styleWithResize .|. ws_MAXIMIZEBOX else styleWithResize

    hwnd <- cCreateWindowExA
              0                    -- dwExStyle: Optional window styles (e.g., WS_EX_CLIENTEDGE)
              cClassName           -- lpClassName: Our registered class name.
              cTitle               -- lpWindowName: The window title.
              currentDwStyle       -- dwStyle: Window style.
              (fromIntegral x)     -- X: Horizontal position of the window.
              (fromIntegral y)     -- Y: Vertical position of the window.
              (fromIntegral w)     -- nWidth: Window width.
              (fromIntegral h)     -- nHeight: Window height.
              nullPtr              -- hWndParent: Parent window handle (nullPtr for top-level).
              nullPtr              -- hMenu: Menu handle (nullPtr if class menu is used or no menu).
              hInst                -- hInstance: Instance handle.
              nullPtr              -- lpParam: Additional application data (passed to WM_CREATE).
    if hwnd == nullPtr
    then do
        errCode <- getLastError
        let errorMsg = "CreateWindowExA failed with error code: " ++ show errCode
        putStrLn errorMsg -- Print to console for immediate feedback
        return Nothing
    else do
        -- Create context and store StablePtr in window user data
        minRef <- newIORef Nothing -- Initialize minSizeRef
        maxRef <- newIORef Nothing -- Initialize maxSizeRef
        let winCtx = WindowContext { eventQueue = eventQueueRef, minSizeRef = minRef, maxSizeRef = maxRef }
        stableCtxPtr <- newStablePtr winCtx
        -- Store the StablePtr as a LONG_PTR
        -- Note: ptrToIntPtr converts Ptr to IntPtr (which is CIntPtr / LONG_PTR)
        -- fromIntegral is used because setWindowLongPtrA expects LONG_PTR (CIntPtr)
        -- castStablePtrToPtr converts StablePtr a to Ptr a
        void $ setWindowLongPtrA hwnd gwlpUserdata (fromIntegral (ptrToIntPtr (castStablePtrToPtr stableCtxPtr)))
        
        _ <- showWindow hwnd swShowNormal
        _ <- updateWindow hwnd
        return (Just (WindowHandle hwnd)) -- Wrap HWND in WindowHandle
-- FFI Imports for Message Loop
-- foreign import stdcall "user32.dll GetMessageA"
--  getMessageA :: Ptr MSG -> HWND -> UINT -> UINT -> IO CInt

#ifdef mingw32_HOST_OS
foreign import stdcall "user32.dll PeekMessageA"
  peekMessageA :: Ptr MSG -> HWND -> UINT -> UINT -> UINT -> IO BOOL
#endif

-- foreign import stdcall "user32.dll WaitMessage"
-- foreign import ccall "user32.dll WaitMessage"
--  waitMessage :: IO BOOL

-- PeekMessage constants
-- pm_NoRemove :: UINT
-- pm_NoRemove = 0x0000

pm_Remove :: UINT
pm_Remove = 0x0001

#ifdef mingw32_HOST_OS
foreign import stdcall "user32.dll TranslateMessage"
  translateMessage :: Ptr MSG -> IO BOOL
#endif

#ifdef mingw32_HOST_OS
foreign import stdcall "user32.dll DispatchMessageA"
  dispatchMessageA :: Ptr MSG -> IO LRESULT
#endif

-- MSG Structure
-- typedef struct tagMSG {
--   HWND   hwnd;
--   UINT   message;
--   WPARAM wParam;
--   LPARAM lParam;
--   DWORD  time;
--   POINT  pt; -- POINT is {LONG x; LONG y;}
-- } MSG, *PMSG, *LPMSG;

-- For POINT structure
type LONG = CLong -- from Foreign.C.Types

data POINT = POINT { x :: LONG, y :: LONG } deriving Show

instance Storable POINT where
  sizeOf _ = sizeOf (undefined :: LONG) * 2 -- 8 bytes on x64 (CLong is 4 bytes, but often padded or part of larger struct)
  alignment _ = alignment (undefined :: LONG)
  peek ptr = POINT <$> peekByteOff ptr 0 <*> peekByteOff ptr (sizeOf (undefined::LONG))
  poke ptr (POINT x_ y_) = pokeByteOff ptr 0 x_ >> pokeByteOff ptr (sizeOf (undefined::LONG)) y_

data MSG = MSG
  { msgHwnd    :: HWND
  , msgMessage :: UINT
  , msgWParam  :: WPARAM
  , msgLParam  :: LPARAM
  , msgTime    :: DWORD
  , msgPt      :: POINT
  } deriving Show

instance Storable MSG where
  -- HWND=8, UINT=4, WPARAM=8, LPARAM=8, DWORD=4, POINT=8 (on x64, assuming CLong is 4, POINT is 8)
  -- Total size: 8+4+8+8+4+8 = 40
  sizeOf _ = sizeOf (undefined :: HWND) +
             sizeOf (undefined :: UINT) +
             sizeOf (undefined :: WPARAM) +
             sizeOf (undefined :: LPARAM) +
             sizeOf (undefined :: DWORD) +
             sizeOf (undefined :: POINT)
  alignment _ = 8 -- Pointer alignment
  peek ptr = MSG <$> peekByteOff ptr 0                        -- hwnd
                 <*> peekByteOff ptr (sizeOf (undefined::HWND)) -- message
                 <*> peekByteOff ptr (sizeOf (undefined::HWND) + sizeOf (undefined::UINT)) -- wParam
                 <*> peekByteOff ptr (sizeOf (undefined::HWND) + sizeOf (undefined::UINT) + sizeOf (undefined::WPARAM)) -- lParam
                 <*> peekByteOff ptr (sizeOf (undefined::HWND) + sizeOf (undefined::UINT) + sizeOf (undefined::WPARAM) + sizeOf (undefined::LPARAM)) -- time
                 <*> peekByteOff ptr (sizeOf (undefined::HWND) + sizeOf (undefined::UINT) + sizeOf (undefined::WPARAM) + sizeOf (undefined::LPARAM) + sizeOf (undefined::DWORD)) -- pt
  poke ptr MSG{..} = do
    let offHwnd    = 0
    let offMessage = offHwnd    + sizeOf msgHwnd
    let offWParam  = offMessage + sizeOf msgMessage
    let offLParam  = offWParam  + sizeOf msgWParam
    let offTime    = offLParam  + sizeOf msgLParam
    let offPt      = offTime    + sizeOf msgTime
    pokeByteOff ptr offHwnd    msgHwnd
    pokeByteOff ptr offMessage msgMessage
    pokeByteOff ptr offWParam  msgWParam
    pokeByteOff ptr offLParam  msgLParam
    pokeByteOff ptr offTime    msgTime
    pokeByteOff ptr offPt      msgPt

-- | Runs the standard Windows message loop.
-- | The HWND argument to GetMessageA is a filter; nullPtr means messages for any window of the current thread.
-- Represents the Windows platform context (can be expanded later if needed)
data WindowsPlatform = WindowsPlatform

instance UICreator WindowsPlatform where
    coreCreateBasicWindow WindowsPlatform eventQueue className title pos size isResizable hasMaximizeBtn = 
        createBasicWindow eventQueue className title pos size isResizable hasMaximizeBtn

    coreRunMessageLoop p@(WindowsPlatform {}) hwnd eventQueue eventHandler = 
        runMessageLoop p hwnd eventQueue eventHandler

    coreSetWindowMinSize _ (WindowHandle hwnd) newMinSize = do
        maybeCtxPtr <- getWindowLongPtrA hwnd gwlpUserdata
        if maybeCtxPtr /= 0 then do
            let stablePtr = castPtrToStablePtr (intPtrToPtr (fromIntegral maybeCtxPtr)) :: StablePtr WindowContext
            actualCtx <- deRefStablePtr stablePtr
            writeIORef (minSizeRef actualCtx) (Just newMinSize)
        else
            putStrLn "Error: coreSetWindowMinSize could not retrieve window context."

    coreSetWindowMaxSize _ (WindowHandle hwnd) newMaxSize = do
        maybeCtxPtr <- getWindowLongPtrA hwnd gwlpUserdata
        if maybeCtxPtr /= 0 then do
            let stablePtr = castPtrToStablePtr (intPtrToPtr (fromIntegral maybeCtxPtr)) :: StablePtr WindowContext
            actualCtx <- deRefStablePtr stablePtr
            writeIORef (maxSizeRef actualCtx) (Just newMaxSize)
        else
            putStrLn "Error: coreSetWindowMaxSize could not retrieve window context."

    coreCreateButton WindowsPlatform (WindowHandle parentHwnd) buttonId label (x, y) (w, h) = do
        hInst <- getModuleHandleA nullPtr
        when (hInst == nullPtr) $ ioError (userError "GetModuleHandleA failed in coreCreateButton.")

        withCString "BUTTON" $ \cClassName ->
          withCString label $ \cLabel -> do
            let dwStyle = wsChild .|. wsVisible .|. bsPushButton
                xPos    = fromIntegral x :: CInt
                yPos    = fromIntegral y :: CInt
                width   = fromIntegral w :: CInt
                height  = fromIntegral h :: CInt
                controlID = intPtrToPtr (fromIntegral buttonId) :: HMENU

            buttonHwnd <- cCreateWindowExA
                              0             -- dwExStyle
                              cClassName    -- lpClassName
                              cLabel        -- lpWindowName
                              dwStyle       -- dwStyle
                              xPos          -- X
                              yPos          -- Y
                              width         -- nWidth
                              height        -- nHeight
                              parentHwnd    -- hWndParent
                              controlID     -- hMenu (Control ID)
                              hInst         -- hInstance
                              nullPtr       -- lpParam

            if buttonHwnd == nullPtr then
                return Nothing
            else
                return (Just (ButtonHandle buttonHwnd))

    coreCreateLabel WindowsPlatform (WindowHandle parentHwnd) label (x, y) (w, h) = do
        hInst <- getModuleHandleA nullPtr
        when (hInst == nullPtr) $ ioError (userError "GetModuleHandleA failed in coreCreateLabel.")

        withCString "STATIC" $ \cClassName ->
          withCString label $ \cLabel -> do
            let dwStyle = wsChild .|. wsVisible .|. ss_RIGHT -- Or ss_LEFT, ss_CENTER
                xPos    = fromIntegral x :: CInt
                yPos    = fromIntegral y :: CInt
                width   = fromIntegral w :: CInt
                height  = fromIntegral h :: CInt

            labelHwnd <- cCreateWindowExA
                              0             -- dwExStyle
                              cClassName    -- lpClassName
                              cLabel        -- lpWindowName
                              dwStyle       -- dwStyle
                              xPos          -- X
                              yPos          -- Y
                              width         -- nWidth
                              height        -- nHeight
                              parentHwnd    -- hWndParent
                              nullPtr       -- hMenu (Static controls don't usually need a menu/ID for creation)
                              hInst         -- hInstance
                              nullPtr       -- lpParam

            if labelHwnd == nullPtr then
                return Nothing
            else
                return (Just (LabelHandle labelHwnd))

    coreSetLabelText WindowsPlatform (LabelHandle labelHwnd) text = do
        withCString text $ \cText ->
            void $ c_SetWindowTextA labelHwnd cText

runMessageLoop :: WindowsPlatform -> WindowHandle -> IORef [Event] -> EventHandler -> IO ()
runMessageLoop platform (WindowHandle _hwnd) eventQueueRef eventHandler = alloca $ \pMsg -> do
  -- Use an IORef to control the loop continuation.
  -- This allows WM_QUIT (or other conditions) to signal the loop to stop.
  continueLoopRef <- newIORef True

  let loop = do
        continue <- readIORef continueLoopRef
        when continue $ do
          -- Process Windows messages
          msgAvailable <- peekMessageA pMsg nullPtr 0 0 pm_Remove
          if msgAvailable /= 0 -- Check CInt boolean (non-zero is true)
            then do
              msg <- peek pMsg -- pMsg is valid here
              when (msgMessage msg == wmQuit) $ do
                writeIORef continueLoopRef False -- Signal loop to stop

              _ <- translateMessage pMsg
              _ <- dispatchMessageA pMsg
              loop -- Process next Windows message immediately
            else do
              -- No Windows messages, process Haskell events
              events <- atomicModifyIORef' eventQueueRef (\es -> ([], reverse es)) -- Get and clear queue
              unless (null events) $ do
                mapM_ (\event -> void $ unGHaskUIApp (eventHandler event) (SomePlatform platform)) events -- Call the provided handler for each event
              -- For now, simple busy wait if no WM_QUIT. 
              -- TODO: Could use WaitMessage here if events is empty and loop should continue.
              loop -- Continue loop

  loop -- Start the loop
  -- Exiting message loop.

-- Make sure wmQuit is defined if not already
-- wmQuit :: UINT
-- wmQuit = 0x0012

-- GHaskUI.Platform.Windows: Windows-specific implementation (Win32 API via FFI).
