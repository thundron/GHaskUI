{-# LANGUAGE CPP #-}
module GHaskUI (
    -- Application Monad & Runner
    GHaskUIApp, -- Abstract type, constructor not exported
    runGHaskUIApp,
    askPlatform,   -- Re-export helper
    liftGHaskUI,   -- Re-export helper

    -- Platform-Oblivious UI Operations
    createBasicWindow,
    createButton,
    createLabel,
    setLabelText,
    runMessageLoop,
    setWindowMinimumSize, -- Added to set minimum window size
    setWindowMaximumSize, -- Added to set maximum window size

    -- Core types
    WindowHandle,
    ButtonHandle,
    LabelHandle, -- Added LabelHandle
    Event(..),
    EventHandler, -- Added EventHandler

    -- UI Creation / Platform
    UICreator(..), -- Exports coreCreateBasicWindow, coreRunMessageLoop, coreCreateLabel, coreCreateButton, coreSetLabelText as methods
    SomePlatform(..)

    -- Platform specific types/functions (if any directly exposed beyond instances)
    -- Concrete platform types like WindowsPlatform are no longer exported directly.
    -- Use getDefaultPlatform to obtain a platform instance.
    ) where

import GHaskUI.Platform.Windows (WindowsPlatform(..)) -- Needed for platform selection
-- Linux placeholder, uncomment and implement when ready
-- import GHaskUI.Platform.Linux (LinuxPlatform(..))
import GHaskUI.Core (
    -- Monad and internals for wrappers
    GHaskUIApp(..), unGHaskUIApp, askPlatform, liftGHaskUI, -- Import new helpers
    UICreator(..), SomePlatform(..), -- UICreator and SomePlatform are needed for the wrappers
    -- Core types to re-export
    WindowHandle, ButtonHandle, LabelHandle, Event(..), EventHandler, Point, Size
    )
-- import GHaskUI.Platform.Linux (LinuxPlatform(..)) -- For PlatformSource IO instance when ready

import Data.IORef (IORef)

-- This is the top-level module for the GHaskUI framework.
-- It re-exports functionalities from Core and Platform-specific modules.


-- | Runs a GHaskUI application.
-- This is the main entry point for applications using the GHaskUI library.
-- It sets up the platform context and executes the application logic.
runGHaskUIApp :: GHaskUIApp a -> IO a
runGHaskUIApp appAction = do
    let platformInstance =
#ifdef mingw32_HOST_OS
            SomePlatform WindowsPlatform
#elif defined(linux_HOST_OS)
            -- When LinuxPlatform is implemented and provides a UICreator instance:
            -- SomePlatform LinuxPlatform -- Placeholder
            error "runGHaskUIApp: Linux platform not yet fully supported in this GHaskUI setup."
#else
            error "runGHaskUIApp: Platform not supported."
#endif
    unGHaskUIApp appAction platformInstance -- Directly run the function with the platform

-- Platform-Oblivious API Wrappers

-- | Creates a basic window.
createBasicWindow :: IORef [Event] -> String -> String -> Point -> Size -> Bool -> Bool -> GHaskUIApp (Maybe WindowHandle)
createBasicWindow eventQueue title windowName pos dimensions isResizable hasMaximizeBtn = do
    SomePlatform p <- askPlatform
    liftGHaskUI $ coreCreateBasicWindow p eventQueue title windowName pos dimensions isResizable hasMaximizeBtn

-- | Creates a button.
createButton :: WindowHandle -> Int -> String -> Point -> Size -> GHaskUIApp (Maybe ButtonHandle)
createButton winHandle btnId label pos dimensions = do
    SomePlatform p <- askPlatform
    liftGHaskUI $ coreCreateButton p winHandle btnId label pos dimensions

-- | Creates a label.
createLabel :: WindowHandle -> String -> Point -> Size -> GHaskUIApp (Maybe LabelHandle)
createLabel winHandle text pos dimensions = do
    SomePlatform p <- askPlatform
    liftGHaskUI $ coreCreateLabel p winHandle text pos dimensions

-- | Sets the text of a label.
setLabelText :: LabelHandle -> String -> GHaskUIApp ()
setLabelText lblHandle text = do
    SomePlatform p <- askPlatform
    liftGHaskUI $ coreSetLabelText p lblHandle text

-- | Sets the minimum resizable size for the given window.
setWindowMinimumSize :: WindowHandle -> Size -> GHaskUIApp ()
setWindowMinimumSize winHandle newMinSize = do
    SomePlatform p <- askPlatform
    liftGHaskUI $ coreSetWindowMinSize p winHandle newMinSize

-- | Sets the maximum resizable size for the given window.
setWindowMaximumSize :: WindowHandle -> Size -> GHaskUIApp ()
setWindowMaximumSize winHandle newMaxSize = do
    SomePlatform p <- askPlatform
    liftGHaskUI $ coreSetWindowMaxSize p winHandle newMaxSize

-- | Runs the main message loop for a window.
-- The event handler will be executed within the GHaskUIApp monad.
runMessageLoop :: WindowHandle -> IORef [Event] -> EventHandler -> GHaskUIApp ()
runMessageLoop winHandle eventQueue eventHandler = do
    SomePlatform p <- askPlatform
    -- The coreRunMessageLoop itself is IO. The eventHandler it calls
    -- produces GHaskUIApp actions. The platform-specific implementation of
    -- coreRunMessageLoop will need to run these using unGHaskUIApp and the platform instance.
    liftGHaskUI $ coreRunMessageLoop p winHandle eventQueue eventHandler
