{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- MonadReader removed
module GHaskUI.Core (
    -- Application Monad & Context
    GHaskUIApp(..),
    unGHaskUIApp,
    askPlatform,   -- Export new helper
    liftGHaskUI,   -- Export new helper

    -- Types
    WindowHandle(..), -- Export the constructor too
    ButtonHandle(..),
    LabelHandle(..), -- Added LabelHandle
    Event(..),
    Point,
    Size,
    EventHandler, -- Added EventHandler type

    -- UI Creation Abstraction
    UICreator(..),
    SomePlatform(..), -- Export the existential type wrapper

    -- Platform Acquisition (Removed PlatformSource and getDefaultPlatform)
    ) where

import Foreign.Ptr (Ptr)
import Data.IORef (IORef) -- Added for IORef in type signatures

-- | Type class for abstracting UI element creation across platforms.
-- The 'platform' type variable represents any platform-specific context
-- or handle that might be needed for widget creation.
class UICreator platform where
    coreCreateButton :: platform -> WindowHandle -> Int -> String -> Point -> Size -> IO (Maybe ButtonHandle)
    coreCreateLabel  :: platform -> WindowHandle -> String -> Point -> Size -> IO (Maybe LabelHandle)
    coreSetLabelText :: platform -> LabelHandle -> String -> IO ()
    coreCreateBasicWindow :: platform -> IORef [Event] -> String -> String -> Point -> Size -> Bool -> Bool -> IO (Maybe WindowHandle) -- Added isResizable, hasMaximizeButton
    coreRunMessageLoop    :: platform -> WindowHandle -> IORef [Event] -> EventHandler -> IO ()
    coreSetWindowMinSize  :: platform -> WindowHandle -> Size -> IO ()
    coreSetWindowMaxSize  :: platform -> WindowHandle -> Size -> IO () -- Added for setting maximum window size
    -- Other widget creation functions like coreCreateTextBox etc. can be added here.

-- | An opaque handle to a native window.
-- The actual underlying type is platform-specific (e.g., HWND on Windows).
-- Using Ptr () for now as a generic, platform-agnostic pointer.
newtype WindowHandle = WindowHandle (Ptr ())
    deriving (Show, Eq)

-- | An opaque handle to a native button.
newtype ButtonHandle = ButtonHandle (Ptr ())
    deriving (Show, Eq)

-- | An opaque handle to a native label/static text control.
newtype LabelHandle = LabelHandle (Ptr ()) 
    deriving (Show, Eq)

-- | Represents a 2D point (x, y).
type Point = (Int, Int)

-- | Represents a 2D size (width, height).
type Size = (Int, Int)

-- | Represents a GUI event.
data Event
    = ButtonClickEvent Int  -- Carries the button ID
    | WindowCloseEvent      -- User requested window close
    | OtherEvent String     -- For unhandled or generic events
    deriving (Show, Eq)

-- | Defines the signature for an event handling function.

-- | Existential type wrapper for any platform that implements a given typeclass `c` (e.g., UICreator).
-- This allows us to have a heterogeneous list or a single value of "some platform"
-- without knowing its concrete type at compile time, as long as it satisfies the constraint.
data SomePlatform c where
    SomePlatform :: c p => p -> SomePlatform c

-- | The GHaskUI application monad. Carries the platform context implicitly.
newtype GHaskUIApp a = GHaskUIApp { unGHaskUIAppInternal :: SomePlatform UICreator -> IO a }

instance Functor GHaskUIApp where
    fmap f (GHaskUIApp g) = GHaskUIApp $ \platform -> fmap f (g platform)

instance Applicative GHaskUIApp where
    pure x = GHaskUIApp $ \_ -> pure x
    (GHaskUIApp f) <*> (GHaskUIApp x) = GHaskUIApp $ \platform -> (f platform) <*> (x platform)

instance Monad GHaskUIApp where
    return = pure -- Same as pure for Applicative
    (GHaskUIApp m) >>= k = GHaskUIApp $ \platform -> do
        a <- m platform
        let (GHaskUIApp m') = k a -- Get the function from the newtype
        m' platform

-- | Retrieves the platform context.
askPlatform :: GHaskUIApp (SomePlatform UICreator)
askPlatform = GHaskUIApp $ \platform -> pure platform

-- | Lifts an IO action into the GHaskUIApp monad.
liftGHaskUI :: IO a -> GHaskUIApp a
liftGHaskUI ioAction = GHaskUIApp $ \_ -> ioAction

-- | Helper to extract the underlying function from GHaskUIApp.
-- Useful for platform implementations when running event handlers.
unGHaskUIApp :: GHaskUIApp a -> (SomePlatform UICreator -> IO a)
unGHaskUIApp = unGHaskUIAppInternal

type EventHandler = Event -> GHaskUIApp () -- Event handlers now run in GHaskUIApp

-- TODO: Define related types like Point, Size, MouseButton, KeyCode, Modifiers if not already present
-- The Event type is now defined earlier in the file (around line 67).

-- GHaskUI.Core: Platform-agnostic core logic for the GUI framework.
