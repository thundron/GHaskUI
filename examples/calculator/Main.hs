{-# LANGUAGE GADTs #-}
module Main where

import GHaskUI ( GHaskUIApp, runGHaskUIApp, liftGHaskUI, LabelHandle, Event(..), createBasicWindow, createButton, createLabel, setLabelText, runMessageLoop, setWindowMinimumSize, setWindowMaximumSize )
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad (unless) -- Added unless based on usage in equals/clear
import System.Exit (exitSuccess, exitFailure)
import Text.Read (readMaybe) -- For safe parsing of numbers

handleCalculatorEvent :: IORef (Maybe LabelHandle) 
                -> IORef String -- For current input
                -> IORef (Maybe String) -- For stored value
                -> IORef (Maybe Int)    -- For pending operation
                -> Event -> GHaskUIApp ()
handleCalculatorEvent displayHandleRef currentInputRef storedValueRef pendingOpRef (ButtonClickEvent buttonId) = do
    if buttonId >= 0 && buttonId <= 9 then do -- Handle numeric buttons 0-9
        let digit = show buttonId
        currentVal <- liftGHaskUI $ readIORef currentInputRef
        let newVal = if currentVal == "0" && digit /= "0" then digit 
                     else if currentVal == "0" && digit == "0" then "0" -- Avoid multiple leading zeros
                     else currentVal ++ digit
        liftGHaskUI $ writeIORef currentInputRef newVal
        
        -- Update display
        maybeLabel <- liftGHaskUI $ readIORef displayHandleRef
        case maybeLabel of
            Nothing -> liftGHaskUI $ putStrLn "Error: Display handle not found for numeric input update."
            Just actualLabelHandle -> setLabelText actualLabelHandle newVal

    else if buttonId >= 10 && buttonId <= 13 then do -- Handle operator buttons + (10), - (11), * (12), / (13)
        currentVal <- liftGHaskUI $ readIORef currentInputRef
        -- Only store if currentVal is not empty or just "0" after an op, to prevent issues with repeated ops
        -- This logic might need refinement based on desired calculator behavior (e.g. 5 * = 25)
        -- For now, a new op overwrites previous state if no new number was entered.
        liftGHaskUI $ writeIORef storedValueRef (Just currentVal) -- Store current input as first operand
        liftGHaskUI $ writeIORef pendingOpRef (Just buttonId)     -- Store operator
        liftGHaskUI $ writeIORef currentInputRef "0"              -- Reset current input for next number
        
        maybeLabel <- liftGHaskUI $ readIORef displayHandleRef
        case maybeLabel of
            Nothing -> liftGHaskUI $ putStrLn "Error: Display handle not found for operator action update."
            Just actualLabelHandle -> setLabelText actualLabelHandle "0"

    else if buttonId == 14 then do -- Handle Equals button (ID 14)
        val2Str <- liftGHaskUI $ readIORef currentInputRef
        maybeVal1Str <- liftGHaskUI $ readIORef storedValueRef
        maybeOpId <- liftGHaskUI $ readIORef pendingOpRef

        case (maybeVal1Str, maybeOpId, readMaybe val2Str :: Maybe Double) of
            (Just val1Str, Just opId, Just val2) ->
                case readMaybe val1Str :: Maybe Double of
                    Just val1 -> do
                        liftGHaskUI $ putStrLn $ "DEBUG: val1Str = " ++ val1Str
                        liftGHaskUI $ putStrLn $ "DEBUG: val2Str = " ++ val2Str
                        liftGHaskUI $ putStrLn $ "DEBUG: val1 (parsed) = " ++ show val1
                        liftGHaskUI $ putStrLn $ "DEBUG: val2 (parsed) = " ++ show val2
                        let result = case opId of
                                10 -> val1 + val2
                                11 -> val1 - val2
                                12 -> val1 * val2
                                13 -> if val2 == 0 then read "NaN" else val1 / val2 -- Handle division by zero
                                _  -> read "NaN" -- Should not happen with current ops
                        liftGHaskUI $ putStrLn $ "DEBUG: result (calc) = " ++ show result
                        
                        let resultStr = if isNaN result || isInfinite result then "Error" else show result
                        liftGHaskUI $ writeIORef currentInputRef resultStr
                        liftGHaskUI $ writeIORef storedValueRef Nothing -- Clear stored value after calculation
                        liftGHaskUI $ writeIORef pendingOpRef Nothing   -- Clear pending operation

                        maybeLabel <- liftGHaskUI $ readIORef displayHandleRef
                        case maybeLabel of
                            Nothing -> liftGHaskUI $ putStrLn "Error: Display handle not found for equals update."
                            Just actualLabelHandle -> setLabelText actualLabelHandle resultStr
                    Nothing -> do 
                        liftGHaskUI $ putStrLn "Error: Failed to parse stored value."
                        maybeLabel <- liftGHaskUI $ readIORef displayHandleRef
                        case maybeLabel of
                            Just actualLabel -> setLabelText actualLabel "Error"
                            Nothing -> liftGHaskUI $ putStrLn "Critical Error: Display handle not found when trying to show parse error."
            _ -> do
                -- Display current input if no operation is pending
                unless (maybeOpId == Nothing && maybeVal1Str == Nothing) $ do
                    maybeLabel <- liftGHaskUI $ readIORef displayHandleRef
                    case maybeLabel of
                        Nothing -> return ()
                        Just actualLabelHandle -> setLabelText actualLabelHandle val2Str

    else if buttonId == 15 then do -- Handle Clear button (ID 15)
        liftGHaskUI $ writeIORef currentInputRef "0"
        liftGHaskUI $ writeIORef storedValueRef Nothing
        liftGHaskUI $ writeIORef pendingOpRef Nothing
        
        maybeLabel <- liftGHaskUI $ readIORef displayHandleRef
        case maybeLabel of
            Nothing -> liftGHaskUI $ putStrLn "Error: Display handle not found for clear action."
            Just actualLabelHandle -> setLabelText actualLabelHandle "0"

    else
        liftGHaskUI $ putStrLn $ "Button ID " ++ show buttonId ++ " not handled."
handleCalculatorEvent _ _ _ _ (OtherEvent detail) = 
    liftGHaskUI $ putStrLn $ "Other event: " ++ detail
handleCalculatorEvent _ _ _ _ event = do -- Catch-all for other event types, if any
    liftGHaskUI $ putStrLn $ "Unhandled event: " ++ show event

main :: IO ()
main = runGHaskUIApp $ do
    -- IORefs are created in IO, so we use liftIO
    displayHandleRef <- liftGHaskUI $ newIORef Nothing
    currentInputRef  <- liftGHaskUI $ newIORef "0" -- Initialize display with 0
    storedValueRef   <- liftGHaskUI $ newIORef Nothing
    pendingOpRef     <- liftGHaskUI $ newIORef Nothing
    eventQueueRef    <- liftGHaskUI $ newIORef [] -- Create the event queue

    -- Layout parameters for dynamic sizing
    let numButtonCols = 4        -- Total button columns (e.g., 7,8,9, /)
    let numDisplayCols = 3       -- Columns for the display label (e.g., 7,8,9 part)
    let numButtonRows = 4
    let buttonWidth = 80
    let buttonHeight = 60
    let uiPadding = 10           -- Padding around content and between elements
    let displayLabelHeight = 50

    -- Calculate dimensions of UI elements
    let actualDisplayLabelWidth = (numDisplayCols * buttonWidth) + ((numDisplayCols - 1) * uiPadding)
    let buttonGridWidth = (numButtonCols * buttonWidth) + ((numButtonCols - 1) * uiPadding)
    let buttonGridHeight = (numButtonRows * buttonHeight) + ((numButtonRows - 1) * uiPadding)

    -- Calculate minimum client area needed
    let minClientAreaWidth = buttonGridWidth -- Display is narrower and fits above this
    let minClientAreaHeight = displayLabelHeight + uiPadding + buttonGridHeight

    -- Estimate window frame (title bar, borders) dimensions
    let estimatedFrameHeight = 40      -- For title bar and bottom border (reduced from 50)
    let estimatedSideFrameWidth = 16   -- For left and right borders (e.g., 8px each side)

    -- Calculate initial outer window dimensions
    let initialWindowWidth = uiPadding + minClientAreaWidth + uiPadding + estimatedSideFrameWidth
    let initialWindowHeight = uiPadding + minClientAreaHeight + uiPadding + estimatedFrameHeight

    -- Create the main window
    mHwnd <- createBasicWindow eventQueueRef "GHaskUICalc" "Calculator" (100, 100) (initialWindowWidth, initialWindowHeight) False False
    case mHwnd of
        Nothing -> liftGHaskUI $ do
            putStrLn "Failed to create window."
            exitFailure
        Just hwnd -> do
            -- Create and place the display label
            mDisplayLabel <- createLabel hwnd "0" (uiPadding, uiPadding) (actualDisplayLabelWidth, displayLabelHeight)
            case mDisplayLabel of
                Nothing -> liftGHaskUI $ putStrLn "Failed to create display label."
                Just actualDisplayHandle -> do
                    liftGHaskUI $ writeIORef displayHandleRef (Just actualDisplayHandle)
                    -- Set the minimum window size (using the same outer dimensions as initial size)
                    setWindowMinimumSize hwnd (initialWindowWidth, initialWindowHeight)
                    -- Set the maximum window size to be the same as the minimum (initial) size
                    -- This effectively makes the window non-resizable via WM_GETMINMAXINFO
                    setWindowMaximumSize hwnd (initialWindowWidth, initialWindowHeight)

            -- Button layout parameters (buttonWidth, buttonHeight, uiPadding already defined)
            let startX = uiPadding
            let startY = uiPadding + displayLabelHeight + uiPadding -- Below the display label

            -- Define buttons (text, ID, position)
            let buttons = [
                  ("7", 7, (startX, startY)), ("8", 8, (startX + buttonWidth + uiPadding, startY)), ("9", 9, (startX + 2*(buttonWidth + uiPadding), startY)), ("/", 13, (startX + 3*(buttonWidth + uiPadding), startY)),
                  ("4", 4, (startX, startY + buttonHeight + uiPadding)), ("5", 5, (startX + buttonWidth + uiPadding, startY + buttonHeight + uiPadding)), ("6", 6, (startX + 2*(buttonWidth + uiPadding), startY + buttonHeight + uiPadding)), ("*", 12, (startX + 3*(buttonWidth + uiPadding), startY + buttonHeight + uiPadding)),
                  ("1", 1, (startX, startY + 2*(buttonHeight + uiPadding))), ("2", 2, (startX + buttonWidth + uiPadding, startY + 2*(buttonHeight + uiPadding))), ("3", 3, (startX + 2*(buttonWidth + uiPadding), startY + 2*(buttonHeight + uiPadding))), ("-", 11, (startX + 3*(buttonWidth + uiPadding), startY + 2*(buttonHeight + uiPadding))),
                  ("0", 0, (startX, startY + 3*(buttonHeight + uiPadding))), ("C", 15, (startX + buttonWidth + uiPadding, startY + 3*(buttonHeight + uiPadding))), ("=", 14, (startX + 2*(buttonWidth + uiPadding), startY + 3*(buttonHeight + uiPadding))), ("+", 10, (startX + 3*(buttonWidth + uiPadding), startY + 3*(buttonHeight + uiPadding)))
                  ]

            -- Create buttons
            mapM_ (\(txt, bid, pos) -> createButton hwnd bid txt pos (buttonWidth, buttonHeight)) buttons

            -- Run the message loop
            runMessageLoop hwnd eventQueueRef (handleCalculatorEvent displayHandleRef currentInputRef storedValueRef pendingOpRef)
            liftGHaskUI exitSuccess -- Should not be reached if runMessageLoop blocks indefinitely
