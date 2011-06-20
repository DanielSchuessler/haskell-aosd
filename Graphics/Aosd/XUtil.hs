{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}
module Graphics.Aosd.XUtil where
import Graphics.X11.Xlib.Display
import Control.Exception

data ScreenSize = ScreenSize { screenWidth, screenHeight :: Int }
    deriving(Show)


getScreenSize :: IO ScreenSize
getScreenSize = do
        display <- openDisplay ""

        let go = do
                -- Work around unsafe FFI declarations in the X11 bindings...
                screen <- evaluate $ defaultScreen display
                screenWidth <- evaluate . fromIntegral $ displayWidth display screen
                screenHeight <- evaluate . fromIntegral $ displayHeight display screen

                return ScreenSize{..}

        go `finally` closeDisplay display

