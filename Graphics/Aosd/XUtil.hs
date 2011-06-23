{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}
module Graphics.Aosd.XUtil where
import Graphics.X11.Xlib.Display
import Control.Exception
import Graphics.X11.Xlib

data ScreenSize = ScreenSize { screenWidth, screenHeight :: !Int }
    deriving(Show)


getScreenSize :: Display -> IO ScreenSize
getScreenSize display = do
                screen <- evaluate $ defaultScreen display
                screenWidth <- evaluate . fromIntegral $ displayWidth display screen
                screenHeight <- evaluate . fromIntegral $ displayHeight display screen

                return ScreenSize{..}


