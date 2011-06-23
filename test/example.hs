{-# LANGUAGE OverloadedStrings #-}
import Graphics.Aosd.Pango 

markup = pSized 50 (pUnlines [pItalic "AOSD","Example"])

main = 
    withAosd 
        defaultOpts 
        (textRenderer markup) { alignment = Just AlignCenter, colour = orange } 
        (\a -> aosdFlash a (symDurations 3000 3000))

