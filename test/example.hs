{-# LANGUAGE OverloadedStrings #-}
import Graphics.Aosd.Pango 

markup = pSized 50 (pUnlines [pItalic "AOSD","Example"])

main = aosdFlash defaultOpts (textRenderer markup) { alignment = Just AlignCenter, colour = orange } (symDurations 3000 3000)

