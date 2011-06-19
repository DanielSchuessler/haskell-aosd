{-# LANGUAGE NoMonomorphismRestriction, StandaloneDeriving, OverloadedStrings, NamedFieldPuns, TemplateHaskell, RecordWildCards #-}
import Graphics.Aosd.Pango
import Control.Monad
import Control.Concurrent
import Data.Monoid
import Language.Haskell.TH
import Language.Haskell.Extract
import Data.Functor
import Graphics.Rendering.Pango.Enums

deriving instance Show LayoutAlignment
deriving instance Show LayoutWrapMode

main = mapM_ run ( $(functionExtractor "Test$") :: [(String,IO ())] )
    where
        run :: (String,IO()) -> IO ()
        run (n,a) = do
            putStrLn ("Running "++n)
            header <- aosdNew (defaultOpts { yPos = Min, offset = (0,100) }) (textRenderer (pText n))
            aosdShow header
            aosdRender header
            aosdLoopOnce header
            a
            aosdHide header
            aosdLoopOnce header

go t r = aosdFlash defaultOpts r (symDurations 100 t) 
goText t f pm = go t (f (textRenderer pm))

tagsMarkup = pUnlines $(listE [ appE (varE (mkName n)) (litE (stringL n)) | n <- words "pBold pBig pItalic pStrikethrough pSmall pSub pSup pUnderline pMono"]) 

tagsTest = goText 7000 (\x -> x { colour=green }) tagsMarkup 


-- catTest = do
--     go 3000 (HCat (textRenderer "Horizontal") (textRenderer "Cat"))
--     go 3000 (VCat (textRenderer "Vertical") (textRenderer "Cat"))


alignmentsTest = do
    sequence_ [ goText 2000 (\x -> x { width = Just width, 
                                       wrapMode = wrap, 
                                       alignment = Just al, 
                                       colour = magenta }) 

                    (pSized 14 $ pUnlines [ pText firstLine 
                              , pShow width
                              , pShow wrap
                              , pShow al]) 

              | al <- [AlignLeft, AlignRight, AlignCenter], 
                firstLine <- flip replicate 'o' <$> [5,80],
                (width,wrap) <- [(Unlimited,Nothing) ,
                                 (100,Just WrapWholeWords) ,
                                 (100,Just WrapAnywhere)
                                 ] 
              
              
              ]


leftOverflowTest =
    sequence_ [ do
                 print ("xPos",xPos)
                 aosdFlash 
                    (defaultOpts {xPos}) 
                    (textRenderer txt) 
                        { alignment = Just AlignRight, width = Just 300, wrapMode = Just WrapWholeWords } 
                    (symDurations 100 3000)
               | xPos <- [Center,Min,Max] ]

 where
    txt0 = "LeftOverflow"
    txt = pSized 24 (pBold "LeftEnd" `mappend` 
                     pText (concat (replicate 4 txt0)) `mappend`
                     pBold "RightEnd"
                    )


sizesTest = do
    goText 2000 id $ mconcat [ pSpan [FontSize s] (pText (show s)) | s <- [ SizeSmall, SizeMedium, SizeLarge, SizeHuge, SizeGiant, SizePoint 12 ] ]

singleParagraphModeTest = do
    goText 1000 (\x -> x { singleParagraphMode = Just True }) (pText . unlines . words $ "single paragraph mode")
     
dur = 5000

posTest = forEachPos f
    where
        f xPos yPos = do
                    let tr = (textRenderer (pText $ show (xPos,yPos))) { colour = red } 
                    aosdFlash defaultOpts { xPos, yPos } tr (symDurations 500 dur)  

forEachPos f = do

    sequence [ forkOS $ f xPos yPos 

              | (xPos,yPos) <- join (liftM2 (,)) [minBound..maxBound]
             ]

    threadDelay ((fromIntegral dur+1000)*1000)

    
    --mapM f strings

--f s = flashText defaultTextConf s (symDurations 200 200)

prStatus = liftIO . print =<< status


circleTest = forEachPos doCircle

doCircle xPos yPos = aosdFlash defaultOpts { xPos, yPos } GeneralRenderer{..} (symDurations 100 2000)
    where
        grInkExtent = Rectangle (-r) (-r) (2*r) (2*r)
        grPositioningExtent = Rectangle (-r_half) (-r_half) r r 

        r_half = 50

        r = 2*r_half

        lw = 10

        grRender = do
            setSourceRGBA 0 1 1 1
            setLineWidth lw
            arc 0 0 (r-lw) 0 (2*pi)
            stroke
            setSourceRGBA 1 0 1 1
            setLineWidth 2
            arc 0 0 (r_half-2) 0 (2*pi)
            stroke
            prStatus

transparencyTest = goText 3000 (\s -> s { colour = lime, opacity = 0.5 }) 
                    (pSized 100 "Transparency")
