{-# LANGUAGE ViewPatterns, NoMonomorphismRestriction #-}
module Graphics.Aosd.Util where
import Graphics.Rendering.Pango.Enums
import Control.Arrow
import Data.Colour.SRGB
import Graphics.Rendering.Cairo

maybeDo :: Monad m => (a -> m ()) -> Maybe a -> m ()
maybeDo f = maybe (return ()) f

(^+^) ::  (Num t) => (t, t) -> (t, t) -> (t, t)
(x,y) ^+^ (x',y') = (x+x',y+y')

negate2 :: (Num t) => (t, t) -> (t, t)
negate2 (x,y) = (-x,-y)

(^-^) ::  (Num t) => (t, t) -> (t, t) -> (t, t)
v ^-^ w = v ^+^ negate2 w

fi :: (Num b, Integral a) => a -> b
fi = fromIntegral

rectLeft :: Rectangle -> Int
rectLeft (Rectangle a _ _ _) = a 
rectTop :: Rectangle -> Int
rectTop (Rectangle _ a _ _) = a 
rectWidth :: Rectangle -> Int
rectWidth (Rectangle _ _ a _) = a 
rectHeight :: Rectangle -> Int
rectHeight (Rectangle _ _ _ a) = a 

rectRight :: Rectangle -> Int
rectRight r = rectLeft r + rectWidth r
rectBottom :: Rectangle -> Int
rectBottom r = rectTop r + rectHeight r

rectLeftTop :: Rectangle -> (Int, Int)
rectLeftTop = rectLeft &&& rectTop

rectSize :: Rectangle -> (Int, Int)
rectSize = rectWidth &&& rectHeight

max2 :: (Ord t, Ord t1) => (t, t) -> (t1, t1) -> (t, t1)
max2 (x,y) (x',y') = (max x y, max x' y')

min2 :: (Ord t, Ord t1) => (t, t) -> (t1, t1) -> (t, t1)
min2 (x,y) (x',y') = (min x y, min x' y')

rectDiff :: Rectangle -> Rectangle -> Rectangle
rectDiff (Rectangle a b c d) (Rectangle a' b' c' d') = Rectangle (a-a') (b-b') (c-c') (d-d')

scale2 :: Num t => t -> (t, t) -> (t, t)
scale2 s (x,y) = (s*x,s*y)

fi2 :: (Integral a, Num b) => (a,a) -> (b,b)
fi2 = fi *** fi

rectCenterX :: Rectangle -> Rational
rectCenterX r = fi (2 * rectRight r + rectWidth r) / 2
rectCenterY :: Rectangle -> Rational
rectCenterY r = fi (2 * rectTop r + rectHeight r) / 2

setSourceColour :: Colour Double -> Double -> Render ()
setSourceColour (toSRGB -> RGB r g b) a = setSourceRGBA r g b a

