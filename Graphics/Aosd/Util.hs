{-# LANGUAGE CPP, ViewPatterns, NoMonomorphismRestriction #-}
module Graphics.Aosd.Util where
import Graphics.Rendering.Pango.Enums
import Control.Arrow
import Data.Colour.SRGB
import Graphics.Rendering.Cairo
import Foreign.StablePtr
import System.IO
import Control.Monad
import Foreign.Concurrent
import Foreign.Ptr
import Foreign.ForeignPtr(ForeignPtr)
import Data.Functor

maybeDo :: Monad m => (a -> m ()) -> Maybe a -> m ()
maybeDo f = maybe (return ()) f

traverseMaybe :: Monad m => (t -> m a) -> Maybe t -> m (Maybe a)
traverseMaybe f Nothing = return Nothing
traverseMaybe f (Just x) = Just `liftM` f x

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

debugMemory :: Bool
putDebugMemory :: String -> String -> IO ()
#ifdef DEBUG_MEMORY
debugMemory = True
putDebugMemory cxt msg = putStdErr (cxt ++ ": "++replicate (30 - length cxt) ' ' ++ msg)
#else
debugMemory = False
putDebugMemory _ _ = return ()
#endif

showStablePtr ::  StablePtr a -> String
showStablePtr = show . castStablePtrToPtr

putStdErr :: String -> IO ()
putStdErr = hPutStrLn stderr


newStablePtrDebug :: String -> String -> a -> IO (StablePtr a)
newStablePtrDebug cxt descr a = do
    sp <- newStablePtr a
    putDebugMemory cxt ("Created "++descr++" StablePtr: "++showStablePtr sp) 
    return sp

freeStablePtrDebug :: String -> String -> StablePtr a -> IO ()
freeStablePtrDebug cxt descr sp = do
    putDebugMemory cxt ("Freeing "++descr++" StablePtr: "++showStablePtr sp) 
    freeStablePtr sp

newForeignPtrDebug :: String -> String -> IO () -> Ptr a -> IO (ForeignPtr a)
newForeignPtrDebug cxt descr finalizer p = do
    fp <- newForeignPtr p 
                (do
                    putDebugMemory "ForeignPtr finalizer" ("Finalizing "++descr++" ForeignPtr made in "++cxt)   
                    finalizer)
    putDebugMemory cxt ("Created "++descr++" ForeignPtr: "++show fp)
    return fp


