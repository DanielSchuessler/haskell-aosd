{-# LANGUAGE ExistentialQuantification, NamedFieldPuns, RecordWildCards #-}
{-# OPTIONS -Wall #-}
-- | For a higher-level API for textual OSDs using /Pango/, use "Graphics.Aosd.Pango".
module Graphics.Aosd(
    -- * Renderers
    AosdRenderer(..), GeneralRenderer(..), 
--     -- ** Simple combinators
--     HCatRenderer(..), VCatRenderer(..),
    -- * Options
    AosdOptions(..), Transparency(..), Position(..), XClassHint(..), defaultOpts,
    -- * Displaying
    aosdFlash,
    FlashDurations(..), symDurations,
    -- ** Low-level operations
    AosdPtr(..),aosdNew,reconfigure,
    aosdRender, aosdShow, aosdHide, aosdLoopOnce, aosdLoopFor,

    -- * Reexports
    module Graphics.Rendering.Cairo,
    Rectangle(..),
    CInt, CUInt

    
    
    ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Foreign
import Foreign.C
import Graphics.Aosd.AOSD_H
import Graphics.Aosd.Util
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Internal(runRender)
import Data.Functor
import Graphics.Rendering.Pango.Enums
import Graphics.X11.Xlib(openDisplay,closeDisplay,displayHeight,displayWidth,defaultScreen)

class AosdRenderer a where
    toGeneralRenderer :: a -> IO GeneralRenderer


data XClassHint = XClassHint { resName, resClass :: String }
    deriving(Show)

data Transparency = None | Fake | Composite
    deriving(Show)

data Position = Min -- ^ Left/top
              | Center 
              | Max -- ^ Right/bottom
    deriving(Show,Enum,Bounded)

data AosdOptions = AosdOptions {
    -- | 'Nothing' = use /libaosd/ default.
    classHint :: Maybe XClassHint,
    -- | 'Nothing' = use /libaosd/ default.
    transparency :: Maybe Transparency,
    xPos :: Position,
    yPos :: Position,
    -- | Positive values denote a rightwards respectively downwards offset (in pixels).
    offset :: (CInt,CInt),
    -- | 'Nothing' = use /libaosd/ default.
    hideUponMouseEvent :: Maybe Bool,
    -- | Mouse-click event handler.
    mouseEventCB :: Maybe (C'AosdMouseEvent -> IO ())
}

data GeneralRenderer = GeneralRenderer {
    grRender :: Render (),
    -- | Part of the surface that the renderer actually draws on (determines the window size).
    grInkExtent :: Rectangle,
    -- | Part of the surface whose...
    --
    -- * ... left edge is aligned to the left edge of the screen, if 'xPos' is 'Min'
    --
    -- * ... center is aligned to the center of the screen, if 'xPos' is 'Center'
    --
    -- * ... right edge is aligned to the right edge of the screen, if 'xPos' is 'Max'
    --
    -- (Likewise for the /y/ axis)
    grPositioningExtent :: Rectangle
}


-- -- | Horizontal concatenation
-- data HCatRenderer = forall r1 r2. (AosdRenderer r1, AosdRenderer r2) => HCat r1 r2
-- 
-- 
-- -- | Vertical concatenation
-- data VCatRenderer = forall r1 r2. (AosdRenderer r1, AosdRenderer r2) => VCat r1 r2


instance AosdRenderer GeneralRenderer where
    toGeneralRenderer = return

-- instance AosdRenderer HCatRenderer where
--     toGeneralRenderer (HCat r1 r2) = do
--         gr1 <- toGeneralRenderer r1
--         gr2 <- toGeneralRenderer r2
--         return GeneralRenderer {
--                     grWidth = ((+) `on` grWidth) gr1 gr2,
--                     grHeight = (max `on` grHeight) gr1 gr2,
--                     grRender = do
--                         grRender gr1
--                         translate (fromIntegral $ grWidth gr1) 0
--                         grRender gr2
--                }
-- 
-- instance AosdRenderer VCatRenderer where
--     toGeneralRenderer (VCat r1 r2) = do
--         gr1 <- toGeneralRenderer r1
--         gr2 <- toGeneralRenderer r2
--         return GeneralRenderer {
--                     grWidth = (max `on` grWidth) gr1 gr2,
--                     grHeight = ((+) `on` grHeight) gr1 gr2,
--                     grRender = do
--                         grRender gr1
--                         translate 0 (fromIntegral $ grHeight gr1)
--                         grRender gr2
--                }

toC'AosdRenderer :: Render () -> IO C'AosdRenderer
toC'AosdRenderer r = mk'AosdRenderer f
    where
        f cairo _ = runReaderT (runRender r) cairo


toAosdTransparency :: Transparency -> C'AosdTransparency
toAosdTransparency None = c'TRANSPARENCY_NONE
toAosdTransparency Fake = c'TRANSPARENCY_FAKE
toAosdTransparency Composite = c'TRANSPARENCY_COMPOSITE


-- toAosdCoordinate :: Position -> C'AosdCoordinate
-- toAosdCoordinate Min = c'COORDINATE_MINIMUM
-- toAosdCoordinate Center = c'COORDINATE_CENTER
-- toAosdCoordinate Max = c'COORDINATE_MAXIMUM


withAosd :: (Ptr C'Aosd -> IO a) -> IO a
withAosd k = do
    a <- c'aosd_new 
    finally (k a) (c'aosd_destroy a)



withConfiguredAosd :: AosdRenderer a => AosdOptions -> a -> (Ptr C'Aosd -> IO r) -> IO r
withConfiguredAosd opts x k = 
    withAosd (\a -> do
        reconfigure0 opts x a 
        k a
    )


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

reconfigure0 :: AosdRenderer a => AosdOptions -> a -> Ptr C'Aosd -> IO ()
reconfigure0 AosdOptions{..} renderer a = do
        GeneralRenderer{..} <- toGeneralRenderer renderer 
        maybeDo (setClassHint a) classHint
        
        maybeDo (c'aosd_set_transparency a . toAosdTransparency) transparency

        ScreenSize{..} <- getScreenSize


        let -- l=Left, t=Top, w=Width, h=Height 
            Rectangle li ti wi hi = grInkExtent
            Rectangle lp tp wp hp = grPositioningExtent

{- 
(These comments only look at the x dimension; the other is analogous) 

Consider the mapping "screenx" from grRender x coordinates to screen x coordinates

Since we translate grRender by -(li,ti), we have:

        screenx x = windowLeft + x - li 

If xPos is Min, we want: 

        screenx lp = 0
        <=>
        windowLeft + lp - li = 0
        <=>
        windowLeft = li - lp

If xPos is Center, we want:

        screenx (lp + wp/2) = screenWidth/2
        <=>
        windowLeft + (lp + wp/2) - li = screenWidth/2
        <=>
        windowLeft = li - lp + (screenWidth - wp)/2 

If xPos is Max, we want:

        screenx (lp+wp) = screenWidth
        <=>
        windowLeft + (lp+wp) - li = screenWidth
        <=>
        windowLeft = li - lp + screenWidth - wp

-}
            calculateOffsetAdjustment pos min_ink min_positioning size_positioning size_screen  = fromIntegral $ 
                        case pos of
                              Min -> min_ink - min_positioning
                              Center -> min_ink - min_positioning + div (size_screen - size_positioning) 2
                              Max -> min_ink - min_positioning + size_screen - size_positioning 

                      

            windowLeft = calculateOffsetAdjustment xPos li lp wp screenWidth  + fst offset
            windowTop  = calculateOffsetAdjustment yPos ti tp hp screenHeight + snd offset

            windowWidth = fromIntegral wi
            windowHeight = fromIntegral hi

            finalRenderer = do
                translate (fi . negate $ li) (fi . negate $ ti)
                grRender



        c'aosd_set_geometry a windowLeft windowTop windowWidth windowHeight
            

        rendererPtr <- toC'AosdRenderer finalRenderer
        c'aosd_set_renderer a rendererPtr nullPtr 

        maybeDo (setHideUponMouseEvent a) hideUponMouseEvent

        maybeDo (setMouseEventCB a) mouseEventCB

setClassHint :: Ptr C'Aosd -> XClassHint -> IO ()
setClassHint a XClassHint{ resName, resClass } = 
    withCString resName (\resName' ->
        withCString resClass (\resClass' ->
            c'aosd_set_names a resName' resClass'))

setHideUponMouseEvent :: Ptr C'Aosd -> Bool -> IO ()
setHideUponMouseEvent a b = c'aosd_set_hide_upon_mouse_event a (if b then 1 else 0)

setMouseEventCB :: Ptr C'Aosd -> (C'AosdMouseEvent -> IO ()) -> IO ()
setMouseEventCB a f = do
    fptr <- mk'AosdMouseEventCb f'
    c'aosd_set_mouse_event_cb a fptr nullPtr
  where
    f' :: Ptr C'AosdMouseEvent -> Ptr () -> IO () 
    f' p _ = do 
        mouseEvent <- peek p 
        f mouseEvent

-- | Non-'Nothing' defaults:
--
-- *       transparency = Just Composite,
--
-- *       xPos = Center, 
--
-- *       yPos = Center,
-- 
-- *       offset = (0,0),
--
-- *       hideUponMouseEvent = Just True
defaultOpts :: AosdOptions
defaultOpts =
    AosdOptions { 
        classHint = Nothing,
        transparency = Just Composite,
        xPos = Center, 
        yPos = Center,
        offset = (0,0),
        hideUponMouseEvent = Just True,
        mouseEventCB = Nothing
    }

data FlashDurations = FlashDurations {
    inMillis :: CUInt -- ^ Fade-in time in milliseconds 
  , fullMillis :: CUInt -- ^ Full display time in milliseconds
  , outMillis :: CUInt -- ^ Fade-out time in milliseconds
}
    deriving(Show)

-- | Construct a 'FlashDurations' with equal 'inMillis' and 'outMillis'.
symDurations :: 
        CUInt -- ^ 'inMillis' and 'outMillis'. 
     -> CUInt -- ^ 'fullMillis'.
     -> FlashDurations
symDurations fadeMillis fullMillis = FlashDurations fadeMillis fullMillis fadeMillis

-- | Main high-level displayer. Blocks. 
aosdFlash :: AosdRenderer a => AosdOptions -> a -> FlashDurations -> IO ()
aosdFlash opts renderer FlashDurations{..} = 
    withConfiguredAosd opts renderer (\p -> c'aosd_flash p inMillis fullMillis outMillis)

                               



newtype AosdPtr = AosdPtr { unAosdPtr :: ForeignPtr C'Aosd }

-- | Create an /Aosd/ object managed by the garbage collector.
aosdNew :: AosdRenderer a => AosdOptions -> a -> IO AosdPtr
aosdNew opts r = do
    p <- c'aosd_new
    reconfigure0 opts r p
    AosdPtr <$> Foreign.newForeignPtr p'aosd_destroy p


reconfigure :: AosdRenderer a => AosdOptions -> a -> AosdPtr -> IO ()
reconfigure opts r = (`withForeignPtr` reconfigure0 opts r) . unAosdPtr

aosdRender :: AosdPtr -> IO ()
aosdRender = (`withForeignPtr` c'aosd_render) . unAosdPtr

aosdShow :: AosdPtr -> IO ()
aosdShow = (`withForeignPtr` c'aosd_show) . unAosdPtr

aosdHide :: AosdPtr -> IO ()
aosdHide = (`withForeignPtr` c'aosd_hide) . unAosdPtr

aosdLoopOnce :: AosdPtr -> IO ()
aosdLoopOnce = (`withForeignPtr` c'aosd_loop_once) . unAosdPtr

aosdLoopFor :: AosdPtr 
            -> CUInt -- ^ Time in milliseconds.
            -> IO ()
aosdLoopFor (AosdPtr fp) millis = (fp `withForeignPtr` (`c'aosd_loop_for` millis)) 

