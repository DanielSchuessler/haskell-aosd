{-# LANGUAGE FlexibleContexts, Rank2Types, KindSignatures, NoMonomorphismRestriction, ExistentialQuantification, NamedFieldPuns, RecordWildCards #-}
{-# OPTIONS -Wall #-}
-- | For a higher-level API for textual OSDs using /Pango/, use "Graphics.Aosd.Pango".
module Graphics.Aosd(
    -- * Renderers
    AosdRenderer(..), GeneralRenderer(..),
--     -- ** Simple combinators
--     HCatRenderer(..), VCatRenderer(..),
    -- * Options
    AosdOptions(..), Transparency(..), Position(..), XClassHint(..), defaultOpts,
    -- * Construction/destruction
    AosdPtr,aosdNew,aosdDestroy,withAosd,
    -- * Displaying
    aosdFlash,
    FlashDurations(..), symDurations,
    -- ** Low-level operations
    reconfigure,
    aosdRender, aosdShow, aosdHide, aosdLoopOnce, aosdLoopFor,

    -- * Diagnostics
    debugRenderer,

    -- * Reexports
    module Graphics.Rendering.Cairo,
    Rectangle(..),
    CInt, CUInt

    ) where

import Control.Concurrent.MVar
import Control.Monad.Trans.Reader
import Foreign.C
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Graphics.Aosd.AOSD_H
import Graphics.Aosd.Options
import Graphics.Aosd.Renderer
import Graphics.Aosd.Util
import Graphics.Aosd.CallbackUtil
import Graphics.Aosd.XUtil
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Internal(runRender,Cairo)
import Graphics.Rendering.Pango.Enums
import System.IO.Unsafe
import Graphics.X11.Xlib(Display,openDisplay,closeDisplay)
import Control.Exception
import Data.Maybe






-- toAosdCoordinate :: Position -> C'AosdCoordinate
-- toAosdCoordinate Min = c'COORDINATE_MINIMUM
-- toAosdCoordinate Center = c'COORDINATE_CENTER
-- toAosdCoordinate Max = c'COORDINATE_MAXIMUM


c'aosd_new_debug :: String -> IO (Ptr C'Aosd)
c'aosd_new_debug cxt = do
    p <- c'aosd_new
    putDebugMemory cxt ("c'aosd_new ==> "++show p)
    return p

c'aosd_destroy_debug :: String -> Ptr C'Aosd -> IO ()
c'aosd_destroy_debug cxt p = do
    putDebugMemory cxt ("c'aosd_destroy "++show p)
    c'aosd_destroy p

type MouseEventHandler = AosdPtr -> C'AosdMouseEvent -> IO ()



{- Position calculation comments:

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


-- | Must *NOT* access the aosdStructOwnedDataVar of the AosdPtr argument (-> deadlock).
reconfigure0 :: (AosdRenderer renderer) => AosdOptions -> renderer -> AosdPtr -> IO AosdStructOwnedData
reconfigure0 AosdOptions{..} renderer aosd@AosdPtr {unAosdPtr=ptr, display} =
    do
        GeneralRenderer{..} <- toGeneralRenderer renderer

        ScreenSize{..} <- getScreenSize display


        let -- l=Left, t=Top, w=Width, h=Height
            Rectangle li ti wi hi = grInkExtent
            Rectangle lp tp wp hp = grPositioningExtent

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

        maybeDo (setClassHint ptr) classHint
        maybeDo (setHideUponMouseEvent ptr) hideUponMouseEvent

        rendererPtr <- setRenderer ptr finalRenderer
        handlerPtr <- traverseMaybe (setMouseEventCB aosd) mouseEventCB



        maybeDo (c'aosd_set_transparency ptr . toAosdTransparency) transparency
        c'aosd_set_geometry ptr windowLeft windowTop windowWidth windowHeight

        return (AosdStructOwnedData rendererPtr handlerPtr)

-- | Does *NOT* free the old handler
setRenderer :: Ptr C'Aosd -> Render () -> IO (StablePtr (Cairo -> IO ()))
setRenderer ptr renderer = tunnelCallback theC'AosdRenderer (c'aosd_set_renderer ptr) f
    where
        f = runReaderT . runRender $ renderer

{-# NOINLINE theC'AosdRenderer #-}
theC'AosdRenderer :: UniversalCallback Cairo
theC'AosdRenderer = unsafePerformIO $ mkUniversalCallback mk'AosdRenderer

{-# NOINLINE theC'AosdMouseEventCb #-}
theC'AosdMouseEventCb :: UniversalCallback (Ptr C'AosdMouseEvent)
theC'AosdMouseEventCb = unsafePerformIO $ mkUniversalCallback mk'AosdMouseEventCb





setClassHint :: Ptr C'Aosd -> XClassHint -> IO ()
setClassHint a XClassHint{ resName, resClass } =
    withCString resName (\resName' ->
        withCString resClass (\resClass' ->
            c'aosd_set_names a resName' resClass'))

setHideUponMouseEvent :: Ptr C'Aosd -> Bool -> IO ()
setHideUponMouseEvent a b = c'aosd_set_hide_upon_mouse_event a (if b then 1 else 0)

-- | Does *NOT* free the old handler.
-- Must *NOT* access the aosdStructOwnedDataVar of the AosdPtr argument (-> deadlock).
setMouseEventCB :: AosdPtr -> MouseEventHandler -> IO (StablePtr (Ptr C'AosdMouseEvent -> IO ()))
setMouseEventCB aosd@AosdPtr {unAosdPtr=ptr} handler = tunnelCallback theC'AosdMouseEventCb (c'aosd_set_mouse_event_cb ptr) f
    where
        f eventp = do
            event <- peek eventp
            handler aosd event


-- | Main high-level displayer. Blocks.
aosdFlash :: AosdPtr -> FlashDurations -> IO ()
aosdFlash a FlashDurations{..} = wrapAosd (\p -> c'aosd_flash p inMillis fullMillis outMillis) a

data AosdPtr = AosdPtr               { unAosdPtr :: !(Ptr C'Aosd)
                                        -- We only keep this around for deallocating
                                     , aosdStructOwnedDataVar :: !(MVar (Maybe AosdStructOwnedData))
                                     , display :: Display
                                     }


aosdNew0 :: IO AosdPtr
aosdNew0 = do
    display <- openDisplay ""
    unAosdPtr <- c'aosd_new_debug "aosdNew"
    aosdStructOwnedDataVar <- newMVar Nothing

    return AosdPtr {unAosdPtr,aosdStructOwnedDataVar,display}


aosdNew :: (AosdRenderer renderer) => AosdOptions -> renderer -> IO AosdPtr
aosdNew opts r = do
    aosd <- aosdNew0
    z <- reconfigure0 opts r aosd
    modifyMVar_ (aosdStructOwnedDataVar aosd) (\x -> assert (isNothing x) $ return (Just z))

    return aosd




reconfigure :: (AosdRenderer renderer) =>
        AosdOptions
     -> renderer
     -> AosdPtr
     -> IO ()

reconfigure opts r aosd@AosdPtr {aosdStructOwnedDataVar} = modifyMVar_ aosdStructOwnedDataVar
    (\zOld -> do
        zNew <- reconfigure0 opts r aosd
        maybeDo (freeAosdStructOwnedData "reconfigure") zOld
        return (Just zNew))

wrapAosd :: (Ptr C'Aosd -> c) -> AosdPtr -> c
wrapAosd f = f . unAosdPtr

aosdRender :: AosdPtr -> IO ()
aosdRender = wrapAosd c'aosd_render

aosdShow :: AosdPtr -> IO ()
aosdShow = wrapAosd c'aosd_show

aosdHide :: AosdPtr -> IO ()
aosdHide = wrapAosd c'aosd_hide

aosdLoopOnce :: AosdPtr -> IO ()
aosdLoopOnce = wrapAosd c'aosd_loop_once

aosdLoopFor ::
        AosdPtr
     -> CUInt -- ^ Time in milliseconds.
     -> IO ()
aosdLoopFor a millis = wrapAosd (flip c'aosd_loop_for millis) a


data AosdStructOwnedData =
    AosdStructOwnedData
        !(StablePtr (Cairo -> IO ()))
        !(Maybe (StablePtr (Ptr C'AosdMouseEvent -> IO())))



freeAosdStructOwnedData :: String -> AosdStructOwnedData -> IO ()
freeAosdStructOwnedData cxt (AosdStructOwnedData sp_r sp_h) = do
    freeStablePtrDebug cxt "renderer" sp_r
    maybeDo (freeStablePtrDebug cxt "mouse event handler") sp_h

aosdDestroy :: AosdPtr -> IO ()
aosdDestroy AosdPtr {unAosdPtr, aosdStructOwnedDataVar, display} =
    modifyMVar_ aosdStructOwnedDataVar $ \z -> do
        c'aosd_destroy_debug "aosdDestroy" unAosdPtr
        maybeDo (freeAosdStructOwnedData "aosdDestroy") z
        closeDisplay display
        return Nothing


-- | 'aosdNew'/'aosdDestroy' bracket. Leaking the 'AosdPtr' out of the third argument leads to undefined behaviour.
withAosd :: AosdRenderer renderer => AosdOptions -> renderer -> (AosdPtr -> IO c) -> IO c
withAosd opts ren = bracket (aosdNew opts ren) aosdDestroy
