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
    -- * Displaying
    aosdFlash,
    FlashDurations(..), symDurations,
    -- ** Low-level operations
    AosdForeignPtr,aosdNew,aosdDestroy,reconfigure,
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
import Graphics.Aosd.XUtil
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Internal(runRender,Cairo)
import Graphics.Rendering.Pango.Enums
import System.IO.Unsafe
import Graphics.X11.Xlib(Display,openDisplay,closeDisplay)
import Control.Exception






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

type Render0 = Cairo -> IO ()







reconfigure0 :: (AosdRenderer renderer) => Display -> AosdOptions -> renderer -> Ptr C'Aosd -> IO AosdStructOwnedData
reconfigure0 display AosdOptions{..} renderer ptr = do
        GeneralRenderer{..} <- toGeneralRenderer renderer

        ScreenSize{..} <- getScreenSize display


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






        maybeDo (setClassHint ptr) classHint
        maybeDo (setHideUponMouseEvent ptr) hideUponMouseEvent
        maybeDo (setMouseEventCB ptr) mouseEventCB

        rendererPtr <- newStablePtrDebug "reconfigure0" "renderer" (runReaderT . runRender $ finalRenderer)


        maybeDo (c'aosd_set_transparency ptr . toAosdTransparency) transparency
        c'aosd_set_geometry ptr windowLeft windowTop windowWidth windowHeight
        c'aosd_set_renderer ptr theC'AosdRenderer (castCairoIOStablePtrToPtr rendererPtr)

        return (AosdStructOwnedData rendererPtr)



castCairoIOStablePtrToPtr :: StablePtr Render0 -> Ptr ()
castCairoIOStablePtrToPtr = castStablePtrToPtr

-- | Excepts its second argument to be a (StablePtr (Cairo -> IO ())).
theAosdRenderer :: Cairo -> Ptr () -> IO ()
theAosdRenderer cairo p = do
    render <- Foreign.StablePtr.deRefStablePtr (Foreign.StablePtr.castPtrToStablePtr p) :: IO Render0
    render cairo

-- | A 'FunPtr' to 'theAosdRenderer'.
{-# NOINLINE theC'AosdRenderer #-}
theC'AosdRenderer :: C'AosdRenderer
theC'AosdRenderer = unsafePerformIO (mk'AosdRenderer theAosdRenderer)


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
    c'aosd_set_mouse_event_cb a fptr Foreign.Ptr.nullPtr
  where
    f' :: Ptr C'AosdMouseEvent -> Ptr () -> IO ()
    f' p _ = do
        mouseEvent <- peek p
        f mouseEvent


-- | Main high-level displayer. Blocks.
aosdFlash :: AosdForeignPtr -> FlashDurations -> IO ()
aosdFlash a FlashDurations{..} = wrapAosd (\p -> c'aosd_flash p inMillis fullMillis outMillis) a

data AosdForeignPtr = AosdForeignPtr { unAosdPtr :: !(Ptr C'Aosd)
                                        -- We only keep this around for deallocating
                                     , aosdStructOwnedDataVar :: !(MVar AosdStructOwnedData)
                                     , display :: Display
                                     }

aosdNew :: (AosdRenderer renderer) => AosdOptions -> renderer -> IO AosdForeignPtr
aosdNew opts r = do
    display <- openDisplay ""
    unAosdPtr <- c'aosd_new_debug "aosdNew"
    z <- reconfigure0 display opts r unAosdPtr
    aosdStructOwnedDataVar <- newMVar z

    return AosdForeignPtr {unAosdPtr,aosdStructOwnedDataVar,display}




reconfigure :: (AosdRenderer renderer) =>
        AosdOptions
     -> renderer
     -> AosdForeignPtr
     -> IO ()

reconfigure opts r AosdForeignPtr {unAosdPtr,aosdStructOwnedDataVar,display} = modifyMVar_ aosdStructOwnedDataVar
    (\zOld -> do
        zNew <- reconfigure0 display opts r unAosdPtr
        freeAosdStructOwnedData "reconfigure" zOld
        return zNew)

wrapAosd :: (Ptr C'Aosd -> c) -> AosdForeignPtr -> c
wrapAosd f = f . unAosdPtr 

aosdRender :: AosdForeignPtr -> IO ()
aosdRender = wrapAosd c'aosd_render

aosdShow :: AosdForeignPtr -> IO ()
aosdShow = wrapAosd c'aosd_show

aosdHide :: AosdForeignPtr -> IO ()
aosdHide = wrapAosd c'aosd_hide

aosdLoopOnce :: AosdForeignPtr -> IO ()
aosdLoopOnce = wrapAosd c'aosd_loop_once

aosdLoopFor ::
        AosdForeignPtr
     -> CUInt -- ^ Time in milliseconds.
     -> IO ()
aosdLoopFor a millis = wrapAosd (flip c'aosd_loop_for millis) a


data AosdStructOwnedData = AosdStructOwnedData !(StablePtr (Cairo -> IO ()))



freeAosdStructOwnedData :: String -> AosdStructOwnedData -> IO ()
freeAosdStructOwnedData cxt (AosdStructOwnedData sp_r) = do
    freeStablePtrDebug cxt "renderer" sp_r

aosdDestroy :: AosdForeignPtr -> IO ()
aosdDestroy AosdForeignPtr {unAosdPtr,aosdStructOwnedDataVar,display} = mask_ $ do
    c'aosd_destroy_debug "aosdDestroy" unAosdPtr
    z <- readMVar aosdStructOwnedDataVar
    freeAosdStructOwnedData "aosdDestroy" z
    closeDisplay display
