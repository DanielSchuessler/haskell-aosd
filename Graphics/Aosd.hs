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
    AosdForeignPtr,aosdNew,reconfigure,
    aosdRender, aosdShow, aosdHide, aosdLoopOnce, aosdLoopFor,

    -- * Diagnostics
    debugRenderer,

    -- * Reexports
    module Graphics.Rendering.Cairo,
    Rectangle(..),
    CInt, CUInt

    ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Reader
import Foreign.C
import Foreign.ForeignPtr
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
import System.Mem.Weak






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

withAosd :: (Ptr C'Aosd -> IO a) -> IO a
withAosd = bracket (c'aosd_new_debug "withAosd") (c'aosd_destroy_debug "withAosd")

type Render0 = Cairo -> IO ()




withConfiguredAosd :: (AosdRenderer renderer) =>
                                                 AosdOptions -> renderer
                                              -> (Ptr C'Aosd -> IO a)
                                              -> IO a
withConfiguredAosd opts x k =
    withAosd (\a -> do
        sp <- reconfigure0 opts x a
        k a `finally` freeStablePtrDebug "withConfiguredAosd" "renderer" sp
    )


reconfigure0 :: (AosdRenderer renderer) => AosdOptions -> renderer -> Ptr C'Aosd -> IO (StablePtr Render0)
reconfigure0 AosdOptions{..} renderer ptr = do
        GeneralRenderer{..} <- toGeneralRenderer renderer

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






        maybeDo (setClassHint ptr) classHint
        maybeDo (setHideUponMouseEvent ptr) hideUponMouseEvent
        maybeDo (setMouseEventCB ptr) mouseEventCB

        rendererPtr <- newStablePtrDebug "reconfigure0" "renderer" (runReaderT . runRender $ finalRenderer)


        maybeDo (c'aosd_set_transparency ptr . toAosdTransparency) transparency
        c'aosd_set_geometry ptr windowLeft windowTop windowWidth windowHeight
        c'aosd_set_renderer ptr theC'AosdRenderer (castCairoIOStablePtrToPtr rendererPtr)

        return rendererPtr



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
aosdFlash :: (AosdRenderer a) => AosdOptions -> a -> FlashDurations -> IO ()
aosdFlash opts renderer durations = withConfiguredAosd opts renderer (aosdFlash' durations)


aosdFlash' :: FlashDurations -> Ptr C'Aosd -> IO ()
aosdFlash' FlashDurations{..} a = (c'aosd_flash a inMillis fullMillis outMillis)

data AosdForeignPtr = AosdForeignPtr { unAosdPtr :: !(ForeignPtr C'Aosd)
                                        -- This has the sole purpose of keeping the StablePtr alive
                                     , afpRendererVar :: !(MVar (StablePtr (Cairo -> IO ())))
                                     }

aosdNew :: (AosdRenderer renderer) => AosdOptions -> renderer -> IO AosdForeignPtr
aosdNew opts r = do
    p <- c'aosd_new_debug "aosdNew"
    sp <- reconfigure0 opts r p
    autoFreeStablePtr "(initial) renderer" sp
    afpRendererVar <- newMVar sp
    unAosdPtr <- newForeignPtrDebug "aosdNew" "C'Aosd" (c'aosd_destroy p) p
    -- The ForeignPtr C'Aosd should keep the StablePtr contained in it alive
    _ <- mkWeak unAosdPtr afpRendererVar
           (if debugMemory
               then Just (putDebugMemory "Weak-finalizer in aosdNew" "Finalizing")
               else Nothing)
    return AosdForeignPtr {unAosdPtr,afpRendererVar}




autoFreeStablePtr :: String -> StablePtr a -> IO ()
autoFreeStablePtr descr sp = void $
    mkWeakPtr sp (Just (freeStablePtrDebug "Weak-finalizer in autoFreeStablePtr" descr sp ))

reconfigure :: (AosdRenderer renderer) =>
        AosdOptions
     -> renderer
     -> AosdForeignPtr
     -> IO ()

reconfigure opts r (AosdForeignPtr fp var) = modifyMVar_ var
    (\_ -> do
        spNew <- reconfigure0 opts r (unsafeForeignPtrToPtr fp)
        autoFreeStablePtr "(new) renderer" spNew
        return spNew)


aosdRender :: AosdForeignPtr -> IO ()
aosdRender = (`withForeignPtr` c'aosd_render) . unAosdPtr

aosdShow :: AosdForeignPtr -> IO ()
aosdShow = (`withForeignPtr` c'aosd_show) . unAosdPtr

aosdHide :: AosdForeignPtr -> IO ()
aosdHide = (`withForeignPtr` c'aosd_hide) . unAosdPtr

aosdLoopOnce :: AosdForeignPtr -> IO ()
aosdLoopOnce = (`withForeignPtr` c'aosd_loop_once) . unAosdPtr

aosdLoopFor ::
        AosdForeignPtr
     -> CUInt -- ^ Time in milliseconds.
     -> IO ()
aosdLoopFor (AosdForeignPtr fp _) millis = (fp `withForeignPtr` (`c'aosd_loop_for` millis))

