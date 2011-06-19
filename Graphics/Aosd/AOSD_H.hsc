#include <bindings.dsl.h>
#include <aosd.h>

-- | Raw bindings.
--
-- Note: The 'Cairo' type (C @cairo_t*@) is from "Graphics.Rendering.Cairo.Types", whose haddock is (as of writing) hidden
module Graphics.Aosd.AOSD_H where
#strict_import
import Graphics.Rendering.Cairo.Types


#opaque_t Aosd

#integral_t AosdCoordinate 
#num COORDINATE_MINIMUM
#num COORDINATE_CENTER
#num COORDINATE_MAXIMUM

#starttype AosdMouseEvent
#field x, CInt
#field y, CInt
#field x_root, CInt
#field y_root, CInt
#field button, CUInt
#field time, CULong
#stoptype

#callback AosdRenderer, Cairo -> Ptr () -> IO ()
#callback AosdMouseEventCb, Ptr <AosdMouseEvent> -> Ptr () -> IO ()

#integral_t AosdTransparency
#num TRANSPARENCY_NONE
#num TRANSPARENCY_FAKE
#num TRANSPARENCY_COMPOSITE

#starttype XClassHint
#field res_name, CString
#field res_class, CString
#stoptype

#ccall aosd_new, IO (Ptr <Aosd>) 
#ccall aosd_destroy, Ptr <Aosd> -> IO ()
 
-- * object configurators

#ccall aosd_set_name, Ptr <Aosd> -> Ptr <XClassHint> -> IO ()
#ccall aosd_set_names, Ptr <Aosd> -> CString -> CString -> IO ()
#ccall aosd_set_transparency, Ptr <Aosd> -> <AosdTransparency> -> IO ()
#ccall aosd_set_geometry, Ptr <Aosd> -> CInt -> CInt -> CInt -> CInt -> IO ()
#ccall aosd_set_position, Ptr <Aosd> -> CUInt -> CInt -> CInt -> IO ()
#ccall aosd_set_position_offset, Ptr <Aosd> -> CInt -> CInt -> IO ()
#ccall aosd_set_position_with_offset, Ptr <Aosd> -> <AosdCoordinate> -> <AosdCoordinate> -> CInt -> CInt -> CInt -> CInt -> IO ()
#ccall aosd_set_renderer, Ptr <Aosd> -> <AosdRenderer> -> Ptr () -> IO ()
#ccall aosd_set_mouse_event_cb, Ptr <Aosd> -> <AosdMouseEventCb> -> Ptr () -> IO ()
#ccall aosd_set_hide_upon_mouse_event, Ptr <Aosd> -> CInt -> IO ()


-- * object manipulators

#ccall aosd_render, Ptr <Aosd> -> IO ()
#ccall aosd_show, Ptr <Aosd> -> IO ()
#ccall aosd_hide, Ptr <Aosd> -> IO ()


-- * X main loop processing

#ccall aosd_loop_once, Ptr <Aosd> -> IO ()
#ccall aosd_loop_for, Ptr <Aosd> -> CUInt -> IO ()


-- * automatic object manipulator

#ccall aosd_flash, Ptr <Aosd> -> CUInt -> CUInt -> CUInt -> IO ()

