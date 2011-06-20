{-# OPTIONS -Wall #-}
module Graphics.Aosd.Renderer where
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango.Enums
import Graphics.Aosd.Util


class AosdRenderer a where
    toGeneralRenderer :: a -> IO GeneralRenderer

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
 
instance AosdRenderer GeneralRenderer where
    toGeneralRenderer = return

debugRenderer :: GeneralRenderer
debugRenderer =
    GeneralRenderer {
        grInkExtent = rect,
        grPositioningExtent = rect,
        grRender = do
            liftIO (putStdErr "debugRenderer invoked")
            setLineWidth lw
            setSourceRGB  0 1 0
            arc 0 0 (r - lw) 0 (2*pi)
            stroke
    }
        where
            rect = Rectangle (-r) (-r) (2*r) (2*r)
            r :: Num a => a
            r = 100
            lw = 5

-- -- | Horizontal concatenation
-- data HCatRenderer = forall r1 r2. (AosdRenderer r1, AosdRenderer r2) => HCat r1 r2
--
--
-- -- | Vertical concatenation
-- data VCatRenderer = forall r1 r2. (AosdRenderer r1, AosdRenderer r2) => VCat r1 r2



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

