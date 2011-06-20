{-# OPTIONS -Wall #-}
module Graphics.Aosd.Options where

import Graphics.Aosd.AOSD_H
import Foreign.C.Types

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

toAosdTransparency :: Transparency -> C'AosdTransparency
toAosdTransparency None = c'TRANSPARENCY_NONE
toAosdTransparency Fake = c'TRANSPARENCY_FAKE
toAosdTransparency Composite = c'TRANSPARENCY_COMPOSITE

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
symDurations fadeMillis fullMillis_ = FlashDurations fadeMillis fullMillis_ fadeMillis

