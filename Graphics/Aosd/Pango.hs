{-# LANGUAGE ViewPatterns, NoMonomorphismRestriction, ScopedTypeVariables, GeneralizedNewtypeDeriving, TypeSynonymInstances, NamedFieldPuns, RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Graphics.Aosd.Pango(

    -- * Base
    TextRenderer(..),Width(..),textRenderer,
    -- * PangoText
    PangoText,pText,pRaw,pEmpty,pTag,
    -- ** Markup tags
    pSpan,pBold,pBig,pItalic,pStrikethrough,pSmall,pSub,pSup,pUnderline,pMono,
    -- *** Span shorthands
    pSized,
    -- ** Standard text combinators
    pIntercalate,pUnlines,pUnwords,pShow,


    -- * Reexports
    SpanAttribute(..),
    LayoutAlignment(..),
    LayoutWrapMode(..),
    TabPosition,
    Size(..),
    sRGB,
    module Graphics.Aosd,
    module Data.Monoid,
    module Data.Colour.Names



    ) where

import Graphics.Aosd hiding(width)
import Graphics.Aosd.Util
import Graphics.Rendering.Pango.Cairo
import Graphics.Rendering.Pango.Enums
import Graphics.Rendering.Pango.Layout
import Control.Monad(void)
import Graphics.Rendering.Pango.Markup
import Data.Monoid
import Data.String
import Data.List(intersperse)
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names


data TextRenderer = TextRenderer {
    -- | Hint: Use 'sRGB' or "Data.Colour.Names".
    colour :: Colour Double,
    -- | 0: Transparent, 1: Opaque.
    opacity :: Double,
    -- | Uses 'layoutSetWidth' if set.
    width :: Maybe Width,
    -- | Uses 'layoutSetWrap' if set.
    wrapMode :: Maybe LayoutWrapMode,
    -- | Uses 'layoutSetJustify' if set.
    justify :: Maybe Bool,
    -- | Uses 'layoutSetAlignment' if set.
    alignment :: Maybe LayoutAlignment,
    -- | Uses 'layoutSetSpacing' if set.
    lineSpacing :: Maybe Double,
    -- | Uses 'layoutSetTabs' if set.
    tabs :: Maybe [TabPosition],
    -- | Uses 'layoutSetSingleParagraphMode' if set.
    singleParagraphMode :: Maybe Bool,
    -- | The actual text
    tcText :: PangoText
}

data Width = Unlimited -- ^ The layout will be as wide as necessary to hold all the lines without wrapping
           | Width Double -- ^ The layout will be wrapped (according to 'wrapMode') to the given width in Pango units
    deriving(Show,Eq)

unsup :: String -> t
unsup s = error (s ++ " unsupported for Graphics.Aosd.Pango.Width")

-- | Supports only 'fromInteger'.
instance Num Width where
    fromInteger = Width . fromIntegral
    (+) = unsup "(+)"
    (*) = unsup "(*)"
    abs = unsup "abs"
    signum = unsup "signum"
    (-) = unsup "(-)"
    negate = unsup "negate"

-- | Supports only 'fromRational'.
instance Fractional Width where
    fromRational = Width . fromRational
    (/) = unsup "/"
    recip = unsup "recip"



-- | Construct a 'TextConf' with most fields set to 'Nothing'
textRenderer :: PangoText -> TextRenderer
textRenderer t = TextRenderer {
    colour = green,
    opacity = 1,
    width = Nothing,
    wrapMode = Nothing,
    justify = Nothing,
    alignment = Nothing,
    lineSpacing = Nothing,
    tabs = Nothing,
    singleParagraphMode = Nothing,
    tcText = t
 }

-- | Plain text or some Pango markup. Suggestion: Use &#123;-\# LANGUAGE OverloadedStrings \#-&#125;.
data PangoText = PlainText ShowS
               | PangoMarkup ShowS
               | Empty

toMarkup :: PangoText -> ShowS
toMarkup (PlainText s) = showString (escapeMarkup (s ""))
toMarkup (PangoMarkup s) = s
toMarkup Empty = mempty

-- | Uses 'pText' (not 'pRaw').
instance IsString PangoText where
    fromString = pText

instance Semigroup PangoText where
    (<>) = mappend

instance Monoid PangoText where
    mempty = Empty
    mappend Empty x2 = x2
    mappend x1 Empty = x1
    mappend (PlainText s1) (PlainText s2) = PlainText (s1 . s2)
    mappend x1 x2 = PangoMarkup (toMarkup x1 . toMarkup x2)

instance Show PangoText where
    showsPrec _ Empty = showString "pEmpty"
    showsPrec prec (PlainText s) = showParen (prec >= 11) (showString "pText " . shows (s ""))
    showsPrec prec (PangoMarkup s) = showParen (prec >= 11) (showString "pRaw " . shows (s ""))

pEmpty :: PangoText
pEmpty = Empty

-- | Raw Pango markup, see <http://developer.gnome.org/pango/stable/PangoMarkupFormat.html>.
pRaw :: String -> PangoText
pRaw = PangoMarkup . showString

-- | Plain text.
pText :: String -> PangoText
pText = PlainText . showString

pSpan :: [SpanAttribute] -> PangoText -> PangoText
pSpan (attrs :: [SpanAttribute]) inner = PangoMarkup it
     where
       it = -- adapted from 'Graphics.Rendering.Pango.Markup.markSpan'
            showString "<span" .
                    foldr (.) (showChar '>') (map shows attrs) .
                    toMarkup inner .
                    showString "</span>"

pIntercalate :: PangoText -> [PangoText] -> PangoText
pIntercalate x = mconcat . intersperse x

pUnlines :: [PangoText] -> PangoText
pUnlines = pIntercalate (pText "\n")

pUnwords :: [PangoText] -> PangoText
pUnwords = pIntercalate (pText " ")

pShow :: Show a => a -> PangoText
pShow = pText . show

-- | Set font size in points
pSized :: Double -> PangoText -> PangoText
pSized pt = pSpan [FontSize (SizePoint pt)]

pTag :: String -- ^ Tag name
        -> PangoText -> PangoText
pTag tagName inner = PangoMarkup $
    showChar '<' . showString tagName . showChar '>' .
    toMarkup inner .
    showString "</" . showString tagName . showChar '>'

pBold :: PangoText -> PangoText
pBold = pTag "b"
pBig :: PangoText -> PangoText
pBig = pTag "big"
pItalic :: PangoText -> PangoText
pItalic = pTag "i"
pStrikethrough :: PangoText -> PangoText
pStrikethrough = pTag "s"


-- | Subscript
pSub :: PangoText -> PangoText
pSub = pTag "sub"

-- | Superscript
pSup :: PangoText -> PangoText
pSup = pTag "sup"

pSmall :: PangoText -> PangoText
pSmall = pTag "small"

-- | Monospace font
pMono :: PangoText -> PangoText
pMono = pTag "tt"


-- | Underline
pUnderline :: PangoText -> PangoText
pUnderline = pTag "u"







layoutSetWidth' :: PangoLayout -> Width -> IO ()
layoutSetWidth' layout w = layoutSetWidth layout (case w of
                                                       Unlimited -> Nothing
                                                       Width x -> Just x)

instance AosdRenderer TextRenderer where
  toGeneralRenderer TextRenderer{..}  = do
    fm <- cairoFontMapGetDefault
    -- resolution <- cairoFontMapGetResolution fm
    cxt <- cairoCreateContext (Just fm)
    layout <- layoutEmpty cxt

    case tcText of
         Empty -> return ()
         PlainText s -> layoutSetText layout (s "")
         PangoMarkup s -> void (layoutSetMarkup layout (s "") :: IO String)

    let go :: (PangoLayout -> a -> IO ()) -> Maybe a -> IO ()
        go f = maybeDo (f layout)


    go layoutSetWidth' width
    go layoutSetWrap wrapMode
    go layoutSetJustify justify
    go layoutSetAlignment alignment
    go layoutSetSpacing lineSpacing
    go layoutSetTabs tabs
    go layoutSetSingleParagraphMode singleParagraphMode


    (grInkExtent,grPositioningExtent) <- layoutGetPixelExtents layout
    let render = do
            --updateLayout layout -- No idea when this is neccessary
            setSourceColour colour opacity
            showLayout layout


    return GeneralRenderer { grInkExtent, grPositioningExtent, grRender = render }





-- getSize l = do
--     -- (ink,logical)
--     a@(Rectangle xi yi wi hi, Rectangle xl yl wl hl) <- layoutGetPixelExtents l
--
--     --     print ("ink",fst a)
--     --     print ("logical",snd a)
--
--     let w = max (xi+wi) (xl+wl)
--         h = max (yi+hi) (yl+hl)
--
--
--     return (fi w, fi h)
--
--   where
--     fi = fromIntegral
--
--
