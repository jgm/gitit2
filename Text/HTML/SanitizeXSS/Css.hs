{-# LANGUAGE OverloadedStrings, CPP #-}
module Text.HTML.SanitizeXSS.Css (
  sanitizeCSS
#ifdef TEST
, allowedCssAttributeValue
#endif
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy (toStrict)
import Data.Set (member, fromList, Set)
import Data.Char (isDigit)
import Control.Applicative ((<|>), pure)
import Text.CSS.Render (renderAttrs)
import Text.CSS.Parse (parseAttrs)
import Prelude hiding (takeWhile)

-- import FileLocation (debug, debugM)


-- this is a direct translation from sanitizer.py, except
--   sanitizer.py filters out url(), but this is redundant
sanitizeCSS :: Text -> Text
sanitizeCSS css = toStrict . toLazyText .
    renderAttrs . filter isSanitaryAttr . filterUrl $ parseAttributes
  where
    filterUrl :: [(Text,Text)] -> [(Text,Text)]
    filterUrl = map filterUrlAttribute
      where
        filterUrlAttribute :: (Text, Text) -> (Text, Text)
        filterUrlAttribute (prop,value) =
            case parseOnly rejectUrl value of
              Left _ -> (prop,value)
              Right noUrl -> filterUrlAttribute (prop, noUrl)

        rejectUrl = do
          pre <- manyTill anyChar (string "url")
          skipMany space
          _<-char '('
          skipWhile (/= ')')
          _<-char ')'
          rest <- takeText
          return $ T.append (T.pack pre) rest


    parseAttributes = case parseAttrs css of
      Left _ -> []
      Right as -> as

    isSanitaryAttr (_, "") = False
    isSanitaryAttr ("",_)  = False
    isSanitaryAttr (prop, value)
      | prop `member` allowed_css_properties = True
      | (T.takeWhile (/= '-') prop) `member` allowed_css_unit_properties &&
          all allowedCssAttributeValue (T.words value) = True
      | prop `member` allowed_svg_properties = True
      | otherwise = False

    allowed_css_unit_properties :: Set Text
    allowed_css_unit_properties = fromList ["background","border","margin","padding"]

allowedCssAttributeValue :: Text -> Bool
allowedCssAttributeValue val =
  val `member` allowed_css_keywords ||
    case parseOnly allowedCssAttributeParser val of
        Left _ -> False
        Right b -> b
  where
    allowedCssAttributeParser = do
      rgb <|> hex <|> rgb <|> cssUnit

    aToF = fromList "abcdef"

    hex = do
      _ <- char '#'
      hx <- takeText
      return $ T.all (\c -> isDigit c || (c `member` aToF)) hx

    -- should have used sepBy (symbol ",")
    rgb = do
      _<- string "rgb("
      skipMany1 digit >> skipOk (== '%')
      skip (== ',')
      skipMany digit >> skipOk (== '%')
      skip (== ',')
      skipMany digit >> skipOk (== '%')
      skip (== ')')
      return True

    cssUnit = do
      skip isDigit
      skipOk isDigit
      skipOk (== '.')
      skipOk isDigit >> skipOk isDigit
      skipSpace
      unit <- takeText
      return $ T.null unit || unit `member` allowed_css_attribute_value_units

skipOk :: (Char -> Bool) -> Parser ()
skipOk p = skip p <|> pure ()

allowed_css_attribute_value_units :: Set Text
allowed_css_attribute_value_units = fromList
  [ "cm", "em", "ex", "in", "mm", "pc", "pt", "px", "%", ",", "\\"]

allowed_css_properties :: Set Text
allowed_css_properties = fromList acceptable_css_properties
  where
    acceptable_css_properties = ["azimuth", "background-color",
      "border-bottom-color", "border-collapse", "border-color",
      "border-left-color", "border-right-color", "border-top-color", "clear",
      "color", "cursor", "direction", "display", "elevation", "float", "font",
      "font-family", "font-size", "font-style", "font-variant", "font-weight",
      "height", "letter-spacing", "line-height", "overflow", "pause",
      "pause-after", "pause-before", "pitch", "pitch-range", "richness",
      "speak", "speak-header", "speak-numeral", "speak-punctuation",
      "speech-rate", "stress", "text-align", "text-decoration", "text-indent",
      "unicode-bidi", "vertical-align", "voice-family", "volume",
      "white-space", "width"]

allowed_css_keywords :: Set Text
allowed_css_keywords = fromList acceptable_css_keywords
  where
    acceptable_css_keywords = ["auto", "aqua", "black", "block", "blue",
      "bold", "both", "bottom", "brown", "center", "collapse", "dashed",
      "dotted", "fuchsia", "gray", "green", "!important", "italic", "left",
      "lime", "maroon", "medium", "none", "navy", "normal", "nowrap", "olive",
      "pointer", "purple", "red", "right", "solid", "silver", "teal", "top",
      "transparent", "underline", "white", "yellow"]

-- used in css filtering
allowed_svg_properties :: Set Text
allowed_svg_properties = fromList acceptable_svg_properties
  where
    acceptable_svg_properties = [ "fill", "fill-opacity", "fill-rule",
        "stroke", "stroke-width", "stroke-linecap", "stroke-linejoin",
        "stroke-opacity"]
