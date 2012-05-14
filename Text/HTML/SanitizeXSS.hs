{-# LANGUAGE OverloadedStrings #-}
module Text.HTML.SanitizeXSS
    ( sanitize
    , sanitizeBalance
    , sanitizeXSS
    , filterTags
    , safeTags
    , sanitizeAttribute
    , sanitaryURI
    ) where

import Text.HTML.SanitizeXSS.Css

import Text.HTML.TagSoup

import Data.Set (Set(), member, notMember, (\\), fromList)
import Data.Char ( toLower )
import Data.Text (Text)
import qualified Data.Text as T

import Network.URI ( parseURIReference, URI (..),
                     isAllowedInURI, escapeURIString, uriScheme )
import Codec.Binary.UTF8.String ( encodeString )

import qualified Data.Map as Map
import Data.Maybe (catMaybes)



-- | santize the html to prevent XSS attacks. See README.md <http://github.com/gregwebs/haskell-xss-sanitize> for more details
sanitize :: Text -> Text
sanitize = sanitizeXSS

-- | alias of sanitize function
sanitizeXSS :: Text -> Text
sanitizeXSS = filterTags safeTags

-- | same as sanitize but makes sure there are no lone closing tags. See README.md <http://github.com/gregwebs/haskell-xss-sanitize> for more details
sanitizeBalance :: Text -> Text
sanitizeBalance = filterTags (balance Map.empty . safeTags)

-- | insert custom tag filtering. Don't forget to compose your filter with safeTags!
filterTags :: ([Tag Text] -> [Tag Text]) -> Text -> Text
filterTags f = renderTagsOptions renderOptions {
    optMinimize = \x -> x `elem` ["br","img"] -- <img><img> converts to <img />, <a/> converts to <a></a>
  } .  f . canonicalizeTags . parseTags

balance :: Map.Map Text Int -> [Tag Text] -> [Tag Text]
balance m [] =
    concatMap go $ Map.toList m
  where
    go (name, i)
        | noClosing name = []
        | otherwise = replicate i $ TagClose name
    noClosing = flip elem ["br", "img"]
balance m (t@(TagClose name):tags) =
    case Map.lookup name m of
        Nothing -> TagOpen name [] : TagClose name : balance m tags
        Just i ->
            let m' = if i == 1
                        then Map.delete name m
                        else Map.insert name (i - 1) m
             in t : balance m' tags
balance m (TagOpen name as : tags) =
    TagOpen name as : balance m' tags
  where
    m' = case Map.lookup name m of
            Nothing -> Map.insert name 1 m
            Just i -> Map.insert name (i + 1) m
balance m (t:ts) = t : balance m ts

-- | Filters out any usafe tags and attributes. Use with filterTags to create a custom filter.
safeTags :: [Tag Text] -> [Tag Text]
safeTags [] = []
safeTags (t@(TagClose name):tags)
    | safeTagName name = t : safeTags tags
    | otherwise = safeTags tags
safeTags (TagOpen name attributes:tags)
  | safeTagName name = TagOpen name
      (catMaybes $ map sanitizeAttribute attributes) : safeTags tags
  | otherwise = safeTags tags
safeTags (t:tags) = t:safeTags tags

safeTagName :: Text -> Bool
safeTagName tagname = tagname `member` sanitaryTags


safeAttribute :: (Text, Text) -> Bool
safeAttribute (name, value) = name `member` sanitaryAttributes &&
  (name `notMember` uri_attributes || sanitaryURI value)

sanitizeAttribute :: (Text, Text) -> Maybe (Text, Text)
sanitizeAttribute ("style", value) =
  let css = sanitizeCSS value in if T.null css then Nothing else Just ("style", css)
sanitizeAttribute attr = if safeAttribute attr
                            then Just attr
                            else Nothing

-- | Returns @True@ if the specified URI is not a potential security risk.
sanitaryURI :: Text -> Bool
sanitaryURI u =
  case parseURIReference (escapeURI $ T.unpack u) of
     Just p  -> (null (uriScheme p)) ||
                ((map toLower $ init $ uriScheme p) `member` safeURISchemes)
     Nothing -> False


-- | Escape unicode characters in a URI.  Characters that are
-- already valid in a URI, including % and ?, are left alone.
escapeURI :: String -> String
escapeURI = escapeURIString isAllowedInURI . encodeString

safeURISchemes :: Set String
safeURISchemes = fromList acceptable_protocols

sanitaryTags :: Set Text
sanitaryTags = fromList (acceptable_elements ++ mathml_elements ++ svg_elements)
  \\ (fromList svg_allow_local_href) -- extra filtering not implemented

sanitaryAttributes :: Set Text
sanitaryAttributes = fromList (allowed_html_uri_attributes ++ acceptable_attributes ++ mathml_attributes ++ svg_attributes)
  \\ (fromList svg_attr_val_allows_ref) -- extra unescaping not implemented

allowed_html_uri_attributes :: [Text]
allowed_html_uri_attributes = ["href", "src", "cite", "action", "longdesc"]

uri_attributes :: Set Text
uri_attributes = fromList $ allowed_html_uri_attributes ++ ["xlink:href", "xml:base"]

acceptable_elements :: [Text]
acceptable_elements = ["a", "abbr", "acronym", "address", "area",
    "article", "aside", "audio", "b", "big", "blockquote", "br", "button",
    "canvas", "caption", "center", "cite", "code", "col", "colgroup",
    "command", "datagrid", "datalist", "dd", "del", "details", "dfn",
    "dialog", "dir", "div", "dl", "dt", "em", "event-source", "fieldset",
    "figure", "footer", "font", "form", "header", "h1", "h2", "h3", "h4",
    "h5", "h6", "hr", "i", "img", "input", "ins", "keygen", "kbd",
    "label", "legend", "li", "m", "map", "menu", "meter", "multicol",
    "nav", "nextid", "ol", "output", "optgroup", "option", "p", "pre",
    "progress", "q", "s", "samp", "section", "select", "small", "sound",
    "source", "spacer", "span", "strike", "strong", "sub", "sup", "table",
    "tbody", "td", "textarea", "time", "tfoot", "th", "thead", "tr", "tt",
    "u", "ul", "var", "video"]
  
mathml_elements :: [Text]
mathml_elements = ["maction", "math", "merror", "mfrac", "mi",
    "mmultiscripts", "mn", "mo", "mover", "mpadded", "mphantom",
    "mprescripts", "mroot", "mrow", "mspace", "msqrt", "mstyle", "msub",
    "msubsup", "msup", "mtable", "mtd", "mtext", "mtr", "munder",
    "munderover", "none"]

-- this should include altGlyph I think
svg_elements :: [Text]
svg_elements = ["a", "animate", "animateColor", "animateMotion",
    "animateTransform", "clipPath", "circle", "defs", "desc", "ellipse",
    "font-face", "font-face-name", "font-face-src", "g", "glyph", "hkern",
    "linearGradient", "line", "marker", "metadata", "missing-glyph",
    "mpath", "path", "polygon", "polyline", "radialGradient", "rect",
    "set", "stop", "svg", "switch", "text", "title", "tspan", "use"]
  
acceptable_attributes :: [Text]
acceptable_attributes = ["abbr", "accept", "accept-charset", "accesskey",
    "align", "alt", "autocomplete", "autofocus", "axis",
    "background", "balance", "bgcolor", "bgproperties", "border",
    "bordercolor", "bordercolordark", "bordercolorlight", "bottompadding",
    "cellpadding", "cellspacing", "ch", "challenge", "char", "charoff",
    "choff", "charset", "checked", "class", "clear", "color",
    "cols", "colspan", "compact", "contenteditable", "controls", "coords",
    -- "data", TODO: allow this with further filtering
    "datafld", "datapagesize", "datasrc", "datetime", "default",
    "delay", "dir", "disabled", "draggable", "dynsrc", "enctype", "end",
    "face", "for", "form", "frame", "galleryimg", "gutter", "headers",
    "height", "hidefocus", "hidden", "high", "hreflang", "hspace",
    "icon", "id", "inputmode", "ismap", "keytype", "label", "leftspacing",
    "lang", "list", "loop", "loopcount", "loopend",
    "loopstart", "low", "lowsrc", "max", "maxlength", "media", "method",
    "min", "multiple", "name", "nohref", "noshade", "nowrap", "open",
    "optimum", "pattern", "ping", "point-size", "prompt", "pqg",
    "radiogroup", "readonly", "rel", "repeat-max", "repeat-min",
    "replace", "required", "rev", "rightspacing", "rows", "rowspan",
    "rules", "scope", "selected", "shape", "size", "span", "start",
    "step",
    "style", -- gets further filtering
    "summary", "suppress", "tabindex", "target",
    "template", "title", "toppadding", "type", "unselectable", "usemap",
    "urn", "valign", "value", "variable", "volume", "vspace", "vrml",
    "width", "wrap", "xml:lang"]

acceptable_protocols :: [String]
acceptable_protocols = [ "ed2k", "ftp", "http", "https", "irc",
    "mailto", "news", "gopher", "nntp", "telnet", "webcal",
    "xmpp", "callto", "feed", "urn", "aim", "rsync", "tag",
    "ssh", "sftp", "rtsp", "afs" ]

mathml_attributes :: [Text]
mathml_attributes = ["actiontype", "align", "columnalign", "columnalign",
    "columnalign", "columnlines", "columnspacing", "columnspan", "depth",
    "display", "displaystyle", "equalcolumns", "equalrows", "fence",
    "fontstyle", "fontweight", "frame", "height", "linethickness", "lspace",
    "mathbackground", "mathcolor", "mathvariant", "mathvariant", "maxsize",
    "minsize", "other", "rowalign", "rowalign", "rowalign", "rowlines",
    "rowspacing", "rowspan", "rspace", "scriptlevel", "selection",
    "separator", "stretchy", "width", "width", "xlink:href", "xlink:show",
    "xlink:type", "xmlns", "xmlns:xlink"]

svg_attributes :: [Text]
svg_attributes = ["accent-height", "accumulate", "additive", "alphabetic",
    "arabic-form", "ascent", "attributeName", "attributeType",
    "baseProfile", "bbox", "begin", "by", "calcMode", "cap-height",
    "class", "clip-path", "color", "color-rendering", "content", "cx",
    "cy", "d", "dx", "dy", "descent", "display", "dur", "end", "fill",
    "fill-opacity", "fill-rule", "font-family", "font-size",
    "font-stretch", "font-style", "font-variant", "font-weight", "from",
    "fx", "fy", "g1", "g2", "glyph-name", "gradientUnits", "hanging",
    "height", "horiz-adv-x", "horiz-origin-x", "id", "ideographic", "k",
    "keyPoints", "keySplines", "keyTimes", "lang", "marker-end",
    "marker-mid", "marker-start", "markerHeight", "markerUnits",
    "markerWidth", "mathematical", "max", "min", "name", "offset",
    "opacity", "orient", "origin", "overline-position",
    "overline-thickness", "panose-1", "path", "pathLength", "points",
    "preserveAspectRatio", "r", "refX", "refY", "repeatCount",
    "repeatDur", "requiredExtensions", "requiredFeatures", "restart",
    "rotate", "rx", "ry", "slope", "stemh", "stemv", "stop-color",
    "stop-opacity", "strikethrough-position", "strikethrough-thickness",
    "stroke", "stroke-dasharray", "stroke-dashoffset", "stroke-linecap",
    "stroke-linejoin", "stroke-miterlimit", "stroke-opacity",
    "stroke-width", "systemLanguage", "target", "text-anchor", "to",
    "transform", "type", "u1", "u2", "underline-position",
    "underline-thickness", "unicode", "unicode-range", "units-per-em",
    "values", "version", "viewBox", "visibility", "width", "widths", "x",
    "x-height", "x1", "x2", "xlink:actuate", "xlink:arcrole",
    "xlink:href", "xlink:role", "xlink:show", "xlink:title", "xlink:type",
    "xml:base", "xml:lang", "xml:space", "xmlns", "xmlns:xlink", "y",
    "y1", "y2", "zoomAndPan"]

-- the values for these need to be escaped
svg_attr_val_allows_ref :: [Text]
svg_attr_val_allows_ref = ["clip-path", "color-profile", "cursor", "fill",
    "filter", "marker", "marker-start", "marker-mid", "marker-end",
    "mask", "stroke"]

svg_allow_local_href :: [Text]
svg_allow_local_href = ["altGlyph", "animate", "animateColor",
    "animateMotion", "animateTransform", "cursor", "feImage", "filter",
    "linearGradient", "pattern", "radialGradient", "textpath", "tref",
    "set", "use"]

