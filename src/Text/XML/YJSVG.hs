module Text.XML.YJSVG (
  showSVG
, SVG(..)
, Transform(..)
, Color(..)
, Position(..), topleft, center
, yjsvgVersion
) where

import Text.XML.HaXml(AttValue(..), QName(..), Prolog(..),
	EncodingDecl(..), XMLDecl(..), SystemLiteral(..), PubidLiteral(..),
	ExternalID(..), DocTypeDecl(..), Misc(..), Element(..), Content(..),
	Document(..))
import Text.XML.HaXml.Pretty
import Data.Word(Word8)

yjsvgVersion :: (Int, String)
yjsvgVersion = (7, "0.1.11a")

type Font = String
data Position
	= TopLeft{posX :: Double, posY :: Double}
	| Center{posX :: Double, posY :: Double}
	deriving (Show, Read)

topleft, center :: Double -> Double -> Position -> Position
topleft w h Center{posX = x, posY = y} =
	TopLeft{posX = x + w / 2, posY = - y + h / 2}
topleft _ _ pos = pos
center w h TopLeft{posX = x, posY = y} =
	Center{posX = x - w / 2, posY = - y + h / 2}
center _ _ pos = pos

getPos :: Double -> Double -> Position -> (Double, Double)
getPos _ _ TopLeft{posX = x, posY = y} = (x, y)
getPos w h Center{posX = x, posY = y} = (x + w / 2, - y + h / 2)

data SVG   = Line Position Position Color Double |
             Polyline [Position] Color Color Double |
             Rect Position Double Double Double Color Color |
             Fill Color |
	     Circle Position Double Color |
             Text Position Double Color Font String |
	     Image Position Double Double FilePath |
	     Group [ Transform ] [ SVG ]
	deriving (Show, Read)
data Color
	= ColorName{
		colorName :: String
	 }
	| RGB {
		colorRed :: Word8,
		colorGreen ::  Word8,
		colorBlue ::  Word8
	 }
	deriving (Eq, Show, Read)

mkColorStr :: Color -> String
mkColorStr ColorName{colorName = n} = n
mkColorStr RGB{colorRed = r, colorGreen = g, colorBlue = b} =
	"rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

data Transform = Matrix Double Double Double Double Double Double |
                 Translate Double Double |
		 Scale Double Double |
		 Rotate Double (Maybe (Double, Double)) |
		 SkewX Double |
		 SkewY Double
	deriving (Show, Read)

showTrans :: Transform -> String
showTrans (Translate tx ty) = "translate(" ++ show tx ++ "," ++ show ty ++ ")"
showTrans (Scale sx sy) = "scale(" ++ show sx ++ "," ++ show sy ++ ")"
showTrans _ = error "not implemented yet"

showSVG :: Double -> Double -> [ SVG ] -> String
showSVG w h = show . document . svgToXml w h

svgToElem :: Double -> Double -> SVG -> Element ()
svgToElem pw ph (Line p1 p2 color lineWidth)
  = Elem (N "line") [
       ( N "x1", AttValue [ Left $ show x1 ] )
     , ( N "y1", AttValue [ Left $ show y1 ] )
     , ( N "x2", AttValue [ Left $ show x2 ] )
     , ( N "y2", AttValue [ Left $ show y2 ] )
     , ( N "stroke", AttValue [ Left $ mkColorStr color ] )
     , ( N "stroke-width", AttValue [ Left $ show lineWidth ] )
	, (N "stroke-linecap", AttValue [Left "round"])
     ] []
	where
	(x1, y1) = getPos pw ph p1
	(x2, y2) = getPos pw ph p2

svgToElem pw ph (Polyline points fillColor lineColor lineWidth)
  = Elem (N "polyline") [
       ( N "points", AttValue [ Left $ pointsToAttVal points ] )
     , ( N "fill"  , AttValue [ Left $ mkColorStr fillColor ] )
     , ( N "stroke", AttValue [ Left $ mkColorStr lineColor ] )
     , ( N "stroke-width", AttValue [ Left $ show lineWidth ] )
     ] []
  where
  pointsToAttVal :: [Position] -> String
  pointsToAttVal [] = ""
  pointsToAttVal (p : ps)
    = let (x, y) = getPos pw ph p in
	show x ++ "," ++ show y ++ " " ++ pointsToAttVal ps

svgToElem pw ph (Rect p w h sw cf cs)
  = Elem (N "rect") [
   ( N "x", AttValue [ Left $ show x ] )
 , ( N "y", AttValue [ Left $ show y ] )
 , ( N "width", AttValue [ Left $ show w ] )
 , ( N "height", AttValue [ Left $ show h ] )
 , ( N "stroke-width", AttValue [ Left $ show sw ] )
 , ( N "fill", AttValue [ Left $ mkColorStr cf ] )
 , ( N "stroke", AttValue [ Left $ mkColorStr cs ] )
 ] []
	where
	(x, y) = getPos pw ph p

svgToElem pw ph (Fill c)
  = svgToElem pw ph $ Rect (TopLeft 0 0) pw ph 0 c c

svgToElem pw ph (Text p s c f t)
  = Elem (N "text") [
   ( N "x", AttValue [ Left $ show x ] )
 , ( N "y", AttValue [ Left $ show y ] )
 , ( N "font-family", AttValue [ Left f ] )
 , ( N "font-size", AttValue [ Left $ show s ] )
 , ( N "fill", AttValue [ Left $ mkColorStr c ] )
 ] [ CString False t () ]
	where
	(x, y) = getPos pw ph p

svgToElem pw ph (Circle p r c)
  = Elem (N "circle") [
       ( N "cx", AttValue [ Left $ show x ] )
     , ( N "cy", AttValue [ Left $ show y ] )
     , ( N "r", AttValue [ Left $ show r ] )
     , ( N "fill", AttValue [ Left $ mkColorStr c ] )
     ] []
	where
	(x, y) = getPos pw ph p

svgToElem pw ph (Image p w h path)
  = Elem (N "image") [
       ( N "x", AttValue [ Left $ show x ] )
     , ( N "y", AttValue [ Left $ show y ] )
     , ( N "width", AttValue [ Left $ show w ] )
     , ( N "height", AttValue [ Left $ show h ] )
     , ( N "xlink:href", AttValue [ Left path ] )
     ] []
	where
	(x, y) = getPos pw ph p

svgToElem pw ph (Group trs svgs)
  = Elem (N "g") (
      map (\ tr -> let a = showTrans tr
                    in ( N "transform", AttValue [ Left a ] ) ) trs
     ) $ map (flip CElem () . svgToElem pw ph) svgs


svgToXml :: Double -> Double -> [ SVG ] -> Document ()
svgToXml w h svgs
  = Document prlg ent
      (Elem (N "svg") (emAtt w h) $ map (flip CElem () . svgToElem w h) svgs) els

pmsc :: [Misc]
pmsc    = [ Comment " MADE BY SVG.HS " ]
pblit, sslit :: String
pblit   = "-//W3C//DTD SVG 1.1//EN"
sslit   = "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"
doctype :: DocTypeDecl
doctype = DTD (N "svg") (Just (PUBLIC (PubidLiteral pblit) (SystemLiteral sslit))) []
xmldcl :: XMLDecl
xmldcl  = XMLDecl "1.0" (Just (EncodingDecl "UTF-8")) Nothing
prlg :: Prolog
prlg    = Prolog (Just xmldcl) pmsc (Just doctype) []

xmlns, ver, xlink :: String
xmlns   = "http://www.w3.org/2000/svg"
ver     = "1.1"
xlink   = "http://www.w3.org/1999/xlink"
emAtt :: (Show a, Show b) => a -> b -> [(QName, AttValue)]
emAtt  w h = [
   ( N "xmlns", AttValue [ Left xmlns ] )
 , ( N "version", AttValue [ Left ver ] )
 , ( N "xmlns:xlink", AttValue [ Left xlink ] )
 , ( N "width", AttValue [ Left $ show w ] )
 , ( N "height", AttValue [ Left $ show h ] )
 ]
ent, els :: [a]
ent     = []
els     = []
{-
height, width :: String
width   = "400"
height  = "640"
em :: CharData -> Element ()
em ougon= Elem (N "svg") (emAtt (100 :: Int) (200 :: Int)) [ CElem rect (), CElem (text ougon) () ]
rect :: Element i
rect = Elem (N "rect") [
   ( N "x", AttValue [ Left "20" ] )
 , ( N "y", AttValue [ Left "30" ] )
 , ( N "width", AttValue [ Left "80" ] )
 , ( N "height", AttValue [ Left "100" ] )
 , ( N "fill", AttValue [ Left "red" ] )
 , ( N "stroke", AttValue [ Left "none" ] )
 ] []
text :: CharData -> Element ()
text ougon = Elem (N "text") [
   ( N "x", AttValue [ Left "20" ] )
 , ( N "y", AttValue [ Left "50" ] )
 , ( N "font-size", AttValue [ Left "20" ] )
 ] [ CString False ougon () ]

-- els     = [ Comment "MADE BY SIMPLE.HS" ]

docu :: CharData -> Document ()
docu ougon = Document prlg ent (em ougon) els

svgMain = do
  ougonXml <- readFile "ougon.xml"
  let Document _ _ (Elem "text" _ [ CString _ ougon _ ]) _
        = xmlParse "ougon.xml" ougonXml
  print $ document $ svgToXml 100 200 [ Rect 20 30 80 90 "black" "purple", Text 40 50 30 "underline" "purple" "good" ]
-}
