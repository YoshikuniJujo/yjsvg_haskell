module Text.XML.YJSVG (
  showSVG
, SVG(..)
, Transform(..)
, yjsvgVersion
) where

import Text.XML.HaXml(AttValue(..), QName(..), Prolog(..),
	EncodingDecl(..), XMLDecl(..), SystemLiteral(..), PubidLiteral(..),
	ExternalID(..), DocTypeDecl(..), Misc(..), Element(..), Content(..),
	Document(..))
import Text.XML.HaXml.Pretty

yjsvgVersion :: (Int, String)
yjsvgVersion = (1, "0.1.6")

data SVG   = Line Double Double Double Double Color Double |
             Polyline [ ( Double, Double ) ] Color Color Double |
             Rect Double Double Double Double Double Color Color |
	     Circle Double Double Double Color |
             Text Double Double Double Color String |
	     Image Double Double Double Double FilePath |
	     Group [ Transform ] [ SVG ]
type Color = String
data Transform = Matrix Double Double Double Double Double Double |
                 Translate Double Double |
		 Scale Double Double |
		 Rotate Double (Maybe (Double, Double)) |
		 SkewX Double |
		 SkewY Double

showTrans :: Transform -> String
showTrans (Translate tx ty) = "translate(" ++ show tx ++ "," ++ show ty ++ ")"
showTrans (Scale sx sy) = "scale(" ++ show sx ++ "," ++ show sy ++ ")"
showTrans _ = error "not implemented yet"

-- instance Show [ SVG ] where
--  show = showSVG

showSVG :: Double -> Double -> [ SVG ] -> String
showSVG w h = show . document . svgToXml w h

svgToElem :: SVG -> Element ()
svgToElem (Line x1 y1 x2 y2 color lineWidth)
  = Elem (N "line") [
       ( N "x1", AttValue [ Left $ show x1 ] )
     , ( N "y1", AttValue [ Left $ show y1 ] )
     , ( N "x2", AttValue [ Left $ show x2 ] )
     , ( N "y2", AttValue [ Left $ show y2 ] )
     , ( N "stroke", AttValue [ Left color ] )
     , ( N "stroke-width", AttValue [ Left $ show lineWidth ] )
     ] []

svgToElem (Polyline points fillColor lineColor lineWidth)
  = Elem (N "polyline") [
       ( N "points", AttValue [ Left $ pointsToAttVal points ] )
     , ( N "fill"  , AttValue [ Left fillColor ] )
     , ( N "stroke", AttValue [ Left lineColor ] )
     , ( N "stroke-width", AttValue [ Left $ show lineWidth ] )
     ] []
  where
  pointsToAttVal :: [ ( Double, Double ) ] -> String
  pointsToAttVal [] = ""
  pointsToAttVal ( (x, y):ps )
    = show x ++ "," ++ show y ++ " " ++ pointsToAttVal ps

svgToElem (Rect x y w h sw cf cs)
  = Elem (N "rect") [
   ( N "x", AttValue [ Left $ show x ] )
 , ( N "y", AttValue [ Left $ show y ] )
 , ( N "width", AttValue [ Left $ show w ] )
 , ( N "height", AttValue [ Left $ show h ] )
 , ( N "stroke-width", AttValue [ Left $ show sw ] )
 , ( N "fill", AttValue [ Left cf ] )
 , ( N "stroke", AttValue [ Left cs ] )
 ] []

svgToElem (Text x y s c t)
  = Elem (N "text") [
   ( N "x", AttValue [ Left $ show x ] )
 , ( N "y", AttValue [ Left $ show y ] )
 , ( N "font-size", AttValue [ Left $ show s ] )
 , ( N "fill", AttValue [ Left c ] )
 ] [ CString False t () ]

svgToElem (Circle x y r c)
  = Elem (N "circle") [
       ( N "cx", AttValue [ Left $ show x ] )
     , ( N "cy", AttValue [ Left $ show y ] )
     , ( N "r", AttValue [ Left $ show r ] )
     , ( N "fill", AttValue [ Left c ] )
     ] []

svgToElem (Image x y w h p)
  = Elem (N "image") [
       ( N "x", AttValue [ Left $ show x ] )
     , ( N "y", AttValue [ Left $ show y ] )
     , ( N "width", AttValue [ Left $ show w ] )
     , ( N "height", AttValue [ Left $ show h ] )
     , ( N "xlink:href", AttValue [ Left p ] )
     ] []

svgToElem (Group trs svgs)
  = Elem (N "g") (
      map (\ tr -> let a = showTrans tr
                    in ( N "transform", AttValue [ Left a ] ) ) trs
     ) $ map (flip CElem () . svgToElem) svgs


svgToXml :: Double -> Double -> [ SVG ] -> Document ()
svgToXml w h svgs
  = Document prlg ent
      (Elem (N "svg") (emAtt w h) $ map (flip CElem () . svgToElem) svgs) els

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
