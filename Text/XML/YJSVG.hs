module Text.XML.YJSVG (
  showSVG
, SVG(..)
, Transform(..)
) where

import Text.XML.HaXml
import Text.XML.HaXml.Pretty

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

-- instance Show [ SVG ] where
--  show = showSVG

showSVG :: Double -> Double -> [ SVG ] -> String
showSVG w h = show . document . svgToXml w h

svgToElem :: SVG -> Element ()
svgToElem (Line x1 y1 x2 y2 color lineWidth)
  = Elem "line" [
       ( "x1", AttValue [ Left $ show x1 ] )
     , ( "y1", AttValue [ Left $ show y1 ] )
     , ( "x2", AttValue [ Left $ show x2 ] )
     , ( "y2", AttValue [ Left $ show y2 ] )
     , ( "stroke", AttValue [ Left color ] )
     , ( "stroke-width", AttValue [ Left $ show lineWidth ] )
     ] []

svgToElem (Polyline points fillColor lineColor lineWidth)
  = Elem "polyline" [
       ( "points", AttValue [ Left $ pointsToAttVal points ] )
     , ( "fill"  , AttValue [ Left fillColor ] )
     , ( "stroke", AttValue [ Left lineColor ] )
     , ( "stroke-width", AttValue [ Left $ show lineWidth ] )
     ] []
  where
  pointsToAttVal :: [ ( Double, Double ) ] -> String
  pointsToAttVal [] = ""
  pointsToAttVal ( (x, y):ps )
    = show x ++ "," ++ show y ++ " " ++ pointsToAttVal ps

svgToElem (Rect x y w h sw cf cs)
  = Elem "rect" [
   ( "x", AttValue [ Left $ show x ] )
 , ( "y", AttValue [ Left $ show y ] )
 , ( "width", AttValue [ Left $ show w ] )
 , ( "height", AttValue [ Left $ show h ] )
 , ( "stroke-width", AttValue [ Left $ show sw ] )
 , ( "fill", AttValue [ Left cf ] )
 , ( "stroke", AttValue [ Left cs ] )
 ] []

svgToElem (Text x y s c t)
  = Elem "text" [
   ( "x", AttValue [ Left $ show x ] )
 , ( "y", AttValue [ Left $ show y ] )
 , ( "font-size", AttValue [ Left $ show s ] )
 , ( "fill", AttValue [ Left c ] )
 ] [ CString False t () ]

svgToElem (Circle x y r c)
  = Elem "circle" [
       ( "cx", AttValue [ Left $ show x ] )
     , ( "cy", AttValue [ Left $ show y ] )
     , ( "r", AttValue [ Left $ show r ] )
     , ( "fill", AttValue [ Left c ] )
     ] []

svgToElem (Image x y w h p)
  = Elem "image" [
       ( "x", AttValue [ Left $ show x ] )
     , ( "y", AttValue [ Left $ show y ] )
     , ( "width", AttValue [ Left $ show w ] )
     , ( "height", AttValue [ Left $ show h ] )
     , ( "xlink:href", AttValue [ Left p ] )
     ] []

svgToElem (Group trs svgs)
  = Elem "g" (
      map (\ tr -> let a = showTrans tr
                    in ( "transform", AttValue [ Left $ a ] ) ) trs
     ) $ map (flip CElem () . svgToElem) svgs


svgToXml :: Double -> Double -> [ SVG ] -> Document ()
svgToXml w h svgs
  = Document prlg ent
      (Elem "svg" (emAtt w h) $ map (flip CElem () . svgToElem) svgs) els

pmsc    = [ Comment " MADE BY SVG.HS " ]
pblit   = "-//W3C//DTD SVG 1.1//EN"
sslit   = "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"
doctype = DTD "svg" (Just (PUBLIC (PubidLiteral pblit) (SystemLiteral sslit))) []
xmldcl  = XMLDecl "1.0" (Just (EncodingDecl "UTF-8")) Nothing
prlg    = Prolog (Just xmldcl) pmsc (Just doctype) []

xmlns   = "http://www.w3.org/2000/svg"
ver     = "1.1"
xlink   = "http://www.w3.org/1999/xlink"
width   = "400"
height  = "640"
emAtt  w h = [
   ( "xmlns", AttValue [ Left xmlns ] )
 , ( "version", AttValue [ Left ver ] )
 , ( "xmlns:xlink", AttValue [ Left xlink ] )
 , ( "width", AttValue [ Left $ show w ] )
 , ( "height", AttValue [ Left $ show h ] )
 ]
em ougon= Elem "svg" (emAtt 100 200) [ CElem rect (), CElem (text ougon) () ]
rect = Elem "rect" [
   ( "x", AttValue [ Left "20" ] )
 , ( "y", AttValue [ Left "30" ] )
 , ( "width", AttValue [ Left "80" ] )
 , ( "height", AttValue [ Left "100" ] )
 , ( "fill", AttValue [ Left "red" ] )
 , ( "stroke", AttValue [ Left "none" ] )
 ] []
text ougon = Elem "text" [
   ( "x", AttValue [ Left "20" ] )
 , ( "y", AttValue [ Left "50" ] )
 , ( "font-size", AttValue [ Left "20" ] )
 ] [ CString False ougon () ]

ent     = []
els     = []
-- els     = [ Comment "MADE BY SIMPLE.HS" ]

docu ougon = Document prlg ent (em ougon) els

{-
svgMain = do
  ougonXml <- readFile "ougon.xml"
  let Document _ _ (Elem "text" _ [ CString _ ougon _ ]) _
        = xmlParse "ougon.xml" ougonXml
  print $ document $ svgToXml 100 200 [ Rect 20 30 80 90 "black" "purple", Text 40 50 30 "underline" "purple" "good" ]
  -}
