module Main where

import Text.XML.YJSVG

main :: IO ()
main = do
	putStrLn $ showSVG 600 550 $ [
		Line 10 50 200 50 (ColorName "red") 10,
		Line 10 75 200 75 (RGB 255 0 0) 10,
		Line 10 100 200 100 (ColorName "yellow") 10,
		Line 10 125 200 125 (RGB 255 255 0) 10
	 ]
