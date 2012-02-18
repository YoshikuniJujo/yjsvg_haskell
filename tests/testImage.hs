import System.Environment
import Text.XML.YJSVG

main = do
  [ pngFileName ] <- getArgs
  putStrLn $ showSVG 600 680 [
     Image 20 200 400 300 pngFileName
   , Text 80 300 40 "blue" "The sea"
   , Text 90 380 40 "blue" "&lt;')))&gt;&lt;"
   , Text 200 430 40 "blue" "ichtus"
   , Rect 100 50 100 100 3 "purple" "none"
   ]
