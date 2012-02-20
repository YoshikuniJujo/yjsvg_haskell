
import Text.XML.YJSVG

main = do
	putStrLn $ showSVG 600 550 $ [
		Text 10 50 25 (ColorName "green") "KochiGothic" "今日は",
		Text 10 90 25 (ColorName "green") "KochiMincho" "今日は"
	 ]
