
import Text.XML.YJSVG

main = do
	putStrLn $ showSVG 400 350 $ [
		Text (Center 100 100) 25 (ColorName "red") "KochiMincho"
			"(100, 100)",
		Text (Center (- 200) 150) 25 (ColorName "yellow") "KochiMincho"
			"(-200, 150)",
		Text (Center 150 (- 175)) 25 (ColorName "blue") "KochiGothic"
			"(150, -175)"
	 ]
