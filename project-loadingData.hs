import Control.Monad.IO.Class (liftIO)


convertToDoubles :: String -> Char -> [Maybe Double]
convertToDoubles "" _ = []
convertToDoubles str endChar = case reads str of
    [(x, rest)] -> Just x : convertToDoubles (dropWhile (== endChar) rest) endChar
    _           -> Nothing : convertToDoubles "" endChar


myPrint :: [Maybe Double] -> IO ()
myPrint message = print message

-- Druhý string jen testuje, zda se to provolává v očekávaném pořadí
-- Vypadá to, že jo..
convertToDoubles2 :: String -> Char -> Char -> ([Maybe Double], String)
convertToDoubles2 "" _ _ = ([], "")
convertToDoubles2 (a: rest) endChar endChar2 
    | a == endChar2 = do
        -- now process a new data
        -- here will be decisionTree processing

        let (res, string) = (convertToDoubles2 rest endChar endChar2)
        ([], ("||||" ++ string))
    | otherwise = case reads (a:rest) of
    [(x, rest)] -> do 
        let (res, string) = convertToDoubles2 (dropWhile (== endChar) rest) endChar endChar2
        (Just x : res, ((show x) ++ "," ++ string))
    _           -> do 
        let (res, string) = convertToDoubles2 "" endChar endChar2
        (Nothing : res, (string))


removeUntilChar :: String -> Char -> String
removeUntilChar "" a = ""
removeUntilChar (begin:rest) a
    | begin == a = rest
    | otherwise = removeUntilChar rest a



loadDataString :: String -> String 
loadDataString str = str

main :: IO ()
main = do
    
    contents <- readFile "b.txt"
    let str = (convertToDoubles2 contents ',' '\n')
    print str
