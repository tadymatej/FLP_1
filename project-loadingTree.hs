
data DecisionTree attributeIndex attributeThreshold className 
    = EmptyTree
    | Node attributeIndex attributeThreshold (DecisionTree attributeIndex attributeThreshold className ) (DecisionTree attributeIndex attributeThreshold className )
    | Leaf className
    deriving (Show)


printDecisionTree :: (Show attributeIndex, Show attributeThreshold, Show className) => DecisionTree attributeIndex attributeThreshold className -> IO ()
printDecisionTree EmptyTree = putStrLn "Empty tree"
printDecisionTree (Node index threshold left right) = do
    putStrLn $ "Node: Index = " ++ show index ++ ", Threshold = " ++ show threshold
    putStrLn "Left subtree:"
    printDecisionTree left
    putStrLn "Right subtree:"
    printDecisionTree right
printDecisionTree (Leaf className) = putStrLn $ "Leaf: Class = " ++ show className


------------------------------------------------
--- Start of loading tree ------

readUntilChar :: String -> Char -> String
readUntilChar "" _ = ""
readUntilChar (begin:rest) a 
    | begin == a = []
    | otherwise = begin : readUntilChar rest a

removeUntilChar :: String -> Char -> String
removeUntilChar "" a = ""
removeUntilChar (begin:rest) a
    | begin == a = rest
    | otherwise = removeUntilChar rest a

convertToDouble :: String -> String -> Double
convertToDouble str endString = case reads str of
    [(x, endString)] -> x
    _         -> 0

convertToInt :: String -> String -> Int
convertToInt str endString = case reads str of
    [(x, endString)] -> x
    _         -> 0

removeChars :: String -> Int -> String
removeChars str 0 = str 
removeChars [] _ = []
removeChars (_:rest) removeCount = removeChars rest (removeCount - 1)


loadTreeString :: String -> Int -> Int -> (DecisionTree Int Double String, String)
-- everything skipped, now process it!!
loadTreeString str 0 removeSpaceCopy
    | beginsWithNode str = do 
        let str2 = drop 5 str
        let attributeIndex = convertToInt str2 ","
        let str3 = removeUntilChar str2 ','
        let attributeThreshold = convertToDouble str3 "\n"
        let str4 = removeUntilChar str3 '\n'
        let (left, remainingStr) = loadTreeString str4 (removeSpaceCopy + 2) (removeSpaceCopy + 2)
        let (right, remainingStr2) = loadTreeString remainingStr (removeSpaceCopy + 2) (removeSpaceCopy + 2)
        (Node attributeIndex attributeThreshold (left) (right), remainingStr2)
    | beginsWithLeaf str = (Leaf (readUntilChar (removeChars str 6) '\n'), (removeUntilChar str '\n'))
    | otherwise = (EmptyTree, str)
loadTreeString (b:rest) a removeSpaceCopy = loadTreeString rest (a - 1) removeSpaceCopy

beginsWithNode :: String -> Bool
beginsWithNode str = (length str >= 6) && str !! 0 == 'N' && str !! 1 == 'o' && str !! 2 == 'd' && str !! 3 == 'e' && str !! 4 == ':' && str !! 5 == ' '

beginsWithLeaf :: String -> Bool
beginsWithLeaf str = (length str >= 6) && str !! 0 == 'L' && str !! 1 == 'e' && str !! 2 == 'a' && str !! 3 == 'f' && str !! 4 == ':' && str !! 5 == ' '



--- End of loading tree ------
---------------------------------------------





----------------------------------------------
---- Start of clasifying ----------

--decisionTree_clasify :: DecisionTree -> String

convertToDoubles2 :: String -> Char -> Char -> [[Double]]
convertToDoubles2 "" _ _ = [] : [[]]
convertToDoubles2 (a: rest) endChar endChar2 
    | a == endChar2 = do
        -- now process a new data
        -- here will be decisionTree processing

        let res = (convertToDoubles2 rest endChar endChar2)
        [] : res
    | otherwise = case reads (a:rest) of
    [(x, rest)] -> do 
        let res = convertToDoubles2 (dropWhile (== endChar) rest) endChar endChar2
        let lastItem = last res
        let headItems = init res
        (x : lastItem) : headItems
    _           -> do 
        let res = convertToDoubles2 "" endChar endChar2
        let lastItem = last res
        let headItems = init res
        (0 : lastItem) : headItems



-------- End of clasifying ---------
-----------------------------------------------





------- Start of data reading ----------
----------------------------------------




---------- End of data reading -----------
-----------------------------------------


printGrid :: [[Double]] -> IO ()
printGrid [] = putStrLn ""  -- Pokud je pole prázdné, vypíšeme prázdný řádek
printGrid (row:rows) = do   -- Výpis každého řádku pole
  printRow row              -- Vypíšeme aktuální řádek
  printGrid rows            -- Rekurzivně zavoláme printGrid pro zbytek pole

-- Funkce pro výpis řádku pole
printRow :: [Double] -> IO ()
printRow [] = putStrLn ""           -- Pokud je řádek prázdný, vypíšeme prázdný řádek
printRow (x:xs) = do                -- Výpis každého prvku řádku
  putStr (show x ++ " ")            -- Vypíšeme aktuální prvek s mezerou
  printRow xs  


main :: IO ()
main = do
    
    contents <- readFile "a.txt"
    let (tree, str) = (loadTreeString contents 0 0)
    printDecisionTree tree

    contents <- readFile "b.txt"
    let grid = (convertToDoubles2 contents ',' '\n')
    printGrid grid

    --let grid = ([4,5,6] : [[1, 2, 3]])
    --printGrid grid
