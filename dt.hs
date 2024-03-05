
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




loadTreeString :: String -> DecisionTree (Int) (Double) String
loadTreeString str = loadDecisionTreeNode str 0


loadDecisionTreeNode :: String -> Int -> DecisionTree (Int) (Double) String
loadDecisionTreeNode string removeCount = loadDecisionTreeSwitch (removeChars string removeCount) removeCount

loadDecisionTreeSwitch :: String -> Int -> DecisionTree (Int) (Double) String
loadDecisionTreeSwitch str removeCount
    | beginsWithNode str = do 
        let (maybeInt, maybeDouble, string) = loadDecisionTreeNodeParameters (removeChars str 5)
        case (maybeInt, maybeDouble) of
            (Just intVal, Just doubleVal) -> Node intVal doubleVal (loadDecisionTreeNode string (removeCount + 2)) (loadDecisionTreeNode string (removeCount + 2))
            _ -> error "Invalid parameters for node"
    | beginsWithLeaf str = loadDecisionTreeLeafParameters (removeChars str 5)
    | otherwise = EmptyTree

loadDecisionTreeLeafParameters :: String -> DecisionTree (Int) (Double) String
loadDecisionTreeLeafParameters str = Leaf str

loadDecisionTreeNodeParameters :: String -> ((Maybe Int), (Maybe Double), String)
loadDecisionTreeNodeParameters str = loadDecisionTreeNodeParameter2 (loadDecisionTreeNodeParameter1 str)

loadDecisionTreeNodeParameter1 :: String -> ((Maybe Int), String)
loadDecisionTreeNodeParameter1 str = ((convertToInt str ","), (removeUntilChar str ',') )

loadDecisionTreeNodeParameter2 :: ((Maybe Int), String) -> ((Maybe Int), (Maybe Double), String)
loadDecisionTreeNodeParameter2 (attributeIndex, str) = 
        (attributeIndex, (convertToDouble str ""), (removeUntilChar str '\n'))

readUntilChar :: String -> Char -> String
readUntilChar "" _ = ""
readUntilChar (begin:rest) a 
    | begin == a = [a]
    | otherwise = begin : readUntilChar rest a

removeUntilChar :: String -> Char -> String
removeUntilChar "" a = ""
removeUntilChar (begin:rest) a
    | begin == a = rest
    | otherwise = removeUntilChar rest a

convertToDouble :: String -> String -> Maybe Double
convertToDouble str endString = case reads str of
    [(x, endString)] -> Just x
    _         -> Nothing

convertToInt :: String -> String -> Maybe Int
convertToInt str endString = case reads str of
    [(x, endString)] -> Just x
    _         -> Nothing

removeChars :: String -> Int -> String
removeChars str 0 = str 
removeChars [] _ = []
removeChars (_:rest) removeCount = removeChars rest (removeCount - 1)

beginsWithNode :: String -> Bool
beginsWithNode str = (length str >= 5) && str !! 0 == 'N' && str !! 1 == 'o' && str !! 2 == 'd' && str !! 3 == 'e' && str !! 4 == ':'

beginsWithLeaf :: String -> Bool
beginsWithLeaf str = (length str >= 5) && str !! 0 == 'L' && str !! 1 == 'e' && str !! 2 == 'a' && str !! 3 == 'f' && str !! 4 == ':'


main :: IO ()
main = do
    --putStrLn (removeUntilChar "aaaa,11" ',')
    let tree :: DecisionTree Int Double String  -- Určení konkrétních typů pro vaši strukturu stromu
        tree = Leaf "a"  -- Vytvoření stromu obsahujícího jediný uzel typu Leaf
    printDecisionTree tree
    
    contents <- readFile "a.txt"
    printDecisionTree (loadTreeString contents)
    --putStrLn (removeChars "aaa" 3)
    contents <- readFile "a.txt"
    putStrLn contents
