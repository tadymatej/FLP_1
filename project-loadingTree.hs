
import Data.List (sortBy)
import Data.Ord (comparing)
import System.Environment

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
    | begin == a = (rest)
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

decisionTree_clasifyAll :: DecisionTree Int Double String -> [[Double]] -> Int -> [String]
decisionTree_clasifyAll EmptyTree _ _ = []
decisionTree_clasifyAll _ [[]] _ = []
decisionTree_clasifyAll (Leaf className) _ _ = [className]
decisionTree_clasifyAll tree inputMatrix processingIndex 
    | length(inputMatrix) <= processingIndex = []
    | otherwise = decisionTree_clasifyAllHelper tree inputMatrix processingIndex

decisionTree_clasifyAllHelper :: DecisionTree Int Double String -> [[Double]] -> Int -> [String]
decisionTree_clasifyAllHelper tree inputMatrix processingIndex
    = (decisionTree_clasify tree (inputMatrix !! processingIndex)) : (decisionTree_clasifyAll tree inputMatrix (processingIndex + 1))

decisionTree_clasify :: DecisionTree Int Double String -> [Double] -> String
decisionTree_clasify (Leaf className) arr = className
decisionTree_clasify EmptyTree arr = ""
decisionTree_clasify (Node aI aT (left) (right)) arr 
    | (arr !! aI) > aT = decisionTree_clasify right arr
    | otherwise = decisionTree_clasify left arr


-------- End of clasifying ---------
-----------------------------------------------





------- Start of data reading ----------
----------------------------------------

convertToGridOfDoubles :: String -> Char -> Char -> [[Double]]
convertToGridOfDoubles "" _ _ = [[]]
convertToGridOfDoubles (a: rest) endChar endChar2 = do
    let (row, continue, remainingStr) = convertToDoubles (a : rest) endChar endChar2
    convertToGridOfDoublesContinue remainingStr endChar endChar2 row continue


convertToGridOfDoublesContinue :: String -> Char -> Char -> [Double] -> Int -> [[Double]]
convertToGridOfDoublesContinue str endChar endChar2 res continue 
    | continue == 1 = res : convertToGridOfDoubles str endChar endChar2
    | otherwise = [res]

convertToDoubles :: String -> Char -> Char -> ([Double], Int, String)
convertToDoubles "" _ _ = ([], 0, "")
convertToDoubles (a: rest) endChar endChar2 
    | a == endChar2 = do
        ([], 1, (rest))
    | otherwise = case reads (a:rest) of
    [(x, rest)] -> do 
        let (res, val, remainingStr) = convertToDoubles (dropWhile (== endChar) rest) endChar endChar2
        ((x : res), val, remainingStr)
    _           -> do 
        let (res, val, remainingStr) = convertToDoubles "" endChar endChar2
        ((0 : res), val, remainingStr)

---------- End of data reading -----------
-----------------------------------------

-----------------------------------------------------------------------------------------------------------
------------------------------------- TASK 2 - Training of decision tree ----------------------------------
-----------------------------------------------------------------------------------------------------------


printList :: [([Double], String)] -> IO ()
printList [] = putStrLn "Empty list"
printList ((nums, str):rest) = do
  putStrLn $ show nums ++ ", " ++ str
  printList rest


convertToGridOfTrainingData :: String -> Char -> Char -> [([Double], String)]
convertToGridOfTrainingData (a: rest) endChar endChar2 = do
    let ((row, classs), continue, remainingStr) = convertToTrainingData (a : rest) endChar endChar2
    convertToGridOfTrainingDataContinue remainingStr endChar endChar2 (row, classs) continue


convertToGridOfTrainingDataContinue :: String -> Char -> Char -> ([Double], String) -> Int -> [([Double], String)]
convertToGridOfTrainingDataContinue "" endChar endChar2 res continue = [res]
convertToGridOfTrainingDataContinue (a : str) endChar endChar2 res continue 
     = res : convertToGridOfTrainingData (a:str) endChar endChar2

convertToTrainingData :: String -> Char -> Char -> (([Double], String), Int, String)
convertToTrainingData (a: rest) endChar endChar2 =
    case reads (a:rest) of
    [(x, rest)] -> do 
        let ((res, classs), val, remainingStr) = convertToTrainingData (dropWhile (== endChar) rest) endChar endChar2
        (((x : res), classs), val, remainingStr)
    _           -> do 
        (([], readUntilChar (a : rest) '\n'), 1, removeUntilChar rest '\n')


splitData :: [([Double], String)] -> Int -> (Int, Double) -> ([([Double], String)], [([Double], String)])
splitData inputData processingIndex (aI, aT)
    | length(inputData) == (processingIndex + 1) = do 
        let (rowData, className) = inputData !! processingIndex
        if ((rowData !! aI) <= aT)
            then ([inputData !! processingIndex], [])
            else ([], [inputData !! processingIndex])
    | otherwise = do 
        let (leftData, rightData) = splitData inputData (processingIndex + 1) (aI, aT)
        let (rowData, className) = inputData !! processingIndex
        if ((rowData !! aI) <= aT)
            then (inputData !! processingIndex : leftData, rightData)
            else (leftData, inputData !! processingIndex : rightData)

------------------------ Learning decision tree ----------------------------

addToUniqArray :: [String] -> String -> [String]
addToUniqArray [] item = [item]
addToUniqArray (firstArrayItem:arrayRest) item 
    | firstArrayItem == item = (firstArrayItem:arrayRest)
    | otherwise = firstArrayItem : addToUniqArray arrayRest item

findIndexOf :: [String] -> String -> Int 
findIndexOf array findItem = findIndexOfHelper array findItem 0

findIndexOfHelper :: [String] -> String -> Int -> Int
findIndexOfHelper [] findItem _ = -1
findIndexOfHelper (firstItem:arrayRest) findItem position 
    | firstItem == findItem = position
    | otherwise = findIndexOfHelper arrayRest findItem (position + 1) 


arrayPush :: [Int] -> Int -> [Int]
arrayPush [] item = [item]
arrayPush (firstItem:arrayRest) item = firstItem : arrayPush arrayRest item

setValueAt :: [Int] -> Int -> Int -> [Int]
setValueAt (firstItem : arrayRest) value pos 
    | pos == 0 = value : arrayRest
    | otherwise = firstItem : setValueAt arrayRest value (pos - 1)

initBuckets :: [([Double], String)] -> Int -> ([Int], [Int], [String])
initBuckets [] _ = ([], [], [])
initBuckets inputData processingIndex
    | length(inputData) <= (processingIndex) = ([], [], [])
    | otherwise = do 
    let (leftBucket, rightBucket, classes) = initBuckets inputData (processingIndex + 1)
    let (inputDataRow, className) = inputData !! processingIndex
    let oldClassesLen = length(classes)
    let newClasses = addToUniqArray classes className
    let newClassesLen = length(newClasses)
    if(newClassesLen == oldClassesLen)
        then do 
            let classIndex = findIndexOf newClasses className
            let rightBucketNewValue = (rightBucket !! classIndex + 1)
            let rightBucketNew = setValueAt rightBucket rightBucketNewValue classIndex
            (leftBucket, rightBucketNew, newClasses)
        else do 
            let leftBucketNew = arrayPush leftBucket 0
            let rightBucketNew = arrayPush rightBucket 1
            (leftBucketNew, rightBucketNew, newClasses)
    


giniScore :: [Int] -> Int -> Double
giniScore bucket classID = do 
    let sumInBucket = fromIntegral (sum bucket) :: Double
    let classCount = fromIntegral (bucket !! classID) :: Double
    1.0 - ((classCount / sumInBucket) ^ 2)

calcGini :: ([Int], Double) -> ([Int], Double) -> Int -> Double
calcGini (leftBucket, giniLeft) (rightBucket, giniRight) inputDataLen = do 
    let inputDataLenDouble = fromIntegral inputDataLen :: Double
    let sumLeftBucketDouble = fromIntegral (sum leftBucket) :: Double
    let sumRightBucketDouble = fromIntegral (sum rightBucket) :: Double
    ((sumLeftBucketDouble * giniLeft) + (sumRightBucketDouble * giniRight) / inputDataLenDouble)


learnDecisionTree :: [([Double], String)] -> DecisionTree Int Double String 
learnDecisionTree [] = EmptyTree
learnDecisionTree (([], ""):rest) = EmptyTree
learnDecisionTree inputData = do 
    let (leftBucket, rightBucket, classes) = initBuckets inputData 0
    if ((length(classes)) == 1)
        then Leaf (classes !! 0)
        else do
            let (firstRow, firstClassName) = inputData !! 0
            let attributesCount = length(firstRow)
            let (bestGini, bestAttributeIndex, bestAttributeThreshold) = _learnDecisionTreeProcessAttribute inputData 0 attributesCount (leftBucket, rightBucket, classes) (1.0, -1, 0.0)
            let (dataLeft, dataRight) = splitData inputData 0 (bestAttributeIndex, bestAttributeThreshold)
        
            Node bestAttributeIndex bestAttributeThreshold (learnDecisionTree dataLeft) (learnDecisionTree dataRight)


_learnDecisionTreeProcessAttribute :: [([Double], String)] -> Int -> Int -> ([Int], [Int], [String]) -> (Double, Int, Double) -> (Double, Int, Double)
_learnDecisionTreeProcessAttribute inputData processingAttributeIndex attributesCount (leftBucket, rightBucket, classes) (bestGini, bestAttributeIndex, bestAttributeThreshold)
    | processingAttributeIndex >= attributesCount = (bestGini, bestAttributeIndex, bestAttributeThreshold)
    | otherwise = do
        let sortedTrainingData = sortBy (trainingDataSortFnc processingAttributeIndex) inputData
        let (newBestGini, newBestAttributeIndex, newBestAttributeThreshold) = _learnDecisionTree sortedTrainingData 0 (leftBucket, rightBucket, classes) processingAttributeIndex (bestGini, bestAttributeIndex, bestAttributeThreshold)
        if (newBestGini < bestGini)
              then _learnDecisionTreeProcessAttribute inputData (processingAttributeIndex + 1) attributesCount (leftBucket, rightBucket, classes) (newBestGini, newBestAttributeIndex, newBestAttributeThreshold)
              else _learnDecisionTreeProcessAttribute inputData (processingAttributeIndex + 1) attributesCount (leftBucket, rightBucket, classes) (bestGini, bestAttributeIndex, bestAttributeThreshold)


_learnDecisionTree :: [([Double], String)] -> Int -> ([Int], [Int], [String]) -> Int -> (Double, Int, Double) -> (Double, Int, Double)
_learnDecisionTree inputData processingIndex (leftBucket, rightBucket, classes) attributeIndex (bestGini, bestAttributeIndex, bestAttributeThreshold)
    | (length(inputData)) <= processingIndex =  (bestGini, bestAttributeIndex, bestAttributeThreshold)
    | processingIndex == 0 = do
        let (row, className) = inputData !! processingIndex
        let classIndex = findIndexOf classes className
        let newLeftBucketValue = (leftBucket !! classIndex) + 1
        let newLeftBucket = setValueAt leftBucket newLeftBucketValue classIndex

        let newRightBucketValue = (rightBucket !! classIndex) - 1
        let newRightBucket = setValueAt rightBucket newRightBucketValue classIndex

        let giniLeft = giniScore newLeftBucket classIndex
        let giniRight = giniScore newRightBucket classIndex
        let newGini = calcGini (newLeftBucket, giniLeft) (newRightBucket, giniRight) (length(inputData))
        if(newGini < bestGini)
            then do
            let attributeThreshold = ((row !! attributeIndex))
            _learnDecisionTree inputData (processingIndex + 1) (leftBucket, rightBucket, classes) attributeIndex (newGini, attributeIndex, attributeThreshold)
            else _learnDecisionTree inputData (processingIndex + 1) (leftBucket, rightBucket, classes) attributeIndex (bestGini, bestAttributeIndex, bestAttributeThreshold)
    | otherwise = do 
        let (row, className) = inputData !! processingIndex
        let classIndex = findIndexOf classes className
        let newLeftBucketValue = (leftBucket !! classIndex) + 1
        let newLeftBucket = setValueAt leftBucket newLeftBucketValue classIndex

        let newRightBucketValue = (rightBucket !! classIndex) - 1
        let newRightBucket = setValueAt rightBucket newRightBucketValue classIndex

        let giniLeft = giniScore newLeftBucket classIndex
        let giniRight = giniScore newRightBucket classIndex
        let newGini = calcGini (newLeftBucket, giniLeft) (newRightBucket, giniRight) (length(inputData))
        let (prevRow, prevClassName) = inputData !! (processingIndex - 1)
        if((row !! attributeIndex) == (prevRow !! attributeIndex))
            then _learnDecisionTree inputData (processingIndex + 1) (leftBucket, rightBucket, classes) attributeIndex (bestGini, bestAttributeIndex, bestAttributeThreshold)
            else do
                if(newGini < bestGini)
                    then do
                    let attributeThreshold = ((row !! attributeIndex) + (prevRow !! attributeIndex)) / 2
                    _learnDecisionTree inputData (processingIndex + 1) (leftBucket, rightBucket, classes) attributeIndex (newGini, attributeIndex, attributeThreshold)
                    else _learnDecisionTree inputData (processingIndex + 1) (leftBucket, rightBucket, classes) attributeIndex (bestGini, bestAttributeIndex, bestAttributeThreshold)



trainingDataSortFnc :: Int -> ([Double], String) -> ([Double], String) -> Ordering
trainingDataSortFnc index1D (data1, className1) (data2, className2) = 
    compare (data1 !! index1D) (data2 !! index1D)


task1 :: String -> String -> [String]
task1 treeFileContent dataFileContent = do
    let (tree, str) = (loadTreeString treeFileContent 0 0)

    let grid = (convertToGridOfDoubles dataFileContent ',' '\n')

    let result = decisionTree_clasifyAll tree grid 0
    result

task2 :: String -> DecisionTree Int Double String
task2 trainingDataFileContent = do 
    let trainingData = (convertToGridOfTrainingData trainingDataFileContent ',' '\n')

    let learnedDecisionTree = learnDecisionTree trainingData
    learnedDecisionTree

main :: IO ()
main = do
    (first: args) <- getArgs
    case first of 
        "-1"  -> do 
            let argsLen = length(args)
            if (argsLen < 2) 
                then putStrLn "Chybi vstupni soubory!!!"
                else do
                    let fileDecisionTree = args !! 0
                    let fileClasifyData = args !! 1
                    fileDecisionTreeContents <- readFile fileDecisionTree
                    fileClasifyDataContents <- readFile fileClasifyData
                    let result = task1 fileDecisionTreeContents fileClasifyDataContents
                    putStrLn (show result)
        "-2" -> do 
            let argsLen = length(args)
            if (argsLen < 1) 
                then putStrLn "Chybi vstupni soubor!!!"
                else do 
                    let fileTrainingData = args !! 0
                    fileTrainingDataContents <- readFile fileTrainingData
                    let learnedDecisionTree = task2 fileTrainingDataContents
                    printDecisionTree learnedDecisionTree
        _ -> do 
            putStrLn("Usage: flp-fun -1 <soubor obsahujici strom> <soubor obsahujici nove data> | flp-fun -2 <soubor obsahujici trenovaci data>")

    -- contents <- readFile "tree.txt"
    -- let (tree, str) = (loadTreeString contents 0 0)
    -- printDecisionTree tree

    -- contents <- readFile "clasifyData.txt"
    -- let grid = (convertToGridOfDoubles contents ',' '\n')
    -- printGrid grid

    -- let result = decisionTree_clasifyAll tree grid 0;

    -- putStrLn (show result)

    -- contents <- readFile "trainData.txt"
    -- let trainingData = (convertToGridOfTrainingData contents ',' '\n')

    -- printList(trainingData)

    -- let (leftData, rightData) = splitData trainingData 0 (0, 1.2)
    -- putStrLn "---------"
    -- printList (leftData)
    -- putStrLn "---------"
    -- printList (rightData)
    -- putStrLn "---------"

    -- let (leftBucket, rightBucket, classes) = initBuckets trainingData 0
    -- print leftBucket
    -- print rightBucket
    -- print classes

    -- let learnedDecisionTree = learnDecisionTree trainingData
    -- printDecisionTree learnedDecisionTree

    -- let grid = ([4,5,6] : [[1, 2, 3]])
    -- printGrid grid
