
import Data.List (sortBy, maximumBy)
import System.Environment
import Data.Ord (comparing)

import Text.Parsec
import Text.Parsec.String (Parser)

data DecisionTree attributeIndex attributeThreshold className 
    = EmptyTree
    | Node attributeIndex attributeThreshold (DecisionTree attributeIndex attributeThreshold className ) (DecisionTree attributeIndex attributeThreshold className )
    | Leaf className
    deriving (Show)

data DecisionTreeNewData 
    = DecisionTreeNewData [Double] 
    deriving (Show)

data DecisionTreeTrainData 
    = DecisionTreeTrainData DecisionTreeNewData String 
    deriving (Show)

------------------------------------------------
--- Start of loading tree ------

readUntilChar :: String -> Char -> String
readUntilChar "" _ = ""
readUntilChar (begin:rest) a 
    | begin == a = []
    | otherwise = begin : readUntilChar rest a

removeUntilChar :: String -> Char -> String
removeUntilChar "" _a = ""
removeUntilChar (begin:rest) a
    | begin == a = (rest)
    | otherwise = removeUntilChar rest a

convertToDouble :: String -> String -> Double
convertToDouble str _endString = case reads str of
    [(x, _notRead)] -> x
    _         -> 0

convertToInt :: String -> String -> Int
convertToInt str _endString = case reads str of
    [(x, _notRead)] -> x
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
loadTreeString (_b:rest) a removeSpaceCopy = loadTreeString rest (a - 1) removeSpaceCopy
loadTreeString "" _index _removeSpaceCopy = (EmptyTree, "")

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
--decisionTree_clasifyAll (Leaf className) _ _ = [className]
decisionTree_clasifyAll tree inputMatrix processingIndex 
    | length(inputMatrix) <= processingIndex = []
    | (inputMatrix !! processingIndex) == [] = []
    | otherwise = decisionTree_clasifyAllHelper tree inputMatrix processingIndex

decisionTree_clasifyAllHelper :: DecisionTree Int Double String -> [[Double]] -> Int -> [String]
decisionTree_clasifyAllHelper tree inputMatrix processingIndex
    = (decisionTree_clasify tree (inputMatrix !! processingIndex)) : (decisionTree_clasifyAll tree inputMatrix (processingIndex + 1))

decisionTree_clasify :: DecisionTree Int Double String -> [Double] -> String
decisionTree_clasify (Leaf className) _arr = className
decisionTree_clasify EmptyTree _arr = ""
decisionTree_clasify (Node aI aT (left) (right)) arr 
    | (arr !! aI) <= aT = decisionTree_clasify left arr
    | otherwise = decisionTree_clasify right arr


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
    [(x, notRead)] -> do 
        let (res, val, remainingStr) = convertToDoubles (dropWhile (== endChar) notRead) endChar endChar2
        ((x : res), val, remainingStr)
    _           -> do 
        let (res, val, remainingStr) = convertToDoubles "" endChar endChar2
        ((0 : res), val, remainingStr)

---------- End of data reading -----------
-----------------------------------------

-----------------------------------------------------------------------------------------------------------
------------------------------------- TASK 2 - Training of decision tree ----------------------------------
-----------------------------------------------------------------------------------------------------------


convertToGridOfTrainingData :: String -> Char -> Char -> [([Double], String)]
convertToGridOfTrainingData (a: rest) endChar endChar2 = do
    let ((row, classs), remainingStr) = convertToTrainingData (a : rest) endChar endChar2
    convertToGridOfTrainingDataContinue remainingStr endChar endChar2 (row, classs)
convertToGridOfTrainingData [] _endChar _endChar2 = []

convertToGridOfTrainingDataContinue :: String -> Char -> Char -> ([Double], String) -> [([Double], String)]
convertToGridOfTrainingDataContinue "" _endChar _endChar2 res = [res]
convertToGridOfTrainingDataContinue (a : str) endChar endChar2 res
     = res : convertToGridOfTrainingData (a:str) endChar endChar2

convertToTrainingData :: String -> Char -> Char -> (([Double], String), String)
convertToTrainingData (a: rest) endChar endChar2 =
    case reads (a:rest) of
    [(x, notRead)] -> do 
        let ((res, classs), remainingStr) = convertToTrainingData (dropWhile (== endChar) notRead) endChar endChar2
        (((x : res), classs), remainingStr)
    _           -> do 
        (([], readUntilChar (a : rest) '\n'), removeUntilChar rest '\n')
convertToTrainingData [] _endChar _endChar2 = (([], ""), "")

splitData :: [([Double], String)] -> Int -> (Int, Double) -> ([([Double], String)], [([Double], String)])
splitData inputData processingIndex (aI, aT)
    | length(inputData) == (processingIndex + 1) = do 
        let (rowData, _className) = inputData !! processingIndex
        if ((rowData !! aI) <= aT)
            then ([inputData !! processingIndex], [])
            else ([], [inputData !! processingIndex])
    | otherwise = do 
        let (leftData, rightData) = splitData inputData (processingIndex + 1) (aI, aT)
        let (rowData, _className) = inputData !! processingIndex
        if ((rowData !! aI) <= aT)
            then ((inputData !! processingIndex) : leftData, rightData)
            else (leftData, (inputData !! processingIndex) : rightData)

------------------------ Learning decision tree ----------------------------

addToUniqArray :: [String] -> String -> [String]
addToUniqArray [] item = [item]
addToUniqArray (firstArrayItem:arrayRest) item 
    | firstArrayItem == item = (firstArrayItem:arrayRest)
    | otherwise = firstArrayItem : addToUniqArray arrayRest item

findIndexOf :: [String] -> String -> Int 
findIndexOf array findItem = findIndexOfHelper array findItem 0

findIndexOfHelper :: [String] -> String -> Int -> Int
findIndexOfHelper [] _findItem _ = -1
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
setValueAt [] _value _pos = []


initBuckets :: [([Double], String)] -> Int -> ([Int], [Int], [String])
initBuckets [] _ = ([], [], [])
initBuckets inputData processingIndex
    | length(inputData) <= (processingIndex) = ([], [], [])
    | otherwise = do 
    let (leftBucket, rightBucket, classes) = initBuckets inputData (processingIndex + 1)
    let (_inputDataRow, className) = inputData !! processingIndex
    let oldClassesLen = length(classes)
    let newClasses = addToUniqArray classes className
    let newClassesLen = length(newClasses)
    if(newClassesLen == oldClassesLen)
        then do 
            let classIndex = findIndexOf newClasses className
            let rightBucketNewValue = ((rightBucket !! classIndex) + 1)
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
    1.0 - ((classCount / sumInBucket) ** 2)

calcGini :: ([Int], Double) -> ([Int], Double) -> Double
calcGini (leftBucket, giniLeft) (rightBucket, giniRight) = do 
    let sumLeftBucketDouble = fromIntegral (sum leftBucket) :: Double
    let sumRightBucketDouble = fromIntegral (sum rightBucket) :: Double
    let inputDataLen = fromIntegral ((length leftBucket) + (length rightBucket))
    let inputDataLen = 10
    (((sumLeftBucketDouble * giniLeft) + (sumRightBucketDouble * giniRight)) / inputDataLen)

findMaxIndex :: Ord a => [a] -> Int
findMaxIndex [] = -1
findMaxIndex xs = snd $ maximumBy (comparing fst) (zip xs [0..])

learnDecisionTree :: [([Double], String)] -> Int -> DecisionTree Int Double String 
learnDecisionTree inputData maxDepth = do
    let (leftBucket, rightBucket, classes) = initBuckets inputData 0
    __learnDecisionTree inputData maxDepth rightBucket

__learnDecisionTree :: [([Double], String)] -> Int -> [Int] -> DecisionTree Int Double String
__learnDecisionTree [] _ _  = Leaf "aa"
__learnDecisionTree (([], ""): _rest) _ _ = EmptyTree
-- __learnDecisionTree inputData 0 rightBucket = do 
--     let (leftBucket, _rightBucket, classes) = initBuckets inputData 0
--     let maxIndex = findMaxIndex _rightBucket
--     Leaf (classes !! maxIndex)
__learnDecisionTree inputData maxDepth rightBucket = do 
    let (leftBucket, _rightBucket, classes) = initBuckets inputData 0

    let (firstRow, _firstClassName) = inputData !! 0
    let attributesCount = length(firstRow)
    let (bestGini, bestAttributeIndex, bestAttributeThreshold) = _learnDecisionTreeProcessAttribute inputData 0 attributesCount (leftBucket, rightBucket, classes) (10.0, 0, 0.0)
    if(bestGini <= 0.00) 
        then do
            let maxIndex = findMaxIndex _rightBucket
            Leaf (classes !! maxIndex)
        else do
            let (dataLeft, dataRight) = splitData inputData 0 (bestAttributeIndex, bestAttributeThreshold)
            if dataRight == [] then do
                let maxIndex = findMaxIndex _rightBucket
                Leaf (classes !! maxIndex)
            else do
                Node bestAttributeIndex bestAttributeThreshold (__learnDecisionTree dataLeft (maxDepth - 1) rightBucket) (__learnDecisionTree dataRight (maxDepth - 1) rightBucket)


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

        let giniRight = giniScore rightBucket classIndex
        let newGini = giniRight
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
        let newGini = calcGini (newLeftBucket, giniLeft) (newRightBucket, giniRight)
        let (prevRow, _prevClassName) = inputData !! (processingIndex - 1)
        if((row !! attributeIndex) == (prevRow !! attributeIndex))
            then _learnDecisionTree inputData (processingIndex + 1) (leftBucket, rightBucket, classes) attributeIndex (bestGini, bestAttributeIndex, bestAttributeThreshold)
            else do
                if(newGini < bestGini)
                    then do
                    let attributeThreshold = ((row !! attributeIndex) + (prevRow !! attributeIndex)) / 2
                    _learnDecisionTree inputData (processingIndex + 1) (leftBucket, rightBucket, classes) attributeIndex (newGini, attributeIndex, attributeThreshold)
                    else 
                        -- if(newGini == bestGini && bestAttributeIndex == attributeIndex)
                        --     then do
                        --         let attributeThreshold = ((row !! attributeIndex) + bestAttributeThreshold) / 2
                        --         _learnDecisionTree inputData (processingIndex + 1) (leftBucket, rightBucket, classes) attributeIndex (newGini, attributeIndex, attributeThreshold)
                        --    else do 
                        _learnDecisionTree inputData (processingIndex + 1) (leftBucket, rightBucket, classes) attributeIndex (bestGini, bestAttributeIndex, bestAttributeThreshold)



trainingDataSortFnc :: Int -> ([Double], String) -> ([Double], String) -> Ordering
trainingDataSortFnc index1D (data1, _className1) (data2, _className2) = 
    compare (data1 !! index1D) (data2 !! index1D)


task1 :: String -> String -> [String]
task1 treeFileContent dataFileContent = do
    let (tree, _str) = (loadTreeString treeFileContent 0 0)

    let grid = (convertToGridOfDoubles dataFileContent ',' '\n')

    let result = decisionTree_clasifyAll tree grid 0
    result

task1Print :: [String] -> IO()
task1Print [] = putStr("")
task1Print (item: []) = putStrLn(item)
task1Print (item : rest) = do 
    putStrLn(item) 
    task1Print rest

logBase2 :: Double -> Double
logBase2 x = log x / log 2

task2 :: String -> DecisionTree Int Double String
task2 trainingDataFileContent = do 
    let trainingData = (convertToGridOfTrainingData trainingDataFileContent ',' '\n')

    let learnedDecisionTree = learnDecisionTree trainingData (ceiling (logBase 2 (fromIntegral (length trainingData))) + 2)
    learnedDecisionTree

task2Print :: DecisionTree Int Double String -> String -> IO()
task2Print EmptyTree _indentStr = putStrLn("")
task2Print (Leaf className) indentStr = putStrLn(indentStr ++ "Leaf: " ++ className)
task2Print (Node attributeIndex attributeThreshold (left) (right)) indentStr = do 
    putStrLn(indentStr ++ "Node: " ++ (show attributeIndex) ++ ", " ++ (show attributeThreshold))
    task2Print left (indentStr ++ "  ")
    task2Print right (indentStr ++ "  ")

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

                    --let grid = (convertToGridOfDoubles fileClasifyDataContents ',' '\n')
                    --print grid

                    task1Print result
        "-2" -> do 
            let argsLen = length(args)
            if (argsLen < 1) 
                then putStrLn "Chybi vstupni soubor!!!"
                else do 
                    let fileTrainingData = args !! 0
                    fileTrainingDataContents <- readFile fileTrainingData
                    let learnedDecisionTree = task2 fileTrainingDataContents
                    task2Print learnedDecisionTree ""

                    -- let trainingData = (convertToGridOfTrainingData fileTrainingDataContents ',' '\n')
                    -- let (dataLeft, dataRight) = splitData trainingData 0 (0, 2.11)
                    -- print dataLeft
                    -- print "------"
                    -- print dataRight

                    -- let (leftBucket, rightBucket, classes) = initBuckets dataLeft 0
                    -- let (bestGini, bestAttributeIndex, bestAttributeThreshold) = _learnDecisionTreeProcessAttribute dataLeft 0 2 (leftBucket, rightBucket, classes) (10.0, 0, 0.0)
                    -- print classes

                    -- let sortedTrainingData = sortBy (trainingDataSortFnc 0) dataLeft
                    -- let (newBestGini, newBestAttributeIndex, newBestAttributeThreshold) = _learnDecisionTree sortedTrainingData 0 (leftBucket, rightBucket, classes) 0 (10.0, 0, 0.0)
                    -- print newBestGini
                    -- print newBestAttributeIndex
                    -- print newBestAttributeThreshold
                    -- print sortedTrainingData


                    -- let (row, className) = sortedTrainingData !! 0
                    -- let classIndex = findIndexOf classes className

                    -- let giniRight = giniScore rightBucket classIndex
                    -- print giniRight
        _ -> do 
            putStrLn("Usage: flp-fun -1 <soubor obsahujici strom> <soubor obsahujici nove data> | flp-fun -2 <soubor obsahujici trenovaci data>")


-- podívat se na implementaci třídyx show (možná myslí deriving Show)
