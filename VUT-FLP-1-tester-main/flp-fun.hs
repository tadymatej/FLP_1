import Text.Parsec 
    (ParseError, digit, many1, 
    optionMaybe, oneOf, char, many, string, (<|>), 
    try, noneOf, spaces, eof, newline, endBy, sepBy)
import System.Environment (getArgs)
import Text.Parsec.String (parseFromFile, Parser)
import Data.List (sortBy)
import Control.Applicative ((<$>), (<*))

data TrainingData = TrainingData 
    [Double] -- ^ Array including for each attribute one value
    String -- ^ class for concrete values of attributes
    deriving(Show, Eq)

data LearnTrainingTreeProps = LearnTrainingTreeProps 
    Double -- ^ Gini score
    Int -- ^ Attribute array index
    Double -- ^ Attribute threshold

data DecisionTree aI aT className = 
    Leaf className | -- ^ Leaf node representing certain class
    EmptyTree | -- ^ EmptyTree only if trainingData is an empty array
    Node aI aT (DecisionTree aI aT className) (DecisionTree aI aT className) -- ^ Node, be which we can clasify the data by threshold aT at attributeIndex aI

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
------------------------------- input parsing ---------------------

-- | Parses one column of input data file (column means sequance of characters splitted by whitespace character)
parseColumn :: Parser String
parseColumn = do
    className <- many (noneOf "\n,")
    return (className)

-- | Parses multiple columns
parseColumns :: Parser [String]
parseColumns = parseColumn `sepBy` separator
    where
        separator = many (char ' ') <* char ',' <* many (char ' ')

parseColumnsList :: Parser [[String]]
parseColumnsList = (parseColumns `endBy` newline) <* eof

-- | Parses file to array of columns splitted by ','
parseFile :: 
    FilePath -> -- ^ path to file which should be parsed
    IO (Either ParseError [[String]])
parseFile path = parseFromFile parseColumnsList path

-- | Parses 2D array of columns (strings) to 2D array of double values
parseNewDataList :: 
    [[String]] -> -- ^ 2d array of strings which will be parsed to doubles
    [[Double]]
parseNewDataList [] = []
parseNewDataList (item:items) = parseNewData item : parseNewDataList items
    where 
        parseNewData [] = []
        parseNewData (number: restNumbers) = (read number) : (parseNewData restNumbers)

-- | Parses 2D array of columns (strings) to array of TrainingData
parseTrainingDataList :: 
    [[String]] -> -- ^ 2d array of strings which will be parsed to array of TrainingData
    [TrainingData]
parseTrainingDataList [] = []
parseTrainingDataList (item : items) = parseTrainingData item : parseTrainingDataList items 
    where 
        parseTrainingData :: [String] -> TrainingData
        parseTrainingData [] = TrainingData [] ""
        parseTrainingData (actualItem: []) = TrainingData [] actualItem
        parseTrainingData (actualItem: restItems) =
            let (TrainingData numbers className) = parseTrainingData restItems
                newNumbers = (read actualItem) : numbers
            in (TrainingData newNumbers className)

-- | Parses decisionTree file which has the following structure (S is starting nonterminal symbol of simplified (pseudo)grammar):
-- S -> <NodeLeaf>
-- <Node> -> Node: <Int>, <Double>
-- depth * '  '<NodeLeaf>
-- depth * '  '<NodeLeaf>
-- <NodeLeaf> -> <Node> | <Leaf>
-- <Leaf> -> Leaf: <String>
-- where depth indicates depth in the decision tree
parseDecisionTree :: 
    String -> -- ^ String indicating the actual required indent
    Parser (DecisionTree Int Double String)
parseDecisionTree indentStr = do 
    _ <- string indentStr
    node <- try $ parseNode <|> parseLeaf
    case node of
        Left l -> return $ Leaf l
        Right (aI, aT) -> do 
            rest <- (parseDecisionTree (indentStr ++ "  "))
            rest2 <- (parseDecisionTree (indentStr ++ "  "))
            return $ Node aI aT rest rest2
  where
    -- | Parses <Node> NonTerminal
    parseNode = do 
        _ <- string "Node:"
        _ <- spaces
        aI <- read <$> many1 digit
        _ <- string ","
        _ <- spaces
        aT <- parseFloat
        _ <- string "\n"
        return $ Right (aI, aT)
    -- | Parses <Leaf>
    parseLeaf = do 
        _ <- string "Leaf: "
        className <- Left <$> ( many (noneOf "\n"))
        _ <- try $ string "\n" <|> string ""
        return className
    -- | parses <Double> nonterminal, this function will accept numbers in scientific notation, with decimal part and without it
    parseFloat = do
        sign <- optionMaybe (oneOf "+-")
        whole <- many1 digit
        maybeDot <- optionMaybe (char '.')
        decimal <- optionMaybe (many digit)
        e <- optionMaybe (char 'e')
        signExponent <- optionMaybe (oneOf "+-")
        exponentDigits <- optionMaybe (many1 digit)
        case (sign, maybeDot, decimal, e, signExponent, exponentDigits) of
            (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) -> return (read whole) -- 11
            (Just c, Nothing, Nothing, Nothing, Nothing, Nothing) -> return (read (c : whole)) -- +11
            (Just c, Nothing, Nothing, Just e2, Nothing, Just digitsExp) ->
                return (read ((c : whole) ++ (e2 : digitsExp))) -- +11e10
            (Just c, Nothing, Nothing, Just e2, Just signExponent2, Just digitsExp) -> -- +11e+10
                return (read ((c : whole) ++ (e2 : (signExponent2 : digitsExp))))
            (Just c, Just dot, Just decimal2, Nothing, Nothing, Nothing) -> -- +11.2
                return (read ((c : whole) ++ (dot : decimal2)))
            (Just c, Just dot, Just decimal2, Just e2, Nothing, Just digits) ->  -- +11.2e10
                return (read ((c : whole) ++ (dot : decimal2) ++ (e2 : digits)))
            (Just c, Just dot, Just decimal2, Just e2, Just esign, Just digits) -> -- +11.2e+10
                return (read ((c : whole) ++ (dot : decimal2) ++ (e2 : (esign: digits))))

            (Nothing, Nothing, Nothing, Just e2, Nothing, Just digits) -> -- 11e10
                return (read ((whole) ++ (e2 : digits)))
            (Nothing, Nothing, Nothing, Just e2, Just signExponent2, Just digitsExp) -> -- 11e+10
                return (read ((whole) ++ (e2 : (signExponent2 : digitsExp))))
            (Nothing, Just dot, Just decimal2, Nothing, Nothing, Nothing) -> -- 11.2
                return (read ((whole) ++ (dot : decimal2)))
            (Nothing, Just dot, Just decimal2, Just e2, Nothing, Just digits) ->  -- 11.2e10
                return (read ((whole) ++ (dot : decimal2) ++ (e2 : digits)))
            (Nothing, Just dot, Just decimal2, Just e2, Just esign, Just digits) -> -- 11.2e+10
                return (read ((whole) ++ (dot : decimal2) ++ (e2 : (esign: digits))))
            _ -> fail "Špatný formát čísla!"


-- | Parses the decision Tree file. More informations at function parseDecisionTree
parseDecisionTreeFile :: FilePath -> IO (Either ParseError (DecisionTree Int Double String))
parseDecisionTreeFile path = parseFromFile (parseDecisionTree "") path



---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
------------------------------- task 1 - classifying ---------------------

-- | classifies according to the decision tree one data item
decisionTreeClasify :: 
    DecisionTree Int Double String -> -- ^ Decision tree by which we are classifying
    [Double] -> -- ^ Array with value for each attribute by which we are classifying
    String
decisionTreeClasify (Leaf className) _ = className
decisionTreeClasify (Node aI aT left right) datas
    | (datas !! aI) <= aT = decisionTreeClasify left datas
    | otherwise = decisionTreeClasify right datas
decisionTreeClasify EmptyTree _ = ""

-- | Classifies all data entries according to the decision tree
decisionTreeClasifyList :: 
    DecisionTree Int Double String -> -- ^ Decision tree by which we are classifying
    [[Double]] -> -- ^ 2D array with values for each attribute by which we are classifying
    [String]
decisionTreeClasifyList tree (item : []) = [decisionTreeClasify tree item]
decisionTreeClasifyList tree (item: rest) = (decisionTreeClasify tree item) : 
    decisionTreeClasifyList tree rest
decisionTreeClasifyList _ _ = []


---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
------------------------------- task 2 - decision tree training ---------------------

updateUniqueCount :: (Eq a) => [(a, Int)] -> a -> [(a, Int)]
updateUniqueCount [] find = [(find, 1)]
updateUniqueCount ((item, countNumber): rest) find
    | item == find = (item, countNumber + 1) : rest
    | otherwise = (item, countNumber) : updateUniqueCount rest find

uniqueCount :: (Eq b, Show b) => (a -> b) -> [a] -> [(b, Int)]
uniqueCount _f [] = []
uniqueCount f (item:[]) = [((f item), 1)]
uniqueCount f (item: rest) = 
    let countArray = uniqueCount f rest
    in updateUniqueCount countArray (f item)


giniScore :: [TrainingData] -> Double 
giniScore classes =
    let uniqueCounts = uniqueCount (\(TrainingData _doubles className) -> className) classes
        lengthClasses = fromIntegral (length classes) :: Double
        probabilities = [((fromIntegral (snd x) :: Double) / lengthClasses) | x <- uniqueCounts ]
    in 1 - (foldr (\x localSum -> localSum + (x ** 2)) 0 probabilities)

splitData :: Int -> Double -> [TrainingData] -> ([TrainingData], [TrainingData])
splitData aI aT ((TrainingData doubles className): []) 
    | (doubles !! aI) > aT = ([], [TrainingData doubles className])
    | otherwise = ([TrainingData doubles className], [])
splitData aI aT ((TrainingData doubles className): rest)
    | (doubles !! aI) > aT = 
        let (leftData, rightData) = splitData aI aT rest
        in (leftData, (TrainingData doubles className) : rightData)
    | otherwise = 
        let (leftData, rightData) = splitData aI aT rest 
        in ((TrainingData doubles className) : leftData, rightData)
splitData _ _ [] = ([], [])

findBestSplit :: [TrainingData] -> LearnTrainingTreeProps
findBestSplit [] = (LearnTrainingTreeProps 0.0 (-1) 0.0)
findBestSplit (TrainingData doubles className : restData) = 
    findBestSplitProcessAttributes ((TrainingData doubles className) : restData) 0 (length(doubles)) 
                                            (LearnTrainingTreeProps 1.0 (-1) 0.0)


getMidpointArrays :: [TrainingData] -> Int -> ([Double], [Double])
getMidpointArrays trainingData attributeIndex = 
    let firstArray = getMidpointArrayFn trainingData attributeIndex (True, 0)
        secondArray = reverse firstArray
    in (firstArray, secondArray)
    where 
        getMidpointArrayFn :: [TrainingData] -> Int -> (Bool, Double) -> [Double]
        getMidpointArrayFn (_trainingData@(TrainingData doubles _className): restTrainingData) localAttributeIndex (firstLoad, prevValue)
            | firstLoad == True && restTrainingData == [] = [(doubles !! localAttributeIndex)]
            | firstLoad == True = (doubles !! localAttributeIndex) : getMidpointArrayFn restTrainingData localAttributeIndex (False, (doubles !! attributeIndex))
            | restTrainingData == [] && (doubles !! localAttributeIndex) /= prevValue = [(doubles !! localAttributeIndex)]
            | ((doubles !! localAttributeIndex) /= prevValue) = (doubles !! localAttributeIndex) : getMidpointArrayFn restTrainingData localAttributeIndex (False, (doubles !! attributeIndex))
            | restTrainingData == [] = []
            | otherwise = getMidpointArrayFn restTrainingData localAttributeIndex (False, prevValue)
        getMidpointArrayFn [] _ _ = []

calcMidpoints :: ([Double], [Double]) -> [Double]
calcMidpoints ((leftThreshold: []), (rightThreshold: [])) = [(leftThreshold + rightThreshold) / 2.0]
calcMidpoints ((leftThreshold:leftData), (rightThreshold: rightData)) = ((leftThreshold + rightThreshold) / 2.0) : calcMidpoints (leftData, rightData)
calcMidpoints _ = []

findBestSplitProcessAttributes :: [TrainingData] -> Int -> Int -> LearnTrainingTreeProps -> LearnTrainingTreeProps
findBestSplitProcessAttributes trainingData processingAI attributesCount learnProps@(LearnTrainingTreeProps prevBestGini _prevAI _prevAT) =
        let sortedTrainingData = sortBy (trainingDataSortFnc processingAI) trainingData
            midpointsArrays = getMidpointArrays sortedTrainingData processingAI
            midpoints = calcMidpoints midpointsArrays
        in recursiveCall (findBestSplitProcessMidpoints midpoints trainingData processingAI learnProps)
        where 
            recursiveCall :: LearnTrainingTreeProps -> LearnTrainingTreeProps
            recursiveCall trainingTreeProps
                | processingAI == (attributesCount - 1) = selectBestProps trainingTreeProps
                | otherwise = findBestSplitProcessAttributes trainingData (processingAI + 1) attributesCount (selectBestProps trainingTreeProps)
            selectBestProps :: LearnTrainingTreeProps -> LearnTrainingTreeProps 
            selectBestProps (LearnTrainingTreeProps bestGini aI aT)
                | bestGini < prevBestGini = LearnTrainingTreeProps bestGini aI aT 
                | otherwise = learnProps

findBestSplitProcessMidpoints :: [Double] -> [TrainingData] -> Int -> LearnTrainingTreeProps -> LearnTrainingTreeProps
findBestSplitProcessMidpoints [] _ _ props = props
findBestSplitProcessMidpoints (threshold : restMidpoints) trainingData processingAI learnProps =
    let (leftData, rightData) = splitData processingAI threshold trainingData
        giniLeft = giniScore leftData
        giniRight = giniScore rightData 
        leftDataSize = fromIntegral (length(leftData)) :: Double
        rightDataSize = fromIntegral (length(rightData)) :: Double
        totalSize = leftDataSize + rightDataSize
        gini = (leftDataSize * giniLeft + rightDataSize * giniRight) / totalSize
    in callRecursive (getBestGini learnProps (LearnTrainingTreeProps gini processingAI threshold))
    where 
        getBestGini :: LearnTrainingTreeProps -> LearnTrainingTreeProps -> LearnTrainingTreeProps 
        getBestGini puvLearnProps@(LearnTrainingTreeProps prevBestGini _prevAI _prevAT) localLearnProps@(LearnTrainingTreeProps gini _aI _aT)
            | gini < prevBestGini = localLearnProps
            | otherwise = puvLearnProps
        callRecursive :: LearnTrainingTreeProps -> LearnTrainingTreeProps
        callRecursive bestLearnProps
            | restMidpoints == [] = bestLearnProps
            | otherwise = findBestSplitProcessMidpoints restMidpoints trainingData processingAI bestLearnProps 


trainingDataSortFnc :: Int -> TrainingData -> TrainingData -> Ordering
trainingDataSortFnc index1D (TrainingData data1 _className1) (TrainingData data2 _className2) = 
    compare (data1 !! index1D) (data2 !! index1D)


buildTree :: [TrainingData] -> DecisionTree Int Double String
buildTree [] = EmptyTree
buildTree (trainingData@(TrainingData _doubles className): restTrainingData) = do
    let trainingTreeProps = findBestSplit (trainingData : restTrainingData)
        in buildTreeHelperFn (trainingData : restTrainingData) trainingTreeProps
    where
        buildTreeHelperFn :: [TrainingData] -> LearnTrainingTreeProps -> DecisionTree Int Double String 
        buildTreeHelperFn localTrainingData (LearnTrainingTreeProps _bestGini aI aT)
            | (length (uniqueCount (\(TrainingData _doubles localClassName) -> localClassName) localTrainingData)) == 1 = 
                Leaf className
            | otherwise =
                let (leftData, rightData) = splitData aI aT localTrainingData
                in (Node aI aT (buildTree leftData) (buildTree rightData))



---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- | Prints classified data (its className) to each line
task1Print :: 
    [String] -> -- ^ Array of classified classNames
    IO()
task1Print [] = putStr("")
task1Print (item: []) = putStrLn(item)
task1Print (item : rest) = do 
    putStrLn(item) 
    task1Print rest

-- | Prints the trained DecisionTree in same format as in task1, described in parseDecisionTree
task2Print :: 
    DecisionTree Int Double String -> -- ^ Trained decision tree to print
    String -> -- ^ Indentation to print
    IO()
task2Print EmptyTree _indentStr = putStrLn("")
task2Print (Leaf className) indentStr = putStrLn(indentStr ++ "Leaf: " ++ className)
task2Print (Node attributeIndex attributeThreshold (left) (right)) indentStr = do 
    putStrLn(indentStr ++ "Node: " ++ (show attributeIndex) ++ ", " ++ (show attributeThreshold))
    task2Print left (indentStr ++ "  ")
    task2Print right (indentStr ++ "  ")

main :: IO ()
main = do 
    actualGetArgs <- getArgs
    case actualGetArgs of 
        ("-1": args)  -> runTask1 args
        ("-2":args) -> runTask2 args
        _ -> putStrLn ("Neznamy parametr!")
    where 
        -- | run task1 (clasifying by existing decision tree)
        runTask1 args 
            | (length(args)) < 2 = putStrLn "Chybi vstupni soubory!!!"
            | otherwise = do 
                result <- parseFile (args !! 1)
                case result of
                    Left err -> putStrLn $ "Chyba při parsování souboru: " ++ show err
                    Right strings -> runParsingDecisionTree args strings
        -- | helper function to run task1 (parses decisionTree input file and runs task1)
        runParsingDecisionTree args strings = do
            let numbers = parseNewDataList strings
            result3 <- parseDecisionTreeFile (args !! 0)
            case result3 of
                Left err -> putStrLn $ "Chyba při parsování souboru: " ++ show err
                Right tree -> task1Print (decisionTreeClasifyList tree numbers)
        -- | run task2 (training decision tree)
        runTask2 args
            | (length(args)) < 1 = putStrLn "Chybi vstupni soubor!!!"
            | otherwise = do 
                result <- parseFile (args !! 0)
                case result of
                    Left err -> putStrLn $ "Chyba při parsování souboru: " ++ show err
                    Right strings -> do
                        let trainingData = parseTrainingDataList strings
                        task2Print (buildTree trainingData) ""
