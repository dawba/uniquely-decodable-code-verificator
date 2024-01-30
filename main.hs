------------------------------------------------------------------------------------------------------------------------
-- GENERAL UTILS

--
--  SETS
--

intersectionOfTwoSets :: Eq a => [a] -> [a] -> [a]
intersectionOfTwoSets first second = foldl (\acc el -> if el `elem` second then el : acc else acc) [] first

areSetsEqual :: Eq a => [a] -> [a] -> Bool
areSetsEqual first second = length first == intersectionLength && length second == intersectionLength
  where
    intersection = intersectionOfTwoSets first second
    intersectionLength = length intersection

--
-- DIGITS
--

isDigit :: Char -> Bool
isDigit char = char >= '0' && char <= '9'

--
-- COMBINATIONS
--

generateCombinationsOfGivenLength :: Int -> [String] -> [String]
generateCombinationsOfGivenLength 0 _ = [""]
generateCombinationsOfGivenLength n xs = [x ++ y | x <- xs, y <- generateCombinationsOfGivenLength (n-1) xs]

listToSet :: [String] -> [String]
listToSet list = foldl(\acc el -> if el `elem` acc then acc else el : acc) [] list

------------------------------------------------------------------------------------------------------------------------
-- SARDINAS-PATERSON ALGORITHM UTILS

--
--  PREFIXES/SUFFIXES
--

isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

getDanglingSuffix :: String -> String -> String
getDanglingSuffix a b
  | a `isPrefix` b = drop (length a) b
  | otherwise = []

getAllDanglingSuffixesForWord :: String -> [String] -> [String]
getAllDanglingSuffixesForWord a codewords = filter(not . null) [suffix | y <- codewords, let suffix = getDanglingSuffix a y]

filterOnlyNewSuffixes :: [String] -> [String] -> [String]
filterOnlyNewSuffixes potentialNewSuffixes currentSuffixes = filter(`notElem` currentSuffixes) potentialNewSuffixes

getAllDanglingSuffixesForList :: [String] -> [String] -> [String]
getAllDanglingSuffixesForList list1 list2 = foldl(\suffixes word -> filterOnlyNewSuffixes (getAllDanglingSuffixesForWord word list2) suffixes ++suffixes) [] list1

getInitialSuffixSet :: [String] -> [String]
getInitialSuffixSet set = getAllDanglingSuffixesForList set set

getNextSuffixSet :: [String] -> [String] -> [String]
getNextSuffixSet codewords suffixes_j = getAllDanglingSuffixesForList codewords suffixes_j ++ getAllDanglingSuffixesForList suffixes_j codewords

checkIfSuffixSetContainsCodeword :: [String] -> [String] -> Bool
checkIfSuffixSetContainsCodeword suffixes codewords = not (null (intersectionOfTwoSets suffixes codewords))

checkIfSuffixLoop :: [String] -> [[String]] -> Bool
checkIfSuffixLoop currentSuffixSet suffixes_j = currentSuffixSet `elem` suffixes_j

------------------------------------------------------------------------------------------------------------------------
-- REGEX

 --
 -- STRING TO REGEX
 --

-- default character set for regex
characters = ['a'..'z'] ++ ['A'..'Z']

data Reg =
            Literal Char |
            Suma Reg Reg |
            Iloczyn Reg Reg |
            Potega Reg Int
            deriving (Eq, Show)

-- REGEX represented by ADS Reg is obtained by parsing the string using recursive descent parser
-- Parser recursively calls parseSum -> parseProduct -> parsePower -> parseLiteral to parse given regex string

parseLiteral :: String -> Maybe (Reg, String)
parseLiteral ('(':xs) = do
    (r, ys) <- parseSum xs
    case ys of
        (')':zs) -> return (r, zs)
        _ -> Nothing
parseLiteral (x:xs) | x `elem` characters = Just (Literal x, xs)
parseLiteral _ = Nothing

parseSum :: String -> Maybe (Reg, String)
parseSum s = do
    (r1, s1) <- parseProduct s
    case s1 of
        ('+':s2) -> do
            (r2, s3) <- parseSum s2
            return (Suma r1 r2, s3)
        _ -> return (r1, s1)

parseProduct :: String -> Maybe (Reg, String)
parseProduct s = do
    (r1, s1) <- parsePower s
    case s1 of
        (x:xs) | x `elem` (characters ++ "(") -> do
            (r2, s2) <- parseProduct (x:xs)
            return (Iloczyn r1 r2, s2)
        _ -> return (r1, s1)

parsePower :: String -> Maybe (Reg, String)
parsePower s = do
    (r1, s1) <- parseLiteral s
    case s1 of
        ('^':s2) -> do
            if null s2 || not (isDigit (head s2))
                then Nothing
                else do
                    let (num, s3) = span isDigit s2
                    return (Potega r1 (read num), s3)
        _ -> return (r1, s1)

string2Regex :: String -> Maybe (Reg, String)
string2Regex input = do
  (r1, s1) <- parseSum input
  case s1 of
    "" -> return (r1, s1)
    _ -> Nothing

--
-- REGEX TO SET OF WORDS
--

regex2List :: Reg -> [String]
regex2List (Literal c) = [[c]]
regex2List (Suma r1 r2) = regex2List r1 ++ regex2List r2
regex2List (Iloczyn r1 r2) = [x ++ y | x <- regex2List r1, y <- regex2List r2]
regex2List (Potega r1 n) = generateCombinationsOfGivenLength n (regex2List r1)

------------------------------------------------------------------------------------------------------------------------
-- SARDINAS-PATERSON ALGORITHM

-- 1. Sardinas-Patterson algorithm checks unique decodability of the code by checking
-- if some codeword from the set is a suffix of another codeword from the set (or is a composition of several suffixes).
-- 2. We define S_0 = C-1C as our initial set of dangling suffixes.
-- 2. We define S_i = C-1 S_j v S_j-1C as our i-th set of dangling suffixes (where C is the set of codewords, S_j is the previous set of dangling suffixes).
-- 4. If for some x in S_i: x in C => the code is not uniquely decodable.
-- 5. If for some i, i > j: S_i = S_j => the code uniquely decodable.
-- 6. If 4. & 5. are not satisfied, we repeat the process with S_i as the new S_j.

spHelper :: [String] -> [[String]] -> Bool
spHelper codewords currentSuffixSets
  | checkIfSuffixSetContainsCodeword (head currentSuffixSets) codewords = False
  | checkIfSuffixLoop (head currentSuffixSets) (tail currentSuffixSets) = True
  | otherwise = spHelper codewords ([getNextSuffixSet codewords (head currentSuffixSets)]++currentSuffixSets)

sp :: [String] -> Bool
sp set = spHelper set [getInitialSuffixSet set]

------------------------------------------------------------------------------------------------------------------------
-- MAIN

main :: IO ()
main = do
  putStrLn "Enter a regular expression"
  putStrLn "Possible characters: literals [a..z, A..Z] , +, ^ followed by a number"
  stringRegex <- getLine
  case string2Regex stringRegex of
    Just (regex, "") -> do                      -- only process regex into codewords set if no tokens left after parsing
        let set = listToSet(regex2List regex)   -- generate codewords set from regex
        print set
        print (sp set)                          -- apply sardinas-paterson algorithms
    _ -> putStrLn "Invalid regular expression"
