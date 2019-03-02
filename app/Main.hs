module Main where

import System.Environment
import Data.List
import System.IO
import System.Exit
import Data.List
import Data.String
import Text.Read

prefixArg::String
prefixArg = "--"

deBruijn_mode::[String]
deBruijn_mode =
    [
        "--check",
        "--unique",
        "--clean"
    ]

deBruijn_help::[String]
deBruijn_help =
    [
        "USAGE: ./deBruijn n [a] [--check|--unique|--clean]",
        "",
        "      --check       check if a sequence is a de Bruijn sequence",
        "      --unique      check if 2 sequences are distinct de Bruijn sequences",
        "      --clean       list cleaning",
        "      n             order of the sequence",
        "      a             alphabet [def: \"01\"]"
    ]


deBruijnExit::IO (a)
deBruijnExit = do
             mapM putStrLn deBruijn_help
             exitWith (ExitFailure 84)

deBruijn_default_alphabet::String
deBruijn_default_alphabet = "01"

main = do
    args <- myGetArgs                  -- IO [String]
    progName <- getProgName          -- IO String
    let test = parseArgs args
    let flag = epureFlag (fst test)
    let arg = snd test
    if length arg == 0
    then
        deBruijnExit
    else do
        let alphabet = getDefaultAlphabet arg
        n_int <- case readMaybe (head arg) of
            Just x -> return x
            Nothing -> deBruijnExit
        if isFlagValid flag && length arg >= 1 && length (removeDuplicate alphabet) == length alphabet && not (n_int == 0)
        then do
            let tflag = if length flag == 0 then [] else head flag
            launchMode tflag (head arg) alphabet
            exitSuccess
        else
            deBruijnExit

myGetArgs::IO([String])
myGetArgs = do
    args <- getArgs
    if (length args == 0)
    then
        deBruijnExit
    else
        return (args)

launchMode::String -> String -> String -> IO()
launchMode mod n alphabet
        | mod == deBruijn_mode!!0 = check n alphabet
        | mod == deBruijn_mode!!1 = unique n alphabet
        | mod == deBruijn_mode!!2 = clean n alphabet
        | otherwise = putStrLn (deBruijnGen alphabet (read n::Int))

unique::String -> String -> IO()
unique n alphabet = do
        let convert_n = read n::Int
        input <- getLine
        input2 <- getLine
        let check_input = removeMemberOfValueFromString 0 input alphabet
        let check_input2 = removeMemberOfValueFromString 0 input2 alphabet
        if length input == length (alphabet) ^ convert_n && length input2 == length (alphabet) ^ convert_n && deBruijnCheck input convert_n && deBruijnCheck input2 convert_n && check_input == "" && check_input2 == "" && not (input == input2)
                then do
                    let comb = deBruijnGeneratorAllTableRotate [] (deBruijnGenerateTable [] input convert_n)
                    if not (elem (deBruijnGenerateTable [] input2 convert_n) comb)
                    then
                        putStrLn "OK"
                    else
                        putStrLn "KO"
                else
                    putStrLn "KO"

check::String -> String -> IO()
check n alphabet = do
        let convert_n = read n::Int
        input <- getLine
        let check_input = removeMemberOfValueFromString 0 input alphabet
        if length input == length (alphabet) ^ convert_n && deBruijnCheck input convert_n && check_input == ""
        then
            putStrLn "OK"
        else
            putStrLn "KO"

clean::String -> String -> IO()
clean n alphabet = do
        let convert_n = read n::Int
        values <- cleanGet [] convert_n alphabet
        mapM printClean values
        return ()

printClean::(String, [[String]]) -> IO()
printClean val = putStrLn (fst val)

cleanGet::[(String, [[String]])] -> Int -> String -> IO([(String, [[String]])])
cleanGet xs n alphabet = do
        input <- getLine
        if input == "END"
        then
            return (xs)
        else do
            let check_input = removeMemberOfValueFromString 0 input alphabet

            if length input == length (alphabet) ^ n && deBruijnCheck input n && check_input == "" && deBruijnCheckIfIsTrueUnique xs (deBruijnGenerateTable [] input n)
            then do
                v <- cleanGet (xs ++ [(input, deBruijnGeneratorAllTableRotate [] (deBruijnGenerateTable [] input n))]) n alphabet
                return (v)
            else do
                v <- cleanGet xs n alphabet
                return (v)

deBruijnGen::String -> Int -> String
deBruijnGen alphabet n = concat (lyndonTodeBruijn (lyndonWords [last alphabet] alphabet n) n)

lyndonTodeBruijn::[String] -> Int -> [String]
lyndonTodeBruijn xs n = [ x | x <- xs, mod n (length x) == 0]

lyndonWords::String -> String -> Int -> [String]
lyndonWords [] alphabet n = []
lyndonWords xs alphabet n = [inc] ++ lyndonWords next alphabet n
                            where inc = incLyndonWord xs alphabet
                                  next = removeTrailingLyndonCharac (repeatLyndonWord inc (length xs) n) alphabet n


removeTrailingLyndonCharac::String -> String -> Int -> String
removeTrailingLyndonCharac xs alphabet n
                            | (not (length xs == 0)) && (last xs == last alphabet) = removeTrailingLyndonCharac (take ((length xs) - 1) xs) alphabet n
                            | otherwise = xs

repeatLyndonWord::String -> Int -> Int -> String
repeatLyndonWord xs x n
                | n == length xs = xs
                | otherwise = repeatLyndonWord (xs ++ [(reverse ((xs ++ xs))!!(x - 1))]) x n

incLyndonWord::String -> String -> String
incLyndonWord xs alpha = (take ((length xs) - 1) xs) ++ [getNextCarac (last xs) alpha]

getNextCarac::Char -> String -> Char
getNextCarac c alpha = (alpha ++ alpha)!!((head (elemIndices c alpha)) + 1)

deBruijnCheck::String -> Int -> Bool
deBruijnCheck input n = if length table == length (removeDuplicate table) then True else False
                        where table = deBruijnGenerateTable [] input n

deBruijnGenerateTable::[String] -> String -> Int -> [String]
deBruijnGenerateTable table input n = if length table == length input
                                      then
                                            table
                                      else
                                            deBruijnGenerateTable (table ++ [drop (length table) (take (length table + n) (input ++ input))]) input n

deBruijnGeneratorAllTableRotate::[[String]] -> [String] -> [[String]]
deBruijnGeneratorAllTableRotate xs [] = [[]]
deBruijnGeneratorAllTableRotate xs val
                                    | length xs == length val = xs
                                    | otherwise = deBruijnGeneratorAllTableRotate (xs ++ [rotateList z val]) val
                                    where z = length xs

getDefaultAlphabet::[String] -> String
getDefaultAlphabet xs = if length xs == 2 then xs!!1 else deBruijn_default_alphabet

parseArgs::[String] -> ([String], [String])
parseArgs[] = ([], [])
parseArgs xs = ([ x | x <- xs, isPrefixOf prefixArg x ], [ x | x <- xs, not (isPrefixOf prefixArg x) ])

isFlagValid::[String] -> Bool
isFlagValid[] = True
isFlagValid xs = if length xs == 1 then True else False

epureFlag::[String] -> [String]
epureFlag xs = removeDuplicate [ x | x <- xs, checkFlagsExist x]

checkFlagsExist::String -> Bool
checkFlagsExist x = if elem x deBruijn_mode then True else False

removeDuplicate::Eq a => [a] -> [a]
removeDuplicate [] = []
removeDuplicate (x:xs) = x : removeDuplicate (filter (x /=) xs)

removeMemberOfValueFromString::Int -> String -> String -> String
removeMemberOfValueFromString ls xs cs
                       | ls == length cs = xs
                       | otherwise = removeMemberOfValueFromString (ls + 1) tab cs
                       where tab = removeMemberOfValue xs (cs!!ls)

removeMemberOfValue::Eq a => [a] -> a -> [a]
removeMemberOfValue [] a = []
removeMemberOfValue xs a = [ x | x <- xs, not (x == a)]

numberOfOccurence::[String] -> String -> Int
numberOfOccurence [] a = 0
numberOfOccurence (x:xs) a = if x == a
                             then 1 + numberOfOccurence xs a
                             else numberOfOccurence xs a

rotateList::Int -> [a] -> [a]
rotateList n xs = take (length xs) (drop n (xs ++ xs))

deBruijnCheckIfIsTrueUnique::[(String, [[String]])] -> [String] -> Bool
deBruijnCheckIfIsTrueUnique [] val = True
deBruijnCheckIfIsTrueUnique (x:xs) val
                            | elem val (snd x) = False
                            | otherwise = deBruijnCheckIfIsTrueUnique xs val