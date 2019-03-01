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

deBruijn_default_alphabet::String
deBruijn_default_alphabet = "01"

main = do
    args <- getArgs                  -- IO [String]
    progName <- getProgName          -- IO String
    let test = parseArgs args
    let flag = epureFlag (fst test)
    let arg = snd test
    let alphabet = getDefaultAlphabet arg
    n_int <- case readMaybe (head arg) of
        Just x -> return x
        Nothing -> exitWith (ExitFailure 84)
    if isFlagValid flag && length arg >= 1 && length (removeDuplicate alphabet) == length alphabet && not (n_int == 0)
    then do
        launchMode (head flag) (head arg) alphabet
        exitWith (ExitSuccess)
    else
        exitWith (ExitFailure 84)

launchMode::String -> String -> String -> IO()
launchMode mod n alphabet
        | mod == deBruijn_mode!!0 = check n alphabet
        | mod == deBruijn_mode!!1 = unique n alphabet
        | mod == deBruijn_mode!!2 = clean n alphabet
        | otherwise = exitWith (ExitFailure 84)

unique::String -> String -> IO()
unique n alphabet = do
        let convert_n = read n::Int
        putStrLn "unique"
        putStrLn n
        putStrLn alphabet

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
        putStrLn "clean"
        putStrLn n
        putStrLn alphabet

deBruijnCheck::String -> Int -> Bool
deBruijnCheck input n = if length table == length (removeDuplicate table) then True else False
                        where table = deBruijnGenerateTable [] input n

deBruijnGenerateTable::[String] -> String -> Int -> [String]
deBruijnGenerateTable table input n = if length table == length input
                                      then
                                            table
                                      else
                                            deBruijnGenerateTable (table ++ [drop (length table) (take (length table + n) (input ++ input))]) input n



deBruijnGen::String -> Int -> [String]
deBruijnGen alphabet n = []

getDefaultAlphabet::[String] -> String
getDefaultAlphabet xs = if length xs == 2 then xs!!1 else deBruijn_default_alphabet

parseArgs::[String] -> ([String], [String])
parseArgs[] = ([], [])
parseArgs xs = ([ x | x <- xs, isPrefixOf prefixArg x ], [ x | x <- xs, not (isPrefixOf prefixArg x) ])

isFlagValid::[String] -> Bool
isFlagValid[] = False
isFlagValid xs = if not (length xs == 1) then False else True

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