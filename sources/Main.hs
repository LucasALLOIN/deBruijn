module Main where

import System.Environment
import Data.List
import System.Environment
import System.IO
import System.Exit
import Data.List
import Data.String
import System.Exit

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
    putStrLn "Arg flag:"
    mapM putStrLn flag
    putStrLn "Arg value:"
    mapM putStrLn arg
    if isFlagValid flag && length arg >= 1 && length (removeDuplicate alphabet) == length alphabet
    then do
        launchMode (head flag) (head arg) alphabet
        putStrLn "OK"
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
        let convert_n = read n::Integer
        putStrLn "unique"
        putStrLn n
        putStrLn alphabet

check::String -> String -> IO()
check n alphabet = do
        let convert_n = read n::Integer
        input <- getLine
        putStrLn input

clean::String -> String -> IO()
clean n alphabet = do
        let convert_n = read n::Integer
        putStrLn "clean"
        putStrLn n
        putStrLn alphabet

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

numberOfOccurence::[String] -> String -> Int
numberOfOccurence [] a = 0
numberOfOccurence (x:xs) a = if x == a
                             then 1 + numberOfOccurence xs a
                             else numberOfOccurence xs a