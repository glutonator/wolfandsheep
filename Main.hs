module Main where

import System.IO
import System.Exit
import Data.Binary
import Data.List
import Data.Ord
import Control.Exception

-- main = do
--     putStrLn "Enter two lines"
--     line1 <- getLine                                    -- line1 :: String
--     line2 <- getLine                                    -- line2 :: String
--     putStrLn ("you said: " ++ line1 ++ " and " ++ line2)
-- -- writeFile "filename.txt" "Your string here."

data Point = Point Int Int deriving (Show, Eq, Read)
xPoint (Point x y) = x
yPoint (Point x y) = y



p1 = Point 7 9
p2 = Point 27 29


pp = [p1,p2]

func p = writeFile "savefiles/test.txt" (show pp)

-- tutaj trzeba zrobic getline i to da nam stringa chyba i bedzie spoko
saveState filename points = writeFile filename (show points)

-- readfunc =
-- 	qqq <- readFile "test.txt"
-- 	let temp :: [Point]
-- 	    temp = read qqq 


-- func p = putStrLn (show p)

main = do
    qqq <- readFile "test.txt"
    let temp :: [Point]
        temp = read qqq
    let yyy = xPoint (head temp)
    let ggg = map xPoint temp
    let mmm = xPoint p2
    putStrLn (show ggg)
    putStrLn (show temp)






--tutaj są dobre funckje
save points = do
    putStrLn "Write file name:"
    putStr "Write here: "
    hFlush stdout
    filename <- getLine
    writeFile ("savefiles/"++filename++".txt") (show points)
    putStrLn ("Saved")

load = do
    putStrLn "Write file name:"
    putStr "Write here: "
    hFlush stdout
    filename <- getLine
    dataInFile <- readFile ("savefiles/"++filename++".txt")
    let points :: [Point]
        points = read dataInFile
    putStrLn (show points)
    putStrLn ("Loaded")