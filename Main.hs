module Main where

import System.IO
import System.Exit
import Data.Binary
import Data.List
import Data.Ord
import Control.Exception
import System.Directory

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

-- main = do
--     qqq <- readFile "test.txt"
--     let temp :: [Point]
--         temp = read qqq
--     let yyy = xPoint (head temp)
--     let ggg = map xPoint temp
--     let mmm = xPoint p2
--     putStrLn (show ggg)
--     putStrLn (show temp)


main = do 
    --clear screen --commented for testing
    -- putStr "\ESC[2J"
    --
    -- listDirectory "/home/filip/Desktop/hprojekt"
    putStrLn "n - start new game"
    putStrLn "s - save current game"
    putStrLn "l - load saved game"
    putStrLn "f - list of existing saves"
    putStrLn "b - create folser for saves" --poxniej trzeba by to gdzies wrzucic w setupie
    putStrLn "------------------------------------------------------"
    putStr "Your decision: "
    input <- getChar
    putStrLn "\n"
    case input of 
        'n' -> do putStrLn "new game"
        's' -> do putStrLn "save"
        'l' -> do putStrLn "load"
        'f' -> do listOfFiles
        'b' -> do createSaveDir


        _ -> main
        -- otherwise -> do  putStrLn "cos innego"
    putStrLn "exit"

    -- putStrLn input


listOfFiles = do
    rrr <- listDirectory "./savefiles"
    putStrLn(show rrr)

createSaveDir = createDirectory "./savefiles"


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
