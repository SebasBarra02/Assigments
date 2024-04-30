{-# LANGUAGE OverloadedStrings #-}
module RandomNamePick where

import System.IO
import System.Random
import Control.Monad (liftM)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Map as Map



type StudentName = String

data Student = Student{
    lastName::String,
    name::StudentName
    } deriving (Show, Eq,Ord)

data SchoolHouse = Griffindor
                    | Hufflepuff
                    | Ravenclaw 
                    | Slytherin 
                    deriving (Eq, Show, Ord, Read)

type HouseAssignments = Map.Map SchoolHouse Int

maxStudentsPerHouse :: Int
maxStudentsPerHouse = 15  

assignSchoolHouseWithLimit :: HouseAssignments -> IO (SchoolHouse, HouseAssignments)
assignSchoolHouseWithLimit assignments = do
    let houses = [Griffindor, Hufflepuff, Ravenclaw, Slytherin]
    let availableHouses = filter (\house -> Map.findWithDefault 0 house assignments < maxStudentsPerHouse) houses
    house <- pickRandom availableHouses
    let updatedAssignments = Map.insertWith (+) house 1 assignments
    return (house, updatedAssignments)

pickRandom :: [a] -> IO a
pickRandom xs = do
    idx <- randomRIO (0, length xs - 1) 
    return (xs !! idx) 

split' :: Eq a => a -> [a] -> [[a]]
split' d [] = []
split' d s = x : split' d (drop 1 y)
             where (x,y) = span (/= d) s

parseLine :: String -> Student
parseLine line =
    let [lastName, name] = split' ',' line
    in Student{lastName=lastName, name=name}

readFileToList :: FilePath -> IO [Student]
readFileToList filePath = do
    contents <- readFile filePath
    let students = map parseLine (lines contents)
    return students

printAssignments :: HouseAssignments -> IO ()
printAssignments assignments = do
    putStrLn "Número de estudiantes asignados a cada casa:"
    putStrLn $ "Griffindor: " ++ show (Map.findWithDefault 0 Griffindor assignments)
    putStrLn $ "Hufflepuff: " ++ show (Map.findWithDefault 0 Hufflepuff assignments)
    putStrLn $ "Ravenclaw: " ++ show (Map.findWithDefault 0 Ravenclaw assignments)
    putStrLn $ "Slytherin: " ++ show (Map.findWithDefault 0 Slytherin assignments)

main :: IO()
main = do
    records <- readFileToList "/home/fundacion/Haskkell/PL/Assigments/list.txt"
    let studentsList assignments [] = return assignments
        studentsList assignments (x:xs) = do 
            (schoolHouse, updatedAssignments) <- assignSchoolHouseWithLimit assignments
            putStrLn $ show (x,schoolHouse)
            studentsList updatedAssignments xs
    finalAssignments <- studentsList Map.empty records
    printAssignments finalAssignments
