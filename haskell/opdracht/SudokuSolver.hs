{-
Pepijn van Wijk
13952072
Bachelor Informatica

Dit programma lost sudoku's op. Je kunt de solver aanroepen door main aan te
roepen en het pad van een .txt sudoku bestand mee te geven. Ook NRC sudoku's
kunnen opgelost worden, de string "nrc" moet dan wel voorkomen in het pad, anders
wordt de NRC solver niet aangeroepen.
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

import System.Environment
import Data.List

type Row = Int
type Column = Int
type Value = Int
type Grid = [[Value]] -- Only used to read/write from/to a file.
type Sudoku = (Row,Column) -> Value
type Constraint = (Row, Column, [Value])
type Node = (Sudoku, [Constraint])
type Solver = Sudoku -> Sudoku

positions :: [Int]
positions = [1..9]

values :: [Value]
values = [1..9]

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

-- Locations of NRC blocks.
blocksNRC :: [[Int]]
blocksNRC = [[2..4], [6..8]]

centerOfBlocks :: [Int]
centerOfBlocks = [2, 5, 8]

-- Center of NRC blocks.
centerOfBlocksNRC :: [Int]
centerOfBlocksNRC = [3, 7]

sud2grid :: Sudoku -> Grid
sud2grid s = [[s (r, c) | c <- positions] | r <- positions]

grid2sud :: Grid -> Sudoku
grid2sud gr = \(r, c) -> pos gr (r, c)
  where pos :: [[a]] -> (Row,Column) -> a
        pos gr (r, c) = gr !! (r - 1) !! (c - 1)

-- Extends a sudoku with a value at (row, column).
extend :: Sudoku -> (Row, Column, Value) -> Sudoku
extend sud (r, c, v) (i, j) = if r == i && c == j then v else sud (i, j)

-- Read a file-sudoku with a Grid like format into a Sudoku.
readSudoku :: String -> IO Sudoku
readSudoku filename =
    do stringGrid <- readFile filename
       return $ (grid2sud . splitStringIntoGrid) stringGrid
       where splitStringIntoGrid = map (map readint . words) . lines
             readint x = read x :: Int

{- Prints a Sudoku to the terminal by transforming it to a grid first.
   Do not modify this, or your tests will fail.
-}
printSudoku :: Sudoku -> IO ()
printSudoku = putStr . unlines . map (unwords . map show) . sud2grid

-- Helper to parse command-line arguments.
getSudokuName :: [String] -> String
getSudokuName [] = error "Filename of sudoku as first argument."
getSudokuName (x:_) = x

-- Returns the value of given coordinates.
getValue :: Sudoku -> (Row, Column) ->Value
getValue sud (row,col) = sud (row, col)

-- Returns a list with the values of the row. (Rows go from 1-9).
getRow :: Sudoku -> Row -> [Value]
getRow sud row = sud2grid sud !! (row - 1)

-- From a given sudoku and row, gives values not yet in the row.
freeInRow :: Sudoku -> Row -> [Value]
freeInRow sud row = values \\ getRow sud row

-- Returns a list with values of the column. (Columns go from 1-9).
getColumn :: Sudoku -> Column -> [Value]
getColumn sud col = map (\ curr_row -> curr_row !! (col - 1)) (sud2grid sud)

-- From a given sudoku and column, gives values not yet in the column.
freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn sud col = values \\ getColumn sud col

-- Creates a list with the values of each cell in a sub grid.
subGridCoords2Values :: Sudoku -> [(Row, Column)] -> [Value]
subGridCoords2Values sud = map
   (\ curr_coords -> sud2grid sud !! (fst curr_coords - 1) !!
      (snd curr_coords - 1))

-- Creates the list with coordinates of the sub grid of a given row and column.
getSubGridCoords :: Sudoku -> (Row, Column) -> [(Row, Column)]
getSubGridCoords sud (row,col) =
   [(row,col) | row<-head (filter (elem row) blocks),
      col <-head (filter (elem col) blocks)]

-- Creates the list with coordinates of the sub grid of a given row and column.
getSubGridCoordsNRC :: Sudoku -> (Row, Column) -> [(Row, Column)]
getSubGridCoordsNRC sud (row,col) =
   [(row,col) | row<-head (filter (elem row) blocksNRC),
      col <-head (filter (elem col) blocksNRC)]

-- From a given sudoku, row and column, gives values not yet in the 3x3 subgrid.
freeInSubgrid :: Sudoku -> (Row, Column) -> [Value]
freeInSubgrid sud (row,col) =
   values \\ subGridCoords2Values sud (getSubGridCoords sud (row, col))

-- From a given sudoku, row and column, gives values not yet in the 3x3 subgrid.
freeInSubgridNRC :: Sudoku -> (Row, Column) -> [Value]
freeInSubgridNRC sud (row,col) =
   values \\ subGridCoords2Values sud (getSubGridCoordsNRC sud (row, col))

-- Gives the possible values for the given position.
freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos sud (row,col) = (freeInRow sud row `intersect` freeInColumn sud col)
   `intersect` freeInSubgrid sud (row, col)

-- Gives the possible values for the given position.
freeAtPosNRC :: Sudoku -> (Row,Column) -> [Value]
freeAtPosNRC sud (row,col)
         | not (any (elem row) blocksNRC) || not (any (elem col) blocksNRC) =
   (freeInRow sud row `intersect` freeInColumn sud col) `intersect`
   freeInSubgrid sud (row, col)
         | otherwise =
   (freeInRow sud row `intersect` freeInColumn sud col) `intersect`
   (freeInSubgrid sud (row, col) `intersect` freeInSubgridNRC sud (row, col))

-- Gives a list with every coordinate in the sudoku that is not filled in yet.
openPositions :: Sudoku -> [(Row,Column)]
openPositions sud = [(row, col) | col <- positions, row <- positions,
   getValue sud (row, col) == 0]

-- Returns true if a row follows sudoku rules (not more than one of the same
-- value per row). Returns true if valid.
rowValid :: Sudoku -> Row -> Bool
rowValid sud row = null (filter (/=0) (getRow sud row) \\ values)

-- Returns true if a column follows sudoku rules (not more than one of the same
-- value per column). Returns true if valid.
colValid :: Sudoku -> Column -> Bool
colValid sud col = null (filter (/=0) (getColumn sud col) \\ values)

-- Returns true if a subgrid follows sudoku rules (not more than one of the same
-- value per subgrid). Returns true if valid.
subgridValid :: Sudoku -> (Row,Column) -> Bool
subgridValid sud (row,col) =
   null (filter (/=0) (subGridCoords2Values sud
      (getSubGridCoords sud (row,col))) \\ values)

-- Returns true if a subgrid follows sudoku rules (not more than one of the same
-- value per subgrid). Returns true if valid.
subgridValidNRC :: Sudoku -> (Row,Column) -> Bool
subgridValidNRC sud (row,col) =
   null (filter (/=0) (subGridCoords2Values sud
      (getSubGridCoordsNRC sud (row,col))) \\ values)

-- Returns a list with the centers of each grid.
centerOfGrids :: [(Row, Column)]
centerOfGrids = [(row,column) | row <- centerOfBlocks, column <- centerOfBlocks]

-- Returns a list with the centers of each grid.
centerOfGridsNRC :: [(Row, Column)]
centerOfGridsNRC = [(row,column) | row <- centerOfBlocksNRC,
   column <- centerOfBlocksNRC]

-- Checks if sudoku follows all rules (not more than one of the same value per
-- row, column and subgrid). Returns true if consistent.
consistent :: Sudoku -> Bool
consistent sud = and  (
                     [rowValid sud row | row <- positions]
                     ++ [colValid sud col | col <- positions]
                     ++ [subgridValid sud subgrid | subgrid <- centerOfGrids])

-- Checks if sudoku follows all NRC rules (not more than one of the same value
-- per row, column and subgrid + NRC subgrid). Returns true if consistent.
consistentNRC :: Sudoku -> Bool
consistentNRC sud =
   and (consistent sud : [subgridValidNRC sud subgrid |
      subgrid <- centerOfGridsNRC])

-- Creates a constraint type from a row and column pair.
createConstraint :: Sudoku -> (Row,Column) -> Constraint
createConstraint sud (row, col) = (row, col, freeAtPos sud (row, col))

-- Creates and ordering based on the length of the values list in a constraint.
lengthValues :: Constraint -> Constraint -> Ordering
lengthValues (row, col, values) (row2, col2, values2) = compare (length values)
   (length values2)

-- Creates a list containing every constraint, sorted from least options to most.
constraints :: Sudoku -> [Constraint]
constraints sud =
   sortBy lengthValues [(row, col, freeAtPos sud (row, col)) |
      (row, col) <- openPositions sud]

-- Creates a list containing every constraint, sorted from least options to most.
constraintsNRC :: Sudoku -> [Constraint]
constraintsNRC sud =
   sortBy lengthValues [(row, col, freeAtPosNRC sud (row, col)) |
      (row, col) <- openPositions sud]

-- Creates a node.
createNode :: Sudoku -> Node
createNode sud = (sud,constraints sud)

-- Creates a node.
createNodeNRC :: Sudoku -> Node
createNodeNRC sud = (sud,constraintsNRC sud)

-- Creates a list of nodes created by placing the possible values of a constraint
-- in the sudoku.
newNodes :: Node -> Constraint -> [Node]
newNodes (sud, constraints) (row, col, values) =
   [createNode (extend sud (row,col,v)) | v <- values ]

-- Creates a list of nodes created by placing the possible values of a constraint
-- in the sudoku.
newNodesNRC :: Node -> Constraint -> [Node]
newNodesNRC (sud, constraintsNRC) (row, col, values) =
   [createNodeNRC (extend sud (row,col,v)) | v <- values ]

-- If the list of a node is empty, the sudoku is solved. Returns true is sudoku
-- is solved.
isSudokuSolved :: Node -> Bool
isSudokuSolved node = null (snd node)

-- Solves the sudoku.
-- Inspiration from https://stackoverflow.com/questions/35750123/backtrack-sudoku-in-haskell
solve :: Node -> [Node]
solve n  | not (consistent (fst n)) = error "Sudoku not solvable" -- Checks if sudoku is solvable.
         | isSudokuSolved n = [n] -- Checks if sudoku is solved.
         | otherwise = concatMap solve $ newNodes n (head (snd n)) -- Recursively calls this function to solve the sudoku.

-- Solves the NRC sudoku.
-- Inspiration from https://stackoverflow.com/questions/35750123/backtrack-sudoku-in-haskell
solveNRC :: Node -> [Node]
solveNRC n  | not (consistentNRC (fst n)) = error "NRC sudoku not solvable" -- Checks if sudoku is solvable.
            | isSudokuSolved n = [n] -- Checks if sudoku is solved.
            | otherwise = concatMap solveNRC $ newNodesNRC n (head (snd n)) -- Recursively calls this function to solve the sudoku.

-- Returns the fist result of the solve sudoku function.
solveSudoku :: Sudoku -> Sudoku
solveSudoku sud = fst (head (solve (createNode sud)))

-- Returns the fist result of the solve sudokuNRC function.
solveSudokuNRC :: Sudoku -> Sudoku
solveSudokuNRC sud = fst (head (solveNRC (createNode sud)))

-- Determines if the NRC or normal solver should be called.
getSolver :: [String] -> Solver
getSolver string sud    | "nrc" `isInfixOf` head string  = solveSudokuNRC sud
                        | otherwise = solveSudoku sud

main :: IO ()
main =
    do args <- getArgs
       sud <- (readSudoku . getSudokuName) args
       printSudoku (getSolver args sud)
