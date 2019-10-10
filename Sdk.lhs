========GAC========
1: Procedure GAC(V,dom,C) 
2:     Inputs
3:         V: a set of variables ===> each position of the grid
4:         dom: a function such that dom(X) is the domain of variable X  ====> {1,2,3,4,5,6,7,8,9}
5:         C: set of constraints to be satisfied ====> {function: firstConstraint, function: sndConstraint, function: thirdConstraint}

\begin{code}

module Sdk where
import Data.List

type Field  = Int
type Row    = [Field]
type Sudoku = [Row]

sudoku :: Sudoku
sudoku = [[8, 0, 1, 3, 4, 0, 0, 0, 0],
          [4, 3, 0, 8, 0, 0, 1, 0, 7],
          [0, 0, 0, 0, 6, 0, 0, 0, 3],
          [2, 0, 8, 0, 5, 0, 0, 0, 9],
          [0, 0, 9, 0, 0, 0, 7, 0, 0],
          [6, 0, 0, 0, 7, 0, 8, 0, 4],
          [3, 0, 0, 0, 1, 0, 0, 0, 0],
          [1, 0, 5, 0, 0, 6, 0, 4, 2],
          [0, 0, 0, 0, 2, 4, 3, 0, 8]]

\end{code}

=====CONSTRAINTS=====
1- Check the 3x3 grid, return False if num is not a solution.
2- Check one column, return False if num is not a solution.
3- Check one row, return False if num is not a solution

\begin{code}

createGrids :: Sudoku -> Sudoku
createGrids [[]] = [[]]
createGrids matrix = map (take 3) (matrix)

createAllGrids :: Sudoku -> Sudoku
createAllGrids [[]] = [[]]
createAllGrids matrix = createGrids(matrix) ++ createGrids (map (drop 3) (matrix)) ++ createGrids (map (drop 6) (matrix))

concatGrids' :: Sudoku -> Sudoku
concatGrids' [[]] = [[]]
concatGrids' matrix = concat (step' matrix) : concatGrids' (step matrix)
    where
      step a
       | length a > 0 = drop 3 a
       | otherwise = [[]]
      step' a
       | length a > 0 = take 3 a
       | otherwise = [[]]

concatGrids :: Sudoku -> Sudoku
concatGrids matrix = reverse $ drop 2 $ reverse $ concatGrids' matrix
--tem que chamar assim: concatGrids( createAllGrids sudoku)

findRow :: Int -> Int
findRow = undefined

checkGrid :: Int -> Bool
checkGrid indexGrid = undefined

sndConstraint :: Sudoku -> Int -> Int -> Bool
sndConstraint [[]] _ _ = True
sndConstraint column solution row
    | elem solution $ (!!) (transpose column) row = False
    | otherwise = True

thirdConstraint :: Row -> Int -> Bool
thirdConstraint [] _ = True
thirdConstraint row num
    | elem num row = False
    | otherwise = True

{-
firstConstraint :: Sudoku -> Int -> Bool
firstConstraint [[]] _ = True
firstConstraint matrix indexNum indexRow -}

--Check wich one of the 3 bricks the 0 are
{-
findGrid :: Int -> Int
findGrid indexRow 
  | indexRow < 3 = findRow
  | indexRow > 5 = findRow
  | otherwise = findRow-}
--Check wich one of the rows the 0 are, discovering the real grid 

\end{code}

