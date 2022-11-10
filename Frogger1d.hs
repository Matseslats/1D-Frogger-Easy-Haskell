module Main where
import Data.Sequence

type Squares = [Int]
type GameSettings = [Int]
type VisitedSquares = [Bool]
-- paygame takes given inputs and runs simFrogger with some extra values 
-- (Prelude.take (squareNo+1) $ repeat False) makes an array of Bools that are all False, same size
--      as squares, but +1 because it is zero indexed 
playgame :: GameSettings -> Squares -> IO ()
playgame [squareNo, index, magic] squares = simFrogger (squareNo, index, magic, 0) squares (Prelude.take (squareNo+1) $ repeat False)

simFrogger :: (Int, Int, Int, Int) -> Squares -> VisitedSquares -> IO ()
simFrogger (squareNo, index, magic, jumps) squares visitations
    -- Check for finished values 
    | index-1 < 0               = putStrLn ("left\n" ++ show jumps)
    | index > squareNo          = putStrLn ("right\n" ++ show jumps)
    | thisSqValue == magic      = putStrLn ("magic\n" ++ show jumps)
    | thisVisit                 = putStrLn ("cycle\n" ++ show jumps)
    -- Program isnt finished, so run again, and again until finished value reached
    | otherwise                 = simFrogger (squareNo, newIindex, magic, jumps+1) squares newVisitations
    where
        thisSqValue = at (index-1) squares -- squares are one indexed. Get val at pos index
        newIindex = index + thisSqValue -- Add square val to index
        thisVisit = atB (index) visitations -- bools are zero indexed. Get val at pos index
        newVisitations = updateBoolList index True visitations -- Update list of visited squares

-- Func takes a list of bools and changes the index to a desired value
updateBoolList :: Int -> Bool -> VisitedSquares -> VisitedSquares
updateBoolList 0 val [x] = [val]
updateBoolList 0 val (x:xs) = val:xs
updateBoolList n val (x:xs) = x: updateBoolList (n-1) val xs

-- Get int at index from array
at      :: Int -> [Int] -> Int
at n [] = error "Cannot get index of empty array"
at 0 (x:xs) = x
at n (x:xs) = if n < 1
                then 0
                else at (n-1) xs

-- Get bool at index from array
atB      :: Int -> [Bool] -> Bool
atB n [] = error "Cannot get index of empty array"
atB 0 (x:xs) = x
atB n (x:xs) = if n < 1
                then False
                else atB (n-1) xs

-- Turn string into int array
parseInts :: String -> [Int]
parseInts str = map read (words str)

main = do
    -- read just a line and convert to a list of Ints
    gameSettings <- fmap parseInts getLine
    squareSettings <- fmap parseInts getLine
    -- Start the game
    playgame gameSettings squareSettings
