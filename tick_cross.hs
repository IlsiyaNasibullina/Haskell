{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
import CodeWorld
import Data.List
import Data.Maybe

-- | A mark for tic-tac-toe.
data Mark = X | O deriving (Eq, Show)

-- | A cell is either empty or has a mark.
type Cell = Maybe Mark 

-- | A board is a 2D grid of cells.
type Board = [[Cell]]

type Coord = (Int, Int)

-- | Sample 5x4 board.
sampleBoard :: Board
sampleBoard = [ [ x, o, n, o, n ], 
                [ n, o, n, x, o ], 
                [ x, n, x, n, n ], 
                [ n, o, n, x, x ] ] 
  where
  (o, x, n) = (Just O, Just X, Nothing)
  
-- | Initialise an empty NxM board.
initBoard :: (Int, Int) -> Board 
initBoard (n, m) = replicate m (replicate n Nothing)


-- | Determine a winner in a game of tic-tac-toe (if exists).
winner :: Board -> Maybe (Mark, [(Int, Int)])
winner board = getWinner (filter isLongStreak (concatMap streaks allLines))
  where
    allLines = rows ++ columns ++ diagonals
    rows = convertRows board
    columns = transpose rows
    diagonals = leftDiagonals ++ rightDiagonals
    leftDiagonals = leftDiagonalsOf rows
    rightDiagonals = leftDiagonalsOf (reverse rows)
    leftDiagonalsOf b = leftTopDiagonalsOf b ++ leftBottomDiagonalsOf b
    leftTopDiagonalsOf = transpose . zipWith drop [0..]
    leftBottomDiagonalsOf = leftTopDiagonalsOf . transpose


convertRows :: Board -> [[Maybe (Mark, (Int, Int))]]
convertRows board = zipWith convertRow [0..] board 
  where
    convertRow :: Int -> [Maybe Mark] -> [Maybe (Mark, (Int, Int))]
    convertRow i row = zipWith (addCoord i) row [0..]

    addCoord :: Int -> Maybe Mark -> Int -> Maybe (Mark, (Int, Int))
    addCoord _ Nothing _ = Nothing
    addCoord i (Just mark) j = Just (mark, (i, j))



-- | Get all consequent streaks ignoring 'Nothing'.
streaks :: Eq a => [Maybe (a, (Int, Int))] -> [(Int, (a, [(Int, Int)]))]
streaks [] = []
streaks (Nothing : xs) = streaks xs
streaks (Just (x, coords) : xs) = (1 + length ys, (x, coords : extractCoords ys)) : streaks zs
  where
    extractCoords :: [Maybe (a, (Int, Int))] -> [(Int, Int)]
    extractCoords [] = []
    extractCoords (Just (_, coord) : rest) = coord : extractCoords rest
    extractCoords (Nothing : rest) = extractCoords rest
    
    extractMarks :: [Maybe (a, (Int, Int))] -> [Maybe a]
    extractMarks [] = []
    extractMarks (Just (p, coord) : rest) = (Just p) : extractMarks rest
    extractMarks (Nothing : rest) = Nothing : extractMarks rest
    (ys, zs) = (take (length first) xs, drop (length first) xs)
    (first, second) = span (== Just x) (extractMarks xs)

    
    
-- | Determine is a streak is long enough to be a winning streak.
isLongStreak :: (Int, a) -> Bool
isLongStreak (i, _) = i >= 3

-- | Get a winning mark (if exists).
getWinner :: [(Int, a)] -> Maybe a
getWinner = listToMaybe . map snd


-- | Draw a rectangular board.
drawBoard :: Board -> Picture
drawBoard board = (drawWin board) 
                             <> (pictures (concatMap drawRow (zip [0..] board)))
  where 
  drawRow :: (Int, [Cell]) -> [Picture]
  drawRow (i, row) = concatMap (drawCell i) (zip [0..] row)
  
  drawCell :: Int -> (Int, Cell) -> [Picture]
  drawCell i (j, cell) = (drawCellAt j i cell) : []
  
  drawWin :: Board -> Picture
  drawWin b = draw (winner b) 
  
  draw :: Maybe (Mark, [Coord]) -> Picture
  draw (Just (a, ((x, y) : xy))) = colored red (drawCellAt y x (Just a)) 
                                  <> draw (Just (a, (xy)))
  draw (Just (_, [])) = blank
  draw _ = blank


-- | Draw a single mark (X or O).
drawMark :: Mark -> Picture
drawMark X = scaled 0.4 0.4 (rotated (pi/4)
  (solidRectangle 0.5 2 <> solidRectangle 2 0.5))
drawMark O = thickCircle 0.2 0.3

-- | Draw one board cell at given coordinates.
drawCellAt :: Int -> Int -> Cell -> Picture
drawCellAt i j cell = translated x y
  (rectangle 1 1 <> cellPicture) 
  where
    x = fromIntegral i
    y = fromIntegral j
    cellPicture = case cell of
        Nothing -> blank
        Just m  -> drawMark m


-- | Try place a mark a given position on a board.
-- The type of mark is determined automatically.
-- When the game is over (a winner exists) no marks are placed. 
putMarkAt :: (Int, Int) -> Board -> Board
putMarkAt (i', j') board 
  | i < 1 || j < 1 || (winner board) /= Nothing = board
  |((countMark X board) + (countMark O board)) == sum (map length board) = board
  | nextStep board == Just X = updateAt i (updateAt j makeStepX) board
  | otherwise =  updateAt i (updateAt j makeStepO) board
  where 
    i = j'+ 1
    j = i'+ 1



-- | Try update an element at a given position in a list.
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt j func list = (take (j-1) list) 
                        ++ (map func (take 1 (drop (j-1) list))) 
                        ++ (drop j list)


makeStepX :: Cell -> Cell
makeStepX Nothing = Just X
makeStepX cell = cell

makeStepO :: Cell -> Cell
makeStepO Nothing = Just O
makeStepO cell = cell


-- Decide, whether to put X or O
nextStep :: Board -> Cell
nextStep board 
  | (countMark X board) == (countMark O board) = Just X
  | otherwise = Just O

countMark :: Mark -> Board -> Int
countMark mark board = sum (map (countLine mark) board)

countLine :: Mark -> [Cell] -> Int
countLine mark line = length (filter (== Just mark) line)

-- | Handle mouse clicks to put marks.
handleGame :: Event -> Board -> Board
handleGame (PointerPress mouse) = putMarkAt (pointToCoords mouse)
handleGame _ = id

-- | Convert mouse position into board coordinates.
pointToCoords :: Point -> (Int, Int)
pointToCoords (x, y) = (round x, round y)

-- | Run a tic-tac-toe game with a sample starting board.
ticTacToe :: Board -> IO ()
ticTacToe board = activityOf board handleGame drawBoard
  where
    updateBoard _dt = id

main :: IO()
main = ticTacToe (initBoard (3, 3))
