module Main where

import           Control.Exception   (SomeException, try)
import qualified Data.Map.Strict     as Map
import           Data.Maybe
import           System.Console.ANSI (clearScreen)

type Coor = (Int,Int)

type Board = Map.Map Coor Piece

data Color = White | Black

instance Show Color where
  show c = case c of
    White -> "White"
    Black -> "Black"

instance Eq Color where
  White == White = True
  Black  == Black  = True
  _     == _     = False

data TypePiece =  Pawn | Rook | Knight | Bishop | Queen | King deriving (Eq)

data Piece = Piece { typeP :: TypePiece
                   , color ::  Color} deriving (Eq)

instance Show Piece where
  show p = case p of
    Piece Rook Black   -> "R_B"
    Piece Knight Black -> "K_B"
    Piece Bishop Black -> "B_B"
    Piece King Black   -> "KOB"
    Piece Queen Black  -> "QEB"
    Piece Pawn Black   -> "P_B"
    Piece Rook White   -> "R_W"
    Piece Knight White -> "K_W"
    Piece Bishop White -> "B_W"
    Piece King White   -> "KOW"
    Piece Queen White  -> "QEW"
    Piece Pawn White   -> "P_W"

changeColor :: Color -> Color
changeColor Black = White
changeColor White = Black

boardToString :: Board -> String
boardToString board = (addRowNumber . unlines . addLineNumber) chars
    where
      addRowNumber = (++) (" " ++ concatMap ( (\s -> "  " ++ s ++ " ") . show) [(0::Int)..7] ++ "\n")
      addLineNumber = zipWith (++) (map ( (++ " ") . show) [(0::Int)..7])
      chars = [unwords [maybe " . " show (Map.lookup (x, y) board)
        | x <- [0..7]]
        | y <- [0..7]]

boardStart :: Board
boardStart = Map.fromList [((0,0), Piece Rook Black),
                            ((1,0), Piece Knight Black),
                            ((2,0), Piece Bishop Black),
                            ((3,0), Piece King Black),
                            ((4,0), Piece Queen Black),
                            ((5,0), Piece Bishop Black),
                            ((6,0), Piece Knight Black),
                            ((7,0), Piece Rook Black),
                            ((0,1), Piece Pawn Black),
                            ((1,1), Piece Pawn Black),
                            ((2,1), Piece Pawn Black),
                            ((3,1), Piece Pawn Black),
                            ((4,1), Piece Pawn Black),
                            ((5,1), Piece Pawn Black),
                            ((6,1), Piece Pawn Black),
                            ((7,1), Piece Pawn Black),
                            ((0,6), Piece Pawn White),
                            ((1,6), Piece Pawn White),
                            ((2,6), Piece Pawn White),
                            ((3,6), Piece Pawn White),
                            ((4,6), Piece Pawn White),
                            ((5,6), Piece Pawn White),
                            ((6,6), Piece Pawn White),
                            ((7,6), Piece Pawn White),
                            ((0,7), Piece Rook White),
                            ((1,7), Piece Knight White),
                            ((2,7), Piece Bishop White),
                            ((3,7), Piece King White),
                            ((4,7), Piece Queen White),
                            ((5,7), Piece Bishop White),
                            ((6,7), Piece Knight White),
                            ((7,7), Piece Rook White)
                            ]

inBoard :: Coor -> Bool
inBoard (x,y)= xIn && yIn
  where
    xIn = x `elem` [0..7]
    yIn = y `elem` [0..7]

validMovePiece :: Piece -> Coor -> Coor -> Bool
validMovePiece p (x1,y1) (x2,y2) = let
  (dx,dy) = (x2 - x1, y2 - y1)
  in case p of
      (Piece Rook _)     -> dx == 0 || dy == 0
      (Piece Knight _) -> abs dx + abs dy == 3 && abs dx /= 3 && abs dy /= 3
      (Piece Bishop _)      -> abs dx == abs dy
      (Piece King _)      -> abs dx + abs dy <= 2 && abs dx /= 2 && abs dy /= 2
      (Piece Queen _)    -> (abs dx == abs dy) || (dx == 0 || dy == 0)
      (Piece Pawn White) -> dx == 0 && dy == -1
      (Piece Pawn Black)  -> dx == 0 && dy == 1

validCapturePiece :: Piece -> Coor -> Coor -> Bool
validCapturePiece p (x1,y1) (x2,y2) = let
  (dx,dy) = (x2 - x1, y2 - y1)
  in case p of
    (Piece Pawn White) -> (dx == 1 || dx == -1) && dy == -1
    (Piece Pawn Black)  -> (dx == 1 || dx == -1) && dy == 1
    _                  -> validMovePiece p (x1,y1) (x2,y2)

validMove :: Board -> Color -> Coor -> Coor -> Bool
validMove board player origin final = inBoard origin && inBoard final
                      && isJust  (Map.lookup origin board)
                      && player == (color . fromJust) (Map.lookup origin board)
                      &&  maybe True (player /=)
                          (fmap color (Map.lookup final board) )
                      && if  isJust (Map.lookup final board) && color (fromJust (Map.lookup final board)) /= player
                          then validCapturePiece (fromJust (Map.lookup origin board)) origin final
                          else validMovePiece  (fromJust (Map.lookup origin board)) origin final
                      && clearRoad board origin final

clearRoad :: Board -> Coor -> Coor -> Bool -- w -> x1 , x -> y1 , y -> x2 , z -> y2
clearRoad board (x1,y1) (x2,y2) = (case board Map.! (x1,y1) of
  (Piece Knight _) -> True
  (Piece Pawn _)   -> True
  (Piece King _)   -> True
  (Piece Rook _)   -> line_row
  (Piece Bishop _) -> diagonnal
  (Piece Queen _)  -> if x1 == x2 || y1 == y2 then line_row else diagonnal)
    where
      line_row= case () of _
                            | x1 == x2 && y1 > y2 -> null [(x1,n) | n <- [y1+1, (y1 - 1)..y2-1] , isJust $ Map.lookup (x1,n) board ]
                            | x1 == x2 && y1 < y2 -> null [(x1,n) | n <- [y1+1..y2-1] , isJust $ Map.lookup (x1,n) board ]
                            | x1 > x2 && y1 == y2 -> null [(n,y1) | n <- [x1+1, (x1 - 1)..x2-1] , isJust $ Map.lookup (n,y1) board ]
                            | x1 < x2 && y1 == y2 -> null [(n,y1) | n <- [x1+1..x2-1] , isJust $ Map.lookup (n,y1) board ]
                            | otherwise -> False
      diagonnal = let
        dx = x2 - x1
        dy = y2 - y1
        in case () of _
                       | dx == dy && dx > 0 -> null [ (x1+n,y1+n) | n <- [1..dx] , isJust $ Map.lookup (x1+n,y1+n) board ]
                       | dx == dy && dx < 0 -> null [ (x1+n,y1+n) | n <- [-1,-2..dx] , isJust $ Map.lookup (x1+n,y1+n) board ]
                       | dx > dy -> null [ (x1+n,y1-n) | n <- [1..dx] ,  isJust $ Map.lookup (x1+n,y1-n) board ]
                       | dx < dy -> null [ (x1-n,y1+n) | n <- [1..(-dx)] ,  isJust $ Map.lookup (x1-n,y1+n) board ]
                       | otherwise -> False


changeBoard :: Board -> Coor -> Coor -> Board
changeBoard board origin final = Map.delete origin $ Map.insert final (board Map.! origin) board

getValidInput :: Board -> Color -> IO (Coor, Coor)
getValidInput board player = do
  putStrLn $ "Format is (x,y) \nIt is the turn of the " ++ show player ++
            " player.\nEnter origin position"
  strOrigin <- getLine
  putStrLn "Enter final position"
  strFinal <- getLine
  tryOrigin <- try (readIO strOrigin) :: IO (Either SomeException (Int,Int))
  tryFinal  <- try (readIO strFinal)  :: IO (Either SomeException (Int,Int))
  case (tryOrigin, tryFinal) of
    (Left _, _ ) -> putStrLn "Invalid synthax for origin position" >> getValidInput board player
    ( _ ,Left _) -> putStrLn "Invalid synthax for final position" >> getValidInput board player
    (Right origin, Right final) -> if not $ validMove board player origin final
      then putStrLn "Invalid move" >> getValidInput board player
      else return (origin, final)

gameLoop :: Board -> Color -> IO ()
gameLoop board player = do
  clearScreen
  putStrLn $ boardToString board
  (origin , final) <- getValidInput board player
  let
    board' = changeBoard board origin final
    in case () of _
                    | Piece King Black `notElem` Map.elems board' -> putStrLn "White has won!"
                    | Piece King White `notElem` Map.elems board' -> putStrLn "Black has won!"
                    | otherwise                                   ->  gameLoop board' (changeColor player)

main :: IO ()
main = gameLoop boardStart White
