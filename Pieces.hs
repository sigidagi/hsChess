-- module defines chess pieces and their moves..

module Pieces where

data Piece = Piece {piece::PieceType, color::PieceColor} deriving (Eq)
data PieceType = Rook | Knight | Bishop | King | Queen | Pawn deriving (Eq)
data PieceColor = Black | White deriving Eq

instance Show PieceColor where
 show Black = "B"
 show White = "W"

instance Show PieceType where
 show King = "K"
 show Queen = "Q"
 show Knight = "N"
 show Rook = "R"
 show Bishop = "B"
 show Pawn = "P"

type Field = Maybe Piece
type Board = [[Field]]
type Position = (Int, Int)

---- some board functions -----

fields :: [Position]
fields = [1..8] >>= \r -> [1..8] >>= \c -> return (r,c)


dirStright :: [Position]
dirStright = [(1,0), (0,1), (0,-1), (-1,0)]  

dirDiagonal :: [Position]
dirDiagonal = [(1,1), (-1,1), (1,-1), (-1,-1)]  

diagonals :: [Position]
diagonals =  dirDiagonal >>= \n -> [1..8] >>= \p -> return (p * fst n, p * snd n)

strights :: [Position]
strights = dirStright >>= \n -> [1..8] >>= \p -> return (p * fst n, p * snd n)

onBoard :: Position -> Bool
onBoard (r, c) = (r,c) `elem` fields 

isWhite :: Position -> Board -> Bool
isWhite pos b | checkColor pos b == Just White = True
              | otherwise = False

isBlack :: Position -> Board -> Bool
isBlack pos b | checkColor pos b == Just Black = True
              | otherwise = False


getDirection :: Position -> Board -> Int
getDirection pos b | isWhite pos b = 1
                   | isBlack pos b = -1
                   | otherwise = 0

-- offset (1,1) - 
checkColor :: Position -> Board -> Maybe PieceColor
checkColor pos b | onBoard pos =  b!!(fst offset)!!(snd offset)  >>= \p -> return (color p) 
                 | otherwise = Nothing
                 where offset = (fst pos - 1, snd pos - 1)

-- checks if two fields contains pieces with oposite color. 
isOpositeColor :: Position -> Position -> Board -> Bool 
isOpositeColor p1 p2 b | isWhite p1 b && isBlack p2 b = True
                       | isBlack p1 b && isWhite p2 b = True
                       | otherwise = False

-- if direction is changed - only the first typle elemenet sign should be changed to oposite
forwardOne :: Position -> Board -> [Position]
forwardOne (r,c) b = [ (r + 1 * getDirection (r,c) b, c) ]

forwardTwo :: Position -> Board -> [Position]
forwardTwo (r,c) b | isWhite (r,c) b && r == 2 = [(r+2,c)] 
                   | isBlack (r,c) b && r == 7 = [(r-2,c)]
                   | otherwise = []

-- (1,-1)
diagOneLeft :: Position ->Board -> [Position]
diagOneLeft (r,c) b | isOpositeColor (r,c) (r + fst toLeft, c + snd toLeft) b = [(r + fst toLeft, c + snd toLeft)] 
                    | otherwise = []
                    where toLeft = (1*getDirection (r,c) b, -1)

-- (1,1)
diagOneRight :: Position ->Board -> [Position]
diagOneRight (r,c) b | isOpositeColor (r,c) (r + fst toRight, c + snd toRight) b = [(r + fst toRight, c + snd toRight)] 
                     | otherwise = []
                     where toRight = (1*getDirection (r,c) b, 1)


-- moves of chess pieces ----
-----------------------------------------------------

pawnMoves :: Position -> Board -> [Position]
pawnMoves pos b = filter onBoard $ forwardOne pos b ++ forwardTwo pos b ++ diagOneRight pos b ++ diagOneLeft pos b 

knightMoves :: Position -> [Position]
knightMoves (r,c) = filter onBoard
                 [(r+1,c+2), (r+1,c-2), (r-1,c+2), (r-1,c-2), (r+2,c+1), (r+2,c-1), (r-2,c+1), (r-2,c-1)]

rookMoves :: Position -> [Position]
rookMoves (r,c) = filter onBoard [ (r + fst x, c + snd x) | x <- strights ]

bishopMoves :: Position -> [Position]
-- using list comprehension
bishopMoves (r,c) = filter onBoard [ (r + fst x, c + snd x ) | x <- diagonals ]
-- using monads. choose which method do you like.
--bishopMoves (r,c) = filter onBoard $ diagonals >>= \n -> return (r + fst n, c + snd n)

queenMoves :: Position -> [Position]
queenMoves (r,c) = filter onBoard [ (r + fst x, c + snd x ) | x <- (diagonals ++ strights) ] 

kingMoves :: Position -> [Position]
kingMoves (r,c) = filter onBoard [ (r + fst x, c + snd x ) | x <- (dirDiagonal ++ dirStright) ]

-------------------------------------------------------

-- -----------------------------------------------------------------
initialBoard :: Board
initialBoard = 
                [[Just (Piece Rook White), Just (Piece Knight White), Just (Piece Bishop White), Just (Piece Queen White), Just (Piece King White), Just (Piece Bishop White), Just (Piece Knight White), Just (Piece Rook White)],
                [Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White)],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black)],
                [Just (Piece Rook Black), Just (Piece Knight Black), Just (Piece Bishop Black), Just (Piece Queen Black), Just (Piece King Black), Just (Piece Bishop Black), Just (Piece Knight Black), Just (Piece Rook Black)]]

emptyBoard :: Board
emptyBoard = [[Nothing | _<- [1..8]] | _<- [1..8]]

----------------------------------------------------------------
-- The value of the king has to be infinity
-- the threshold has to be infinity minus
-- the maximal material value of one player
infinity = 1000::Int
threshold = 900::Int

valuePiece::PieceType->Int
valuePiece Pawn = 1
valuePiece Rook = 5
valuePiece Knight = 3
valuePiece Bishop = 3
valuePiece Queen = 9
valuePiece King = infinity

-- aggregated value of material of both players
boardAnalysis::Board->(Int,Int)
boardAnalysis = foldl addValue (0,0) . concat 
   where addValue points Nothing = points
         addValue (pw,pb) (Just (Piece a f)) | f == Black = (pw, pb + valuePiece a)
                                             | otherwise = (pw + valuePiece a, pb)

evalBoard::Board->Int
evalBoard b = let (p1,p2) = boardAnalysis b in p1-p2

