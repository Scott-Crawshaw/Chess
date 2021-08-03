{-# OPTIONS_GHC -Wall #-}

module Chess where
import Data.Maybe
import Data.List
import qualified Data.Set as Set
import DataTypes

-- Get the two adjacent positions
getTwo :: Pos -> [Maybe Pos]
getTwo S1 = [Nothing, Just S2]
getTwo S2 = [Just S1, Just S3]
getTwo S3 = [Just S2, Just S4]
getTwo S4 = [Just S3, Just S5]
getTwo S5 = [Just S4, Just S6]
getTwo S6 = [Just S5, Just S7]
getTwo S7 = [Just S6, Just S8]
getTwo S8 = [Just S7, Nothing]

-- Get the previous position
prevPosition :: Pos -> Maybe Pos
prevPosition pos = head (getTwo pos)

-- Get the next position
nextPosition :: Pos -> Maybe Pos
nextPosition pos = last (getTwo pos)

-- Get the color of a piece
color :: Piece -> Color
color (Piece _ c) = c

-- Get the opposite color
nextColor :: Color -> Color
nextColor Black = White
nextColor White = Black

-- Get all the pieces of the color whose turn it is
activePieces :: GameState -> [PieceOnBoard]
activePieces (GameState c ps) = filter (\(PieceOnBoard piece _) -> (color piece) == c) ps

-- Get the color of the piece at a given position, or Nothing if it's empty.
occupantColor :: [PieceOnBoard] -> Pos -> Maybe Color
occupantColor ps pos = getColor (filter (\(PieceOnBoard _ pos1) -> pos1 == pos) ps)

-- Get the color of the first piece in a list of pieces. Used as a helper function in stateToString.
getColor :: [PieceOnBoard] -> Maybe Color
getColor [] = Nothing
getColor ((PieceOnBoard piece _):_) = Just (color piece)

-- Move a given piece to a given position, capturing if needed.
move :: GameState -> PieceOnBoard -> Pos -> GameState
move gme piece pos = addPiece (removePieces gme piece pos) piece pos

-- Add a piece to a certain postition on the board. Helper function for move.
addPiece :: GameState -> PieceOnBoard -> Pos -> GameState
addPiece (GameState c ps) (PieceOnBoard piece _) newpos = GameState c ((PieceOnBoard piece newpos):ps)

-- Remove captured piece from board, and remove old version of capturing piece. Helper function for move.
removePieces :: GameState -> PieceOnBoard -> Pos -> GameState
removePieces gme piece pos = removePieceOnBoard (removePieceAtPos gme pos) piece

-- Remove old version of capturing piece. Helper function for removePieces.
removePieceOnBoard :: GameState -> PieceOnBoard -> GameState
removePieceOnBoard (GameState c ps) piece = GameState c (filter (\piece1 -> piece1 /= piece) ps)

-- Remove captured piece from board. Helper function for removePieces.
removePieceAtPos :: GameState -> Pos -> GameState
removePieceAtPos (GameState c ps) pos = GameState c (filter (\(PieceOnBoard _ pos1) -> pos1 /= pos) ps)

-- Convert state to string.
stateToString :: GameState -> String
stateToString (GameState c ps) = (show c) ++ ": " ++ mbyshow (getPiece S1 ps) ++ mbyshow (getPiece S2 ps) ++ mbyshow (getPiece S3 ps) ++ mbyshow (getPiece S4 ps) ++ mbyshow (getPiece S5 ps) ++ mbyshow (getPiece S6 ps) ++ mbyshow (getPiece S7 ps) ++ mbyshow (getPiece S8 ps)

-- Show piece, or show "-" for empty space. Helper function for stateToString.
mbyshow :: [PieceOnBoard] -> String
mbyshow [] = "-"
mbyshow ((PieceOnBoard piece _):_) = show piece

-- Get the piece at a given position. Helper function for stateToString.
getPiece :: Pos -> [PieceOnBoard] -> [PieceOnBoard]
getPiece pos ps = filter (\(PieceOnBoard _ pos1) -> pos1 == pos) ps

-- Convert a string to a GameState
stringToState :: String -> GameState
stringToState (c1:c2:c3:cs) = GameState (stringToColor c1 c2 c3) (stringToPieces cs)
stringToState _ = error "Invalid String"

-- Convert a string to a list of pieces. Helper function for stringToState.
stringToPieces :: String -> [PieceOnBoard]
stringToPieces str = catMaybes (map charToPOB (zip [0..] str))

-- Convert a given char to a PieceOnBoard. Helper function for stringToPieces.
charToPOB :: (Int, Char) -> Maybe PieceOnBoard
charToPOB (_, '-') = Nothing
charToPOB (i, 'K') = Just (PieceOnBoard (Piece King White) (posFromIndex i))
charToPOB (i, 'N') = Just (PieceOnBoard (Piece Knight White) (posFromIndex i))
charToPOB (i, 'R') = Just (PieceOnBoard (Piece Rook White) (posFromIndex i))
charToPOB (i, 'k') = Just (PieceOnBoard (Piece King Black) (posFromIndex i))
charToPOB (i, 'n') = Just (PieceOnBoard (Piece Knight Black) (posFromIndex i))
charToPOB (i, 'r') = Just (PieceOnBoard (Piece Rook Black) (posFromIndex i))
charToPOB _ = error "Invalid String"

-- Convert a given list index to a position. Helper function for charToPOB.
posFromIndex :: Int -> Pos
posFromIndex 0 = S1
posFromIndex 1 = S2
posFromIndex 2 = S3
posFromIndex 3 = S4
posFromIndex 4 = S5
posFromIndex 5 = S6
posFromIndex 6 = S7
posFromIndex 7 = S8
posFromIndex _ = error "Invalid String"

-- Convert a given char to a corresponding Color. Helper function for stringToState.
stringToColor :: Char -> Char -> Char -> Color
stringToColor 'B' ':' ' ' = Black
stringToColor 'W' ':' ' ' = White
stringToColor _ _ _ = error "Invalid String"

-- Get all potential moves for a given piece.
potentialSquares :: GameState -> PieceOnBoard -> [Pos]
potentialSquares gme (PieceOnBoard (Piece Rook c) pos) = goRight gme c (nextPosition pos) ++ goLeft gme c (prevPosition pos)
potentialSquares gme (PieceOnBoard (Piece t c) pos) = filter (checkSameColor c gme) (possibleMovesForPiece (PieceOnBoard (Piece t c) pos))

-- Find all valid move positions to the right of a rook.
goRight :: GameState -> Color -> Maybe Pos -> [Pos]
goRight (GameState gmec ps) c (Just pos)
    | (occupantColor ps pos) == Nothing = (pos:(goRight (GameState gmec ps) c (nextPosition pos)))
    | fromJust (occupantColor ps pos) /= c = (pos:[])
    | otherwise = []
goRight _ _ Nothing = []

-- Find all valid move positions to the left of a rook.
goLeft :: GameState -> Color -> Maybe Pos -> [Pos]
goLeft (GameState gmec ps) c (Just pos)
    | (occupantColor ps pos) == Nothing = (pos:(goLeft (GameState gmec ps) c (prevPosition pos)))
    | fromJust (occupantColor ps pos) /= c = (pos:[])
    | otherwise = []
goLeft _ _ Nothing = []

-- Check if a given color is the same as the piece color at a position. Helper function for potentialSquares.
checkSameColor :: Color -> GameState -> Pos -> Bool
checkSameColor c (GameState _ ps) pos = isSameColor c (occupantColor ps pos)

-- Check if a given color is the same as another color. Helper function for checkSameColor.
isSameColor :: Color -> Maybe Color -> Bool
isSameColor _ Nothing = False
isSameColor c (Just c1) = c1 == c

-- Get all possible moves for a knight or king, without regard to whether the space is availible. Helper function for potentialSquares.
possibleMovesForPiece :: PieceOnBoard -> [Pos]
possibleMovesForPiece (PieceOnBoard (Piece King _) pos) = catMaybes (getTwo pos)
possibleMovesForPiece (PieceOnBoard (Piece Knight _) pos) = getKnightSpots (getTwo pos)
possibleMovesForPiece _ = error "Internal Error"

-- Get all possible moves for a knight, without regard to whether the space is availible. Helper function for possibleMovesForPiece.
getKnightSpots :: [Maybe Pos] -> [Pos]
getKnightSpots ((Just prev):(Just next):_) = catMaybes [prevPosition prev, nextPosition next]
getKnightSpots (Nothing:(Just next):_) = catMaybes [nextPosition next]
getKnightSpots ((Just prev):Nothing:_) = catMaybes [prevPosition prev]
getKnightSpots _ = error "Internal Error"

-- Check if the active player can take king.
kingInCheck :: GameState -> Bool
kingInCheck gme = elem (getKingPos gme) (activePossibleMoves gme)

-- Check if the active player cannot take the king.
kingNotInCheck :: GameState -> Bool
kingNotInCheck gme = not (kingInCheck gme)

--Get all possible new positions for given gamestate. Helper function for kingInCheck.
activePossibleMoves :: GameState -> [Pos]
activePossibleMoves gme = concat (map (potentialSquares gme) (activePieces gme))

-- Get the current position of the non-active king. Helper function for kingInCheck.
getKingPos :: GameState -> Pos
getKingPos (GameState c ps) = getPos (head (filter (isCorrectKing (nextColor c)) ps))

-- Check if given piece is the king we are looking for. Helper function for getKingPos.
isCorrectKing :: Color -> PieceOnBoard -> Bool
isCorrectKing c (PieceOnBoard (Piece King c1) _) = c == c1
isCorrectKing _ _ = False

-- Get position of a piece. Helper function for getKingPos.
getPos :: PieceOnBoard -> Pos
getPos (PieceOnBoard _ pos) = pos

-- Get all legal next GameStates.
legalMoves :: GameState -> [GameState]
legalMoves (GameState c ps) = filter kingNotInCheck (concat (map (playMoves (GameState c ps)) (zip ps (map (potentialSquares (GameState c ps)) ps))))

-- Turn all legal moves into new GameStates. Helper function for legalMoves.
playMoves :: GameState -> (PieceOnBoard, [Pos]) -> [GameState]
playMoves gme (piece, ps) = map (move gme piece) ps

-- Initial position constant.
initialPosition :: GameState
initialPosition = stringToState "W: KNR--rnk"

-- Code for part 3 taken from Professor Joosten. End result is 379 GameStates. That took 0.18 seconds to calculate.
getAllStates :: [GameState]
getAllStates = nub (helper (Set.empty) [initialPosition])
helper :: Set.Set GameState -> [GameState] -> [GameState]
helper lookedAt [] = Set.toList lookedAt
helper lookedAt (e:es)
    | Set.member e lookedAt = helper lookedAt es
    | otherwise = helper (Set.insert e lookedAt) (es ++ legalMoves e)