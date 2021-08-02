{-# OPTIONS_GHC -Wall #-}

module Functions where
import Data.Maybe
import DataTypes

getTwo :: Pos -> [Maybe Pos]
getTwo S1 = [Nothing, Just S2]
getTwo S2 = [Just S1, Just S3]
getTwo S3 = [Just S2, Just S4]
getTwo S4 = [Just S3, Just S5]
getTwo S5 = [Just S4, Just S6]
getTwo S6 = [Just S5, Just S7]
getTwo S7 = [Just S6, Just S8]
getTwo S8 = [Just S7, Nothing]

prevPosition :: Pos -> Maybe Pos
prevPosition pos = head (getTwo pos)

nextPosition :: Pos -> Maybe Pos
nextPosition pos = last (getTwo pos)

color :: Piece -> Color
color (Piece _ c) = c

nextColor :: Color -> Color
nextColor Black = White
nextColor White = Black

activePieces :: GameState -> [PieceOnBoard]
activePieces (GameState c ps) = filter (\(PieceOnBoard piece _) -> (color piece) == c) ps

occupantColor :: [PieceOnBoard] -> Pos -> Maybe Color
occupantColor ps pos = getColor (filter (\(PieceOnBoard _ pos1) -> pos1 == pos) ps)

getColor :: [PieceOnBoard] -> Maybe Color
getColor [] = Nothing
getColor ((PieceOnBoard piece _):_) = Just (color piece)

move :: GameState -> PieceOnBoard -> Pos -> GameState
move gme piece pos = addPiece (removePieces gme piece pos) piece pos

addPiece :: GameState -> PieceOnBoard -> Pos -> GameState
addPiece (GameState c ps) (PieceOnBoard piece _) newpos = GameState c ((PieceOnBoard piece newpos):ps)

removePieces :: GameState -> PieceOnBoard -> Pos -> GameState
removePieces gme piece pos = removePieceOnBoard (removePieceAtPos gme pos) piece

removePieceOnBoard :: GameState -> PieceOnBoard -> GameState
removePieceOnBoard (GameState c ps) piece = GameState c (filter (\piece1 -> piece1 /= piece) ps)

removePieceAtPos :: GameState -> Pos -> GameState
removePieceAtPos (GameState c ps) pos = GameState c (filter (\(PieceOnBoard _ pos1) -> pos1 /= pos) ps)

stateToString :: GameState -> String
stateToString (GameState c ps) = (show c) ++ ": " ++ mbyshow (getPiece S1 ps) ++ mbyshow (getPiece S2 ps) ++ mbyshow (getPiece S3 ps) ++ mbyshow (getPiece S4 ps) ++ mbyshow (getPiece S5 ps) ++ mbyshow (getPiece S6 ps) ++ mbyshow (getPiece S7 ps) ++ mbyshow (getPiece S8 ps)

mbyshow :: [PieceOnBoard] -> String
mbyshow [] = "-"
mbyshow ((PieceOnBoard piece _):_) = show piece

getPiece :: Pos -> [PieceOnBoard] -> [PieceOnBoard]
getPiece pos ps = filter (\(PieceOnBoard _ pos1) -> pos1 == pos) ps

stringToState :: String -> GameState
stringToState (c1:c2:c3:cs) = GameState (stringToColor c1 c2 c3) (stringToPieces cs)
stringToState _ = error "Invalid String"

stringToPieces :: String -> [PieceOnBoard]
stringToPieces str = catMaybes (map charToPOB (zip [0..] str))

charToPOB :: (Int, Char) -> Maybe PieceOnBoard
charToPOB (_, '-') = Nothing
charToPOB (i, 'K') = Just (PieceOnBoard (Piece King White) (posFromIndex i))
charToPOB (i, 'N') = Just (PieceOnBoard (Piece Knight White) (posFromIndex i))
charToPOB (i, 'R') = Just (PieceOnBoard (Piece Rook White) (posFromIndex i))
charToPOB (i, 'k') = Just (PieceOnBoard (Piece King Black) (posFromIndex i))
charToPOB (i, 'n') = Just (PieceOnBoard (Piece Knight Black) (posFromIndex i))
charToPOB (i, 'r') = Just (PieceOnBoard (Piece Rook Black) (posFromIndex i))
charToPOB _ = error "Invalid String"

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

stringToColor :: Char -> Char -> Char -> Color
stringToColor 'B' ':' ' ' = Black
stringToColor 'W' ':' ' ' = White
stringToColor _ _ _ = error "Invalid String"