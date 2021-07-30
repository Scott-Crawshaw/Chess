module Functions where
import Data.Maybe
import Data.List
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
color (Piece t c) = c

nextColor :: Color -> Color
nextColor Black = White
nextColor White = Black

activePieces :: GameState -> [PieceOnBoard]
activePieces (GameState c ps) = filter (\(PieceOnBoard piece pos) -> (color piece) == c) ps

occupantColor :: [PieceOnBoard] -> Pos -> Maybe Color
occupantColor ps pos = getColor (filter (\(PieceOnBoard piece pos1) -> pos1 == pos) ps)

getColor :: [PieceOnBoard] -> Maybe Color
getColor [] = Nothing
getColor ((PieceOnBoard piece pos):ps) = Just (color piece)