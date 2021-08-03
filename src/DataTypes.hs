{-# OPTIONS_GHC -Wall #-}

module DataTypes where

data Pos = S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 deriving (Eq, Ord)

data Piece = Piece Type Color deriving (Eq, Ord)
data Color = Black | White deriving (Eq, Ord)
data Type = King | Knight | Rook deriving (Eq, Ord)

data PieceOnBoard = PieceOnBoard Piece Pos deriving (Eq, Ord)

data GameState = GameState Color [PieceOnBoard] deriving (Eq, Ord)

-- show instances
instance Show Piece where
    show (Piece King White) = "K"
    show (Piece Knight White) = "N"
    show (Piece Rook White) = "R"
    show (Piece King Black) = "k"
    show (Piece Knight Black) = "n"
    show (Piece Rook Black) = "r"

instance Show Color where
    show Black = "B"
    show White = "W"