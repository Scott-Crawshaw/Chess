{-# OPTIONS_GHC -Wall #-}

module DataTypes where

data Pos = S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 deriving Eq

data Piece = Piece Type Color deriving Eq
data Color = Black | White deriving Eq
data Type = King | Knight | Rook deriving Eq

data PieceOnBoard = PieceOnBoard Piece Pos deriving Eq

data GameState = GameState Color [PieceOnBoard]