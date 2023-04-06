module Src.Poker_logic (
    Table(..),
    Pot(..),
    addToPot
) where

import Src.Deck_logic
import Src.Hand_logic

{-| Table datatype to keep track of entire games with deck of cards, list of hands, number of players and location of the button
-}
data Table = Table [Card] [(Card, Card)] Int Int deriving (Show)

{-| Pot datatype that keeps track of the total Pot, minimum bet and maximum bet
-}
data Pot = Pot Double Double Double deriving (Show)

instance Eq Pot where
    (Pot x1 y1 z1) == (Pot x2 y2 z2) = (x1 == x2 && y1 == y2 && z1 == z2)

addToPot a (Pot x y z) = 
    if a <= z && a >= y then
        Pot (a+x) y z
    else
        Pot x y z