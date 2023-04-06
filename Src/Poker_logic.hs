module Src.Poker_logic (
    Table(..),
    Pot(..),
    addToPot,
    getxHands
) where

import Src.Deck_logic
import Src.Hand_logic

{-| Table datatype to keep track of entire games with a Pot, deck of cards, list of hands, number of players and location of the button
-}
data Table = Table Pot [Card] [(Card, Card)] Int Int deriving (Show)

setTable min max players = Table (Pot 0 min max) (create_deck) [] players 0

--dealCards (Table pot deck _ players button) = Table pot (create_without ) 

getxHands x deck = []
    

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