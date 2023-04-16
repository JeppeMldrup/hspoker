module Src.Poker_logic (
    Table(..),
    Pot(..),
    addToPot,
    dealHands,
    dealFlop,
    dealTurn,
    dealRiver,
    shuffleDeckOnTable
) where

import Src.Deck_logic
import Src.Hand_logic

{-| Table datatype to keep track of entire games with a Pot, deck of cards, list of hands, number of players and location of the button
-}
data Table = Table {
    pot :: Pot,
    deck :: [Card],
    hands :: [(Card, Card)],
    pile :: [Card],
    buttonLoc :: Int,
    numPlayers :: Int,
    turn :: Int
} deriving (Show)

instance Eq Table where
    (Table a b c d e f g) == (Table a1 b1 c1 d1 e1 f1 g1) = (a == a1 && b == b1 && c == c1 && d == d1 && e == e1 && f == f1 && g == g1)

{-| Pot datatype that keeps track of the total Pot, minimum bet and maximum bet
-}
data Pot = Pot {
    total :: Double,
    min :: Double,
    max :: Double
} deriving (Show)

instance Eq Pot where
    (Pot x1 y1 z1) == (Pot x2 y2 z2) = (x1 == x2 && y1 == y2 && z1 == z2)

addToPot a (Pot x y z) = 
    if a <= z && a >= y then
        Pot (a+x) y z
    else
        Pot x y z

{-| dealHands function that takes a table record and returns a new table record with all hands dealt
-}
dealHands (Table a deck c d e f g) = Table a (drop (f*2) deck) (take f (get_hands deck)) d e f g

{-| dealFlop function to deal the flop from deck onto the pile
-}
dealFlop (Table a b c d e f g) = Table a (drop 3 b) c (take 3 b) e f g

{-| dealTurn function to deal the turn from the deck onto the pile
-}
dealTurn (Table a b c d e f g) = Table a (drop 1 b) c ((head b) : d) e f g
dealRiver = dealTurn

shuffleDeckOnTable (Table a b c d e f g) = do
    shuffled <- (shuffle b)
    return (Table a shuffled c d e f g)