module Deck_logic (
    Card,
    Hand,
    Deck,
    create_deck,
    shuffle,
    get_hand,
    create_without,
    remove_cards
) where

import System.Random

data Suit = Clubs | Spades | Hearts | Diamonds deriving (Show, Enum, Eq);

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Enum, Eq, Ord);

--type Card = (Value, Suit)
data Card = Card Value Suit deriving (Show)

instance Eq Card where
    (Card va sa) == (Card vb sb) = va == vb

instance Ord Card where
    compare (Card va _) (Card vb _) = compare va vb

type Hand = (Card, Card)

type Deck = [Card]

create_deck :: Deck
create_deck = [Card a b | a <- [Ace ..], b <- [Clubs ..]]

create_without :: [Card] -> [Card]
create_without a = filter (\x -> (notElem x a)) create_deck

shuffle :: [a] -> IO [a]
shuffle x = if length x < 2 then return x else do
    i <- System.Random.randomRIO (0, length(x)-1)
    r <- shuffle (take i x ++ drop (i+1) x)
    return (x!!i : r)

get_hand :: [a] -> (a, a)
get_hand (x:xs) = (x, head xs)

remove_cards :: Int -> Deck -> Deck
remove_cards a deck = drop a deck