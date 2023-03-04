module Deck_logic (
    Card,
    Hand,
    Deck,
    create_deck,
    shuffle,
    create_without
) where

import System.Random

data Suit = Clubs | Spades | Hearts | Diamonds deriving (Show, Enum, Eq);

data Value = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Show, Enum, Eq);

type Card = (Value, Suit)

type Hand = (Card, Card)

type Deck = [Card]

create_deck :: Deck
create_deck = [(a, b) | a <- [Ace ..], b <- [Clubs ..]]

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