module Deck_logic (
    Card(..),
    Hand_value(..),
    Hand,
    Deck,
    create_deck,
    shuffle,
    get_hand,
    create_without,
    remove_cards,
    card_value
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

data Hand_value = Hand_value [Card] Double deriving (Show)

instance Eq Hand_value where
    (Hand_value ca va) == (Hand_value cb vb) = va == vb

instance Ord Hand_value where
    compare (Hand_value ca va) (Hand_value cb vb) = compare va vb

type Hand = (Card, Card)

type Deck = [Card]

create_deck :: Deck
create_deck = [Card a b | a <- [Two ..], b <- [Clubs ..]]

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

card_value :: Card -> Double
card_value (Card value suit) = case value of
    Two -> 1/13
    Three -> 2/13
    Four -> 3/13
    Five -> 4/13
    Six -> 5/13
    Seven -> 6/13
    Eight -> 7/13
    Nine -> 8/13
    Ten -> 9/13
    Jack -> 10/13
    Queen -> 11/13
    King -> 12/13
    Ace -> 1