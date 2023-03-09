{- |
Module: Deck_logic
Description: Module containing all the functionality regarding simulating decks of cards
-}

module Src.Deck_logic (
    Card(..),
    Hand_value(..),
    Hand,
    Deck,
    Suit(..),
    Value(..),
    create_deck,
    shuffle,
    get_hand,
    create_without,
    remove_cards,
    card_value
) where

import System.Random

{-| Suit datatype for cards
-}
data Suit = Clubs | Spades | Hearts | Diamonds deriving (Show, Enum, Eq);

{-| Value datatype for cards
-}
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Enum, Eq, Ord);

{-| Card datatype using Value and Suit
-}
data Card = Card Value Suit deriving (Show)

instance Eq Card where
    (Card va sa) == (Card vb sb) = va == vb

instance Ord Card where
    compare (Card va _) (Card vb _) = compare va vb

{-| Hand value datatype containing a list of cards and a score of it
-}
data Hand_value = Hand_value [Card] Double deriving (Show)

instance Eq Hand_value where
    (Hand_value ca va) == (Hand_value cb vb) = va == vb

instance Ord Hand_value where
    compare (Hand_value ca va) (Hand_value cb vb) = compare va vb

{-| Hand datatype
-}
type Hand = (Card, Card)

{-| Deck datatype
-}
type Deck = [Card]

{-| A function to create a new deck of cards
-}
create_deck :: Deck
create_deck = [Card a b | a <- [Two ..], b <- [Clubs ..]]

{-| A function to create a new deck of cards excluding a given list of cards
-}
create_without :: [Card] -> [Card]
create_without a = filter (\x -> (notElem x a)) create_deck

{-| A function that shuffles a deck of cards into a random position
-}
shuffle :: [a] -> IO [a]
shuffle x = if length x < 2 then return x else do
    i <- System.Random.randomRIO (0, length(x)-1)
    r <- shuffle (take i x ++ drop (i+1) x)
    return (x!!i : r)

{-| A function that returns a hand object given a deck of cards
-}
get_hand :: [a] -> (a, a)
get_hand (x:xs) = (x, head xs)

{-| A function that removes some number of cards from the top of a deck of cards
-}
remove_cards :: Int -> Deck -> Deck
remove_cards a deck = drop a deck

{-| A function that returns the value of a single card
-}
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