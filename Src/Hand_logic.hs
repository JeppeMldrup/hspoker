{- |
Module: Hand_logic
Description: A module containing function that handle scoring poker hands
-}
module Src.Hand_logic(
    all_combinations,
    best_combo,
    subsets,
    combo_value,
    straight_flush,
    four_of_a_kind,
    full_house,
    flush,
    checkFlush,
    straight,
    checkStraight,
    three_of_a_kind,
    find_triplet,
    two_pairs,
    removeList,
    pair,
    find_pair,
    high_card,
    kicker
) where

import Src.Deck_logic
import Data.List
import Data.Ord

{- | A function that takes a hand with two cards, and whatever cards are in the middle, and returns all possible combinations
-}
all_combinations hand deck = [a | a <-(subsets ((fst hand) : (snd hand) : deck)), length a == 5]

{- | A function that takes a list and returns all subsets
-}
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

{- | A function that takes a list of card combinations, and returns the best combination from the list with a valuation so it can be compared
-}
best_combo :: [[Card]] -> Hand_value
best_combo hands = head (sortDesc (map combo_value hands))

{- | A function that takes a single card combination and returns the value
-}
combo_value combo = straight_flush (sortDesc combo)

{-| Check for straight flush
-}
straight_flush combo =
    if ((checkFlush combo) && (checkStraight combo)) then
        Hand_value combo (80 + (kicker combo))
    else
        four_of_a_kind combo

{-| Chech for four of a kind
-}
four_of_a_kind combo = let x = (find_triplet combo) in
    if (length x > 0) && ((head x) == (head combo)) then
        Hand_value combo (70 + (kicker combo))
    else
        full_house combo

{-| Check for full house
-}
full_house combo = let x = (find_triplet combo)
                       y = (find_pair (removeList x combo)) in
    if (length y == 0) || (length x == 0) then
        flush combo
    else
        Hand_value combo (60 + (kicker combo))

{-| Score hand for flush or lower
-}
flush combo =
    if (checkFlush combo) then
        Hand_value combo (50 + (kicker combo))
    else
        straight combo

{-| Check for flush and return true/false value
-}
checkFlush [] = False
checkFlush [a] = True
checkFlush (x:xs) = let f = (\(Card _ a) (Card _ b) -> a == b) in
    if (f x (head xs)) then
        checkFlush xs
    else
        False

{-| Score hand for straight or lower
-}
straight combo =
    if (checkStraight combo) then
        Hand_value combo (40 + (kicker combo))
    else
        three_of_a_kind combo

{-| Check for straight and return true/false
-}
checkStraight [] = False
checkStraight [a] = True
checkStraight (x:xs) = if ((pred x) == (head xs)) then
    checkStraight xs
    else
        False

{-| Check for three of a kind
-}
three_of_a_kind combo = let x = (find_triplet combo) in
    if (length x) == 0 then
        two_pairs combo
    else
        Hand_value combo (30 + (kicker combo))

{-| Find and return a potential triplet in a list of cards
-}
find_triplet [] = []
find_triplet [a] = []
find_triplet [a, b] = []
find_triplet (a:b:cs) = if (a == b && b == (head cs)) then
    [a, b, (head cs)]
    else
        find_triplet (b:cs)

{-| Check for two pairs
-}
two_pairs combo = let x = (find_pair combo)
                      y = (find_pair (removeList x combo)) in
    if (length y == 0) then
        pair combo
    else
        Hand_value combo (20 + (kicker combo))

{-| Remove one list from another
-}
removeList a b = (filter (\x -> notElem x a) b)

{-| Check for a single pair
-}
pair combo = let x = (find_pair combo) in
    if length x == 0 then
        high_card combo
    else
        Hand_value combo (10 + (kicker combo))

{-| A function that finds and returns a potential pair in a list of cards
-}
find_pair [] = []
find_pair [a] = []
find_pair (x:xs) = if x == (head xs) then [x, (head xs)] else (find_pair xs)

{-| Generates a valuation on a hand, assuming the hand has no other better combinations than the highest card
-}
high_card combo = Hand_value combo (kicker combo)

{-| A function that takes a card combination and calculates a value representing the kicker power of the combo
-}
kicker [] = 0
kicker combo = kickerValue (sortDesc (map card_value combo))

kickerValue :: [Double] -> Double
kickerValue x = if length x == 1 then head x else
    (head x) + (0.1 * (kickerValue (tail x)))

sortDesc a = sortOn Down a