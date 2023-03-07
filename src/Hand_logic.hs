module Hand_logic(
    all_combinations,
    best_combo,
    subsets,
    combo_value,
    straight_flush,
    four_of_a_kind,
    full_house,
    flush,
    straight,
    three_of_a_kind,
    two_pairs,
    pair,
    high_card,
    kicker
) where

import Deck_logic
import Data.List
import Data.Ord

all_combinations hand deck = [a | a <-(subsets ((fst hand) : (snd hand) : deck)), length a == 5]

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

best_combo :: [[Card]] -> Hand_value
best_combo hands = head (sortDesc (map combo_value hands))

combo_value combo = straight_flush combo

straight_flush combo = four_of_a_kind combo

four_of_a_kind combo = full_house combo

full_house combo = flush combo

flush combo = straight combo

straight combo = three_of_a_kind combo

three_of_a_kind combo = two_pairs combo

two_pairs combo = pair combo

pair combo = high_card combo

high_card combo = Hand_value combo (kicker (sortDesc (map card_value combo)))

kicker :: [Double] -> Double
kicker x = if length x == 1 then head x else
    (head x) + (0.1 * (kicker (tail x)))

sortDesc a = sortOn Down a