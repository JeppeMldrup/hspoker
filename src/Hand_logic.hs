import Deck_logic

all_combinations hand deck = [a | a <-(subsets ((fst hand) : (snd hand) : deck)), length a == 5]

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

best_combo combos = foldr (\x -> max x (snd y)) 0 (map combo_value combos)

combo_value combo = straight_flush combo

straight_flush combo = four_of_a_kind combo

four_of_a_kind combo = full_house combo

full_house combo = flush combo

flush combo = straight combo

straight combo = three_of_a_kind combo

three_of_a_kind combo = two_pairs combo

two_pairs combo = pair combo

pair combo = high_card combo

high_card combo = 0