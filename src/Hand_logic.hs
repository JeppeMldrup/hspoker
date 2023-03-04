import Deck_logic

best_hand hand deck = [a | a <-(subsets ((fst hand) : (snd hand) : deck)), length a == 5]

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)