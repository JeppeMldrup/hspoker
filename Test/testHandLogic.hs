module Test.Hand_logic(
    main
) where

import Src.Hand_logic
import Src.Deck_logic
import Test.Framework

test_handLogic = [
    -- Test kicker function
    Test "Test kicker on empty list" (\x -> do
        let cards = [] :: [Card]
            expected = 0
        assertEqual x (kicker cards) expected),
    Test "Test kicker function on min values" (\x -> do
        let minHand = [Card Two Hearts, Card Two Spades, Card Two Clubs, Card Two Diamonds, Card Three Hearts]
            minHandExpected = 2/13+1/130+1/1300+1/13000+1/130000
        assertEqualMargin x (kicker minHand) minHandExpected 0.0000001),
    Test "Test kicker function on max values" (\x -> do
        let maxHand = [Card Ace Hearts, Card Ace Spades, Card Ace Clubs, Card Ace Diamonds, Card King Hearts]
            maxHandExpected = 1 + 1/10 + 1/100 + 1/1000 + 12/130000
        assertEqualMargin x (kicker maxHand) maxHandExpected 0.0000001),
    
    -- Test find_pairs function
    Test "Test find_pair on null list" (\x -> do
        let cards = [] :: [Card]
            expected = [] :: [Card]
        assertEqual x (find_pair cards) expected),
    Test "Test find_pair on list without a pair" (\x -> do
        let cards = [Card Ace Hearts, Card King Spades, Card Seven Clubs, Card Eight Hearts, Card Ten Diamonds] :: [Card]
            expected = [] :: [Card]
        assertEqual x (find_pair cards) expected),
    Test "Test find_pair on list with a pair not in order" (\x -> do
        let cards = [Card Ace Hearts, Card Eight Spades, Card Seven Clubs, Card Eight Hearts, Card Ten Diamonds] :: [Card]
            expected = [] :: [Card]
        assertEqual x (find_pair cards) expected),
    Test "Test find_pair on list with a pair in order" (\x -> do
        let cards = [Card Ace Hearts, Card Seven Spades, Card Seven Clubs, Card Eight Hearts, Card Ten Diamonds] :: [Card]
            expected = [Card Seven Spades, Card Seven Clubs] :: [Card]
        assertEqual x (find_pair cards) expected),
    Test "Test find_pair on list with two pairs in order" (\x -> do
        let cards = [Card Ace Hearts, Card Seven Spades, Card Seven Clubs, Card Ten Hearts, Card Ten Diamonds] :: [Card]
            expected = [Card Seven Spades, Card Seven Clubs] :: [Card]
        assertEqual x (find_pair cards) expected),
    
    -- Test pair function
    Test "Test pair on empty list" (\x -> do
        let cards = [] :: [Card]
            expected = (Hand_value [] 0)
        assertEqual x (pair cards) expected),
    Test "Test pair on list without pair" (\x -> do
        let cards = [Card Ace Hearts, Card Two Spades, Card Ten Clubs, Card Seven Spades] :: [Card]
            expected = (Hand_value cards (kicker cards))
        assertEqual x (pair cards) expected),
    Test "Test pair on list with pair" (\x -> do
        let cards = [Card Ace Hearts, Card Ace Spades, Card Ten Clubs, Card Seven Spades] :: [Card]
            expected = (Hand_value cards (10 + (kicker cards)))
        assertEqual x (pair cards) expected),
    
    -- Test two_pairs function
    Test "Test two_pairs on empty list" (\x -> do
        let cards = [] :: [Card]
            expected = (Hand_value [] 0)
        assertEqual x (two_pairs cards) expected),
    Test "Test two_pairs on list without pair" (\x -> do
        let cards = [Card Ace Hearts, Card Two Spades, Card Ten Clubs, Card Seven Spades] :: [Card]
            expected = (Hand_value cards (kicker cards))
        assertEqual x (two_pairs cards) expected),
    Test "Test two_pairs on list with pair" (\x -> do
        let cards = [Card Ace Hearts, Card Ace Spades, Card Ten Clubs, Card Seven Spades] :: [Card]
            expected = (Hand_value cards (10 + (kicker cards)))
        assertEqual x (two_pairs cards) expected),
    Test "Test two_pairs on list with two pairs" (\x -> do
        let cards = [Card Ace Hearts, Card Ace Spades, Card Ten Clubs, Card Ten Spades, Card Three Clubs] :: [Card]
            expected = (Hand_value cards (20 + (kicker cards)))
        assertEqual x (two_pairs cards) expected),

    -- Test three_of_a_kind function
    Test "Test three_of_a_kind on empty list" (\x -> do
        let cards = [] :: [Card]
            expected = (Hand_value [] 0)
        assertEqual x (three_of_a_kind cards) expected),
    Test "Test three_of_a_kind on list without pair" (\x -> do
        let cards = [Card Ace Hearts, Card Two Spades, Card Ten Clubs, Card Seven Spades] :: [Card]
            expected = (Hand_value cards (kicker cards))
        assertEqual x (three_of_a_kind cards) expected),
    Test "Test three_of_a_kind on list with pair" (\x -> do
        let cards = [Card Ace Hearts, Card Ace Spades, Card Ten Clubs, Card Seven Spades] :: [Card]
            expected = (Hand_value cards (10 + (kicker cards)))
        assertEqual x (three_of_a_kind cards) expected),
    Test "Test three_of_a_kind on list with two pairs" (\x -> do
        let cards = [Card Ace Hearts, Card Ace Spades, Card Ten Clubs, Card Ten Spades, Card Three Clubs] :: [Card]
            expected = (Hand_value cards (20 + (kicker cards)))
        assertEqual x (three_of_a_kind cards) expected),
    Test "Test three_of_a_kind on list with three of a kind" (\x -> do
        let cards = [Card Ace Hearts, Card Ace Spades, Card Ace Clubs, Card Ten Spades, Card Three Clubs] :: [Card]
            expected = (Hand_value cards (30 + (kicker cards)))
        assertEqual x (three_of_a_kind cards) expected)
    ]

main = do runAll test_handLogic