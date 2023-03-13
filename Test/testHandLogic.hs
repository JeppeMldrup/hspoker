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

    -- Test find_triplet function
    Test "Test find_triplet on empty list" (\x -> do
        let cards = [] :: [Card]
            expected = [] :: [Card]
        assertEqual x (find_triplet cards) expected),
    Test "Test find_triplet on list with one card" (\x -> do
        let cards = [Card Ace Spades] :: [Card]
            expected = [] :: [Card]
        assertEqual x (find_triplet cards) expected),
    Test "Test find_triplet on list without triplet" (\x -> do
        let cards = [Card Ace Spades, Card Ace Diamonds, Card King Clubs] :: [Card]
            expected = [] :: [Card]
        assertEqual x (find_triplet cards) expected),
    Test "Test find_triplet on list with triplet in three cards" (\x -> do
        let cards = [Card Ten Spades, Card Ten Clubs, Card Ten Diamonds] :: [Card]
            expected = [Card Ten Spades, Card Ten Clubs, Card Ten Diamonds]
        assertEqual x (find_triplet cards) expected),
    Test "Test find_triplet on list with triplet" (\x -> do
        let cards = [Card King Diamonds, Card Ten Spades, Card Ten Clubs, Card Ten Diamonds] :: [Card]
            expected = [Card Ten Spades, Card Ten Clubs, Card Ten Diamonds]
        assertEqual x (find_triplet cards) expected),
    Test "Test find_triplet on list with triplet not ordered" (\x -> do
        let cards = [Card King Diamonds, Card Ten Spades, Card Ten Clubs, Card Nine Hearts, Card Ten Diamonds] :: [Card]
            expected = []
        assertEqual x (find_triplet cards) expected),

    -- Test removeList function
    Test "Test removeList on list with pair and list with full house" (\x -> do
        let a = [3, 3, 3, 1, 1]
            b = [1, 1]
            expected = [3, 3, 3]
        assertEqual x (removeList b a) expected),
    Test "Test removeList on list with triplet and list with full house" (\x -> do
        let a = [3, 3, 3, 1, 1]
            b = [3, 3, 3]
            expected = [1, 1]
        assertEqual x (removeList b a) expected),
    Test "Test removeList on list with pair and list with full house other way" (\x -> do
        let a = [1, 1, 3, 3, 3]
            b = [1, 1]
            expected = [3, 3, 3]
        assertEqual x (removeList b a) expected),
    Test "Test removeList on two lists with no mutual elements" (\x -> do
        let a = [1, 2, 3, 4, 5]
            b = [6, 7]
            expected = [1, 2, 3, 4, 5]
        assertEqual x (removeList b a) expected),

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
        assertEqual x (three_of_a_kind cards) expected),
    
    -- Test checkStraight function
    Test "Test checkStraight on empty list" (\x -> do
        let cards = [] :: [Card]
            expected = False
        assertEqual x (checkStraight cards) expected),
    Test "Test checkStraight on list with cards not in order" (\x -> do
        let cards = [Card Ace Spades, Card Three Hearts, Card Two Clubs] :: [Card]
            expected = False
        assertEqual x (checkStraight cards) expected),
    Test "Test checkStraight on list with cards in order" (\x -> do
        let cards = [Card Ace Spades, Card King Hearts, Card Queen Clubs] :: [Card]
            expected = True
        assertEqual x (checkStraight cards) expected),
    Test "Test checkStraight on list with cards in order with extra card not in order" (\x -> do
        let cards = [Card Ace Spades, Card King Hearts, Card Queen Clubs, Card Ten Diamonds] :: [Card]
            expected = False
        assertEqual x (checkStraight cards) expected),
    
    -- Test straight function
    Test "Test straight on empty list" (\x -> do
        let cards = [] :: [Card]
            expected = Hand_value cards (kicker cards)
        assertEqual x (straight cards) expected),
    Test "Test straight on list not in order" (\x -> do
        let cards = [Card Ace Spades, Card Two Diamonds, Card Four Hearts] :: [Card]
            expected = Hand_value cards (kicker cards)
        assertEqual x (straight cards) expected),
    Test "Test straight on list in order" (\x -> do
        let cards = [Card Eight Spades, Card Seven Diamonds, Card Six Hearts] :: [Card]
            expected = Hand_value cards (40 + kicker cards)
        assertEqual x (straight cards) expected),
    Test "Test straight on list ending with two min cards" (\x -> do
        let cards = [Card Eight Spades, Card Two Diamonds, Card Two Hearts] :: [Card]
            expected = Hand_value cards (10 + kicker cards) -- There is a pair in the list, so it will be 10+kicker
        assertEqual x (straight cards) expected),
    
    -- Test checkFlush function
    Test "Test checkFlush on list with cards no flush" (\x -> do
        let cards = [Card Ace Spades, Card King Hearts, Card Queen Clubs, Card Ten Diamonds] :: [Card]
            expected = False
        assertEqual x (checkFlush cards) expected),
    Test "Test checkFlush on list with cards with flush" (\x -> do
        let cards = [Card Ace Hearts, Card King Hearts, Card Queen Hearts, Card Ten Hearts] :: [Card]
            expected = True
        assertEqual x (checkFlush cards) expected),

    -- Test flush function
    Test "Test flush on empty list" (\x -> do
        let cards = [] :: [Card]
            expected = Hand_value cards (kicker cards)
        assertEqual x (flush cards) expected),
    Test "Test flush on list no flush" (\x -> do
        let cards = [Card Ace Spades, Card Two Diamonds, Card Four Hearts] :: [Card]
            expected = Hand_value cards (kicker cards)
        assertEqual x (flush cards) expected),
    Test "Test flush on list with flush" (\x -> do
        let cards = [Card Eight Diamonds, Card Seven Diamonds, Card Six Diamonds, Card Four Diamonds, Card Two Diamonds] :: [Card]
            expected = Hand_value cards (50 + kicker cards)
        assertEqual x (flush cards) expected),
    
    -- Test full_house function
    Test "Test full_house on empty list" (\x -> do
        let cards = [] :: [Card]
            expected = Hand_value cards (kicker cards)
        assertEqual x (full_house cards) expected),
    Test "Test full_house on list no full house" (\x -> do
        let cards = [Card Ace Spades, Card Two Diamonds, Card Four Hearts] :: [Card]
            expected = Hand_value cards (kicker cards)
        assertEqual x (full_house cards) expected),
    Test "Test full_house on list with full house" (\x -> do
        let cards = [Card Eight Diamonds, Card Eight Diamonds, Card Three Diamonds, Card Three Hearts, Card Three Diamonds] :: [Card]
            expected = Hand_value cards (60 + kicker cards)
        assertEqual x (full_house cards) expected),
    Test "Test full_house on list with full house other rotation" (\x -> do
        let cards = [Card Eight Diamonds, Card Eight Diamonds, Card Eight Spades, Card Three Diamonds, Card Three Diamonds] :: [Card]
            expected = Hand_value cards (60 + kicker cards)
        assertEqual x (full_house cards) expected)
    ]

main = do runAll test_handLogic