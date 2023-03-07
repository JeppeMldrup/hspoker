module Test.Hand_logic(
) where

import Src.Hand_logic
import Src.Deck_logic
import Test.Framework

tests = [
    Test "Test kicker function on max and min values" (\x -> do
        let minHand = [Card Two Hearts, Card Two Spades, Card Two Clubs, Card Two Diamonds, Card Three Hearts]
            minHandExpected = 2/13+1/130+1/1300+1/13000+1/130000
        assertEqualMargin x (kicker minHand) minHandExpected 0.0000001),
    Test "Test kicker function on max and min values" (\x -> do
        let minHand = [Card Two Hearts, Card Two Spades, Card Two Clubs, Card Two Diamonds, Card Three Hearts]
            minHandExpected = 2/13+1/130+1/1300+1/13000+1/130000
        assertEqual x (kicker minHand) minHandExpected)
    ]

