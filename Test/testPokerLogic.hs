module Test.Poker_logic (
    test_pokerlogic
) where

import Src.Poker_logic
import Test.Framework

test_pokerlogic = [
    -- Test addToPot function
    Test "Test addToPot function with less than min" (\x -> do
        let pot = Pot 1.0 2.0 3.0
        assertEqual x (addToPot 1 pot) pot),
    Test "Test addToPot function with more than max" (\x -> do
        let pot = Pot 1 2 3
        assertEqual x (addToPot 4 pot) pot),
    Test "Test addToPot function with value between min and max" (\x -> do
        let pot = Pot 1 2 3
        assertEqual x (addToPot 2.5 pot) (Pot 3.5 2 3))
    ]