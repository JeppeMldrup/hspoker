module Test.Main (
    main
) where

import Test.Hand_logic
import Test.Poker_logic
import Test.Framework

tests = test_handLogic ++ test_pokerlogic

main = do runAll tests