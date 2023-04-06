module Src.Poker_logic (
    Table
) where

import Src.Deck_logic
import Src.Hand_logic

{-| Table datatype to keep track of entire games with deck of cards, list of hands, number of players and location of the button
-}
data Table = Table [Card] [(Card, Card)] Int Int

{-| Pot datatype that keeps track of the total Pot, minimum bet and maximum bet
-}
data Pot = Pot Float Float Float

