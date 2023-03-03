import System.Random

data Suit = Clubs | Spades | Hearts | Diamonds deriving (Show, Enum);

data Value = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Show, Enum);

type Card = (Value, Suit)

type Deck = [Card]

create_deck :: Deck
create_deck = [(a, b) | a <- [Ace ..], b <- [Clubs ..]]

shuffle :: [a] -> IO [a]
shuffle x = if length x < 2 then return x else do
    i <- System.Random.randomRIO (0, length(x)-1)
    r <- shuffle (take i x ++ drop (i+1) x)
    return (x!!i : r)