--  File    : Proj1.hs
--  Author  : Oliver Tonks
--  Purpose : COMP30020 - Declarative Programming - Project 1

-- | This module implements three main functions for the card game
--   as well as the GameState type. 
--   
--   A GameState is defined to be the list of remaining possible answers
--   this is generated from the combinations of cards that remain. The aim
--   of the game is to guess the answer as quickly as possible, both in terms of
--   number of guesses and speed of calculation. Each guess receives feedback
--   which is then used to determine through determining which answer will leave
--   the smallest GameState, or through pruning the GameState of impossible 
--   answers.

module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Card
import Data.List
import Data.Function

type GameState = [[Card]]
state_limit :: Int
state_limit = 1000

-- | Feedback generates a tuple of results from calculating if a guess matches
--   a set of five different criteria for a given answer
--   
--   The function can be abstracted into two different sections of criteria:
--   1) A count of matching elements, i.e equal card, rank or suit
--   2) A count of elements that are less than or greater than the max or min
--   of an answer. 
--   This feedback was then decomposed into these functions which are explained
--   below

feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
feedback [] _ = (0,0,0,0,0)
feedback _ [] = (0,0,0,0,0)
feedback as gs =  (matchCount as gs,
                   orderCount ansRanks guessRanks LT, 
                   matchCount ansRanks guessRanks, 
                   orderCount ansRanks guessRanks GT,
                   matchCount ansSuits guessSuits)
                   where ansRanks = getProps as rank
                         guessRanks = getProps gs rank
                         ansSuits = getProps as suit
                         guessSuits = getProps gs suit

-- | This function takes two lists and calculates the number of matching 
--   elements without duplication. This is done by deleting a matched element
--   from the secondary list
matchCount :: Eq a => [a] -> [a] -> Int
matchCount [] _ = 0
matchCount _ [] = 0
matchCount (x:xs) ys = if elem x ys
                        then 1 + matchCount xs (delete x ys)
                        else matchCount xs ys

-- | This function takes two lists that can be ordered and determines the number
--   of elements less than the minimum of the second list or the number of
--   elements greater than the maxmimum of the second list. This is determined
--   by checking the supplied Ordering element
orderCount :: Ord a => [a] -> [a] -> Ordering -> Int
orderCount [] _ _ = 0
orderCount _ [] _ = 0
orderCount (x:xs) ys order
    | compare x minmax == order = 1 + orderCount xs ys order
    | otherwise = orderCount xs ys order
    where minmax = if order == LT then minimum ys
            else maximum ys

-- | Takes a list of cards and a function that returns one of 
--   its properties, finally returns a list of all the wanted properties 
--   from supplied cards
getProps :: [Card] -> (Card->a) -> [a]
getProps [] _ = []
getProps (card:cards) property = property card: getProps cards property

-- | Determines the initialGuess for a game, given a supplied integer this works
--   for any number of cards in the initial guess. It calculates separate lists
--   for rank and suit and generates cards from that.
--   
--   The state is then chosen as the combinations of size num of all possible
--   cards. The guess is then removed from that state
initialGuess :: Int -> ([Card], GameState)
initialGuess num = (guess,state)
    where 
        cards = [(minBound::Card)..(maxBound::Card)]
        guess = [(Card x y) | (x, y) <- zip (suitsList num) (ranksList num)]
        state = combinationsOfSize num cards \\ [guess]

-- | Generates a list of ranks that from the Rank enum type that follows
--   the formula of being 13/n+1 cards apart. It uses modulo to work up to any
--   number of cards
ranksList :: Int -> [Rank]
ranksList n = [toEnum $ ((ceiling (13/((fromIntegral n::Double)+1)))*x )`mod` 13 
                ::Rank | x <- [1..n]]

-- | Generates a list of suits by taking the first n elements of the iteration
--   over 'next' Suit. 
suitsList :: Int -> [Suit]
suitsList n = take n (iterate next Club)

-- | Next takes the subsequent enum from a given enum and cycles back to the 
--   minimum bound when it arrives at the max to provide constant cycling
next :: (Enum a, Bounded a, Eq a) => a -> a
next x 
    | x == maxBound = minBound
    | otherwise     = succ x

-- | Takes a number (n) and a list and returns all combinations of size n of
--   that list.
--
--   This is achieved by taking the first element of a list (x) and generating
--   all combinations of length (n-1) then appending x to the start of that list
--   using map. This is achieved recursively, the rest of the combinations are 
--   then added to the end of the list using n and the tail of the list
combinationsOfSize :: Int -> [a] -> [[a]]
combinationsOfSize 0 _ = [[]]
combinationsOfSize _ [] = []
combinationsOfSize n (x:xs) = (map (x:) (combinationsOfSize (n-1) xs)) ++ 
                                (combinationsOfSize n xs)

-- | Takes a selection (set of cards) and a feedback from a previous guess and
--   returns the next guess to take with the next state
--
--   The nextstate is first generated by removing all inconsistent possible 
--   answers from the next state by taking the possible answer and last guess
--   and checking if the feedback received is equal. Finally the last guess is
--   removed from the next state to avoid repetition
--
--   The next guess is then generated. If the remaining gamestate is large then 
--   it simply picks the last possible answer from the set of pruned possible 
--   answers. If the size is small enough then the bestguess is generated, this 
--   process takes a lot of computation and hence is only done if the remaining
--   state is small enough
nextGuess :: ([Card],GameState) -> (Int, Int, Int, Int, Int) -> ([Card],GameState)
nextGuess (lastGuess, state) lastFeedback = (nextguess, nextstate)
    where
        nextstate = delete lastGuess [card | card <- state, 
                    feedback card lastGuess == lastFeedback]
        nextguess = if length nextstate > state_limit 
                    then last nextstate 
                    else bestGuess nextstate

-- | Take a list of possible answers (a state), evaluate each possible answer 
--   and determine which will generate the smallest remaining list of possible
--   answers
--   
--   An evaluated guess is a tuple of ([Card], Double) where the Double is the
--   result of evaluation. 
--
--   The best guess is therefore the head of the list of evaluated guesses, and
--   the final return value is therefore the first value (the card) in the 
--   bestguess tuple
bestGuess :: GameState -> [Card]
bestGuess state = fst bestguess
    where
        evaluatedGuess = [(guess, evaluation)| guess <- state, 
                            let evaluation = evaluate guess (state \\ [guess])]
        bestguess = head (sortBy (compare `on` snd) evaluatedGuess)

-- | Evaluate takes a possible answer (possibility) and evaluates that with the
--   given state.
--
--   This is done through taking the possibility and every other possible answer
--   in the state and calculating the feedback of those two selections together
--   (feedbacks). These feedbacks are then grouped into matching feedbacks
--   (groupedFeedbacks). The number of these different feedbacks is then
--   calculated by taking the length of that list. Using fromIntegral to keep
--   it as a Num rather than an Int
--
--   Then each group is taken from the grouped feedbacks and the size of it 
--   it calculated (again using fromIntegral to be a Num). The evaluation is
--   then performed by taking the size of each feedback group squared and
--   weighted by the total number of feedbacks. The final answer is the sum
--   of each of these for that possibility
evaluate :: [Card] -> GameState -> Double
evaluate possbility state
    = sum [grpSize**2 / totalFeedbacks | grp <- groupedFeedbacks, 
            let grpSize = fromIntegral (length grp)]
    where
        feedbacks  = [result | potentialAns <- state, 
                        let result = feedback possbility potentialAns]
        groupedFeedbacks = group (sort feedbacks)
        totalFeedbacks = fromIntegral (length feedbacks)
