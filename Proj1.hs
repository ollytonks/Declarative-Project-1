--  File    : Proj1.hs
--  Author  : Oliver Tonks (835410)
--  Purpose : COMP30020 - Declarative Programming - Project 1

-- | This module implements three main functions for the card game
--   as well as the GameState type

module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Card
import Data.List
import Data.Function

type GameState = [[Card]]

-- might have to just iterate over the list that many times it looks simpler
-- what im trying to do is to say given we have one guess and one ans check what matches and create tuple of tht
-- then sum over the result
feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
feedback [] _ = (0,0,0,0,0)
feedback _ [] = (0,0,0,0,0)
feedback as gs =  (matchCount as gs,
                   matchOrder ansRanks guessRanks LT, 
                   matchCount ansRanks guessRanks, 
                   matchOrder ansRanks guessRanks GT,
                   matchCount (getProps as suit) (getProps gs suit))
                   where ansRanks = getProps as rank
                         guessRanks = getProps gs rank

matchCount :: Eq a => [a] -> [a] -> Int
matchCount [] _ = 0
matchCount _ [] = 0
matchCount (x:xs) ys = if elem x ys
                             then 1 + matchCount xs (delete x ys)
                             else matchCount xs ys

-- takes a list of cards and a function that takes a card that returns one of its properties,
-- finally returns a list of all the wanted properties from supplied cards
getProps :: [Card] -> (Card->a) -> [a]
getProps [] _ = []
getProps (card:cards) property = property card: getProps cards property

-- given two lists of objects with ordered elements and a wanted ordering count how many
-- elements fulfil that ordering given the min or max of the second list
matchOrder :: Ord a => [a] -> [a] -> Ordering -> Int
matchOrder [] _ _ = 0
matchOrder _ [] _ = 0
matchOrder (x:xs) ys order
    | compare x minmax == order = 1 + matchOrder xs ys order
    | otherwise = matchOrder xs ys order
    -- boost efficiency by generalising to find min or max before hand so only does it onc
    where minmax = if order == LT then minimum ys
            else maximum ys

initialGuess :: Int -> ([Card],GameState)
initialGuess num = (guess, state)
    where 
        cards = [(minBound::Card)..(maxBound::Card)]
        guess = if num == 2 then [(Card Club Ace), (Card Heart R7)]
                else if num == 3 then [(Card Club Ace), (Card Diamond R7), (Card Spade R2)]
                else [(Card Club R3), (Card Diamond R7), (Card Heart Jack), (Card Spade Ace)]
        state = combinationsOfSize num cards \\ [guess]

combinationsOfSize :: Int -> [a] -> [[a]]
combinationsOfSize 0 _ = [[]]
combinationsOfSize _ [] = []
combinationsOfSize n (x:xs) = (map (x:) (combinationsOfSize (n-1) xs)) ++ (combinationsOfSize n xs)

nextGuess :: ([Card],GameState) -> (Int, Int, Int, Int, Int) -> ([Card],GameState)
nextGuess (lastGuess, state) lastFeedback = (nextguess, nextState)
    where
        nextState = delete lastGuess [card | card <- state, feedback card lastGuess == lastFeedback]
        nextguess = if length nextState > 1000 then last nextState else bestGuess nextState

--IMPLEMENT QUICKGUESS

-- take a list of remaining possible answers (GameState) and return the best possible guess
bestGuess :: GameState -> [Card]
bestGuess state = fst (head bestguess)
    where
        evaluatedGuess = [(guess, evaluation)| guess <- state, 
                        let evaluation = evaluate guess (state \\ [guess])]
        bestguess = sortBy (compare `on` snd) evaluatedGuess

evaluate :: [Card] -> GameState -> Double
evaluate guess state
    = sum [grpSize**2 / totalFeedbacks | grp <- groupedFeedbacks, 
            let grpSize = fromIntegral (length grp)]
    where
        feedbacks  = [result | potentialAns <- state, let result = feedback guess potentialAns]
        groupedFeedbacks = group (sort feedbacks)
        totalFeedbacks = fromIntegral (length feedbacks)
