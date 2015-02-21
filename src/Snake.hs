{-# LANGUAGE RecordWildCards #-}
module Snake
  ( initialize
  , step
  , screenWidth
  , screenHeight
  , spriteWidth
  , spriteHeight
  , Coord
  , Item
  , Direction(..)
  , GameState(..)
  , Phase(..)
  , ItemType(..)
  )
where

import Prelude hiding (Left, Right, head, init, tail)
import Safe (headMay, initMay, tailMay)
import Data.Maybe (fromMaybe)
import Data.List (partition)
import Control.Arrow ((>>>), first, second)
import Control.Monad (guard)
import Data.Bool (bool)

type Coord = (Int, Int)
type Item  = (ItemType, Coord)

data Direction = Left | Right | Up | Down deriving Show
data Phase     = Running | Lost | Won deriving Show
data ItemType  = Red | Green | Yellow deriving Show
data Effect    = None deriving Show

data GameState = GameState { snake     :: [Coord]
                           , items     :: [Item]
                           , blocks    :: [Coord]
                           , effect    :: Effect
                           , direction :: Direction
                           , phase     :: Phase
                           } deriving Show

screenWidth, screenHeight, spriteWidth, spriteHeight :: Int
screenWidth  = 600
screenHeight = 600
spriteWidth  = 30
spriteHeight = 30

initialize :: GameState
initialize
  = GameState [ (270, 240)
              , (270, 270)
              , (270, 300)
              , (270, 330)
              ]
              [ (Red,    (90, 90))
              , (Green,  (180,180))
              , (Yellow, (510,510))
              ] [(390,240)] None Up Running

up, down, left, right :: Coord -> Coord
up    = second (\y -> y - spriteHeight)
down  = second (+ spriteHeight)
left  = first (\x -> x - spriteWidth)
right = first (+ spriteWidth)

translate :: Direction -> Coord -> Coord
translate Up    = up
translate Down  = down
translate Left  = left
translate Right = right

extendSnake :: GameState -> GameState
extendSnake state@(GameState {..}) =
  moveSnake (translate direction)
  where
    moveSnake :: (Coord -> Coord) -> GameState
    moveSnake move =
      fromMaybe state $ do
        head <- headMay snake
        return $ state { snake = move head : snake }

reduceSnake :: GameState -> GameState
reduceSnake state@(GameState {..}) =
  fromMaybe state $ do
    head <- headMay snake
    case partition ((== head) . snd) items of
         ([], _)   -> do
           init <- initMay snake
           return $ state { snake = init }
         (_, rest) -> return $ state { items = rest }

preventTurnAround :: GameState -> GameState
preventTurnAround state@(GameState {..}) =
  fromMaybe state $ do
    head <- headMay snake
    tail <- tailMay snake
    body <- headMay tail
    guard $ body == translate direction head
    return $ state { direction = turnAround direction }

turnAround :: Direction -> Direction
turnAround Up    = Down
turnAround Down  = Up
turnAround Left  = Right
turnAround Right = Left

detectCollision :: GameState -> GameState
detectCollision state@(GameState {..}) =
  fromMaybe state $ do
    head <- headMay snake
    tail <- tailMay snake
    guard $ offMap head || head `elem` tail || head `elem` blocks
    return $ state { phase = Lost }
  where
    offMap :: Coord -> Bool
    offMap (x, y) =
      x >= screenWidth || y >= screenHeight || x < 0 || y < 0

checkWin :: GameState -> GameState
checkWin state@(GameState {..}) =
  state { phase = phase `bool` Won $ null items }

step :: GameState -> GameState
step = preventTurnAround
   >>> extendSnake
   >>> reduceSnake
   >>> detectCollision
   >>> checkWin
