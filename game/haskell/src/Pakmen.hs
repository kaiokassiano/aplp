module Pakmen (
  pakmen,
  State
) where

import UI.HSCurses.Curses hiding (board, move)
import UI.HSCurses.CursesHelper
import UI.HSCurses.Logging
import qualified UI.HSCurses.Widgets
import System.Exit
import Control.Monad.Loops
import Control.Monad

type Vector = (Int, Int)
type Row = [Vector]
type Board = [Row]

boardHeight = 11
boardWidth = 7
pacmanPosition = (3, 4)
wallPositions = [
  (1, 3), (2, 3), (4, 3), (5, 3),
  (2, 4), (4, 4),
  (1, 5), (2, 5), (4, 5), (5, 5)]
cherriesPositions = [(3, 10)]

data State = State {
  input :: Maybe Char,
  height :: Int,
  width :: Int,
  move :: Maybe Vector,
  pacman :: Vector,
  ghosts :: [Vector],
  walls :: [Vector],
  fruits :: [Vector],
  cherries :: [Vector],
  points :: Int
}

pakmen :: IO State
pakmen = do
    win <- initScr
    echo False
    keypad win True
    initialState >>= iterateUntilM gameOver (loopStep win)

initialState :: IO State
initialState = return State {
  input = Nothing,
  height = boardHeight,
  width = boardWidth,
  move = Nothing,
  pacman = pacmanPosition,
  ghosts = [],
  walls = wallPositions,
  cherries = cherriesPositions,
  fruits = getAvailableVectors boardWidth boardHeight $ [pacmanPosition] ++ wallPositions ++ cherriesPositions,
  points = 0
}

getAvailableVectors :: Int -> Int -> [Vector] -> [Vector]
getAvailableVectors width height occupied =
  [(x, y) | x <- [0 .. width - 1], y <- [0 .. height - 1], not $ (x, y) `elem` occupied]

gameOver :: State -> Bool
gameOver (State {
    pacman = pacmanCurrent,
    ghosts = ghostsCurrent
  })
  | pacmanCurrent `elem` ghostsCurrent = True
  | otherwise = False

loopStep :: Window -> State -> IO State
loopStep win state = do
  displayState win state
  getCh >>= \ input ->
    return $ updateState state (inputToTuple input)

inputToTuple :: Key -> Maybe Vector
inputToTuple (KeyChar 'w') = Just (0, -1)
inputToTuple (KeyChar 's') = Just (0, 1)
inputToTuple (KeyChar 'a') = Just (-1, 0)
inputToTuple (KeyChar 'd') = Just (1, 0)
inputToTuple _ = Nothing

updateState :: State -> Maybe Vector -> State
updateState state move
  = updateCherry $ updateFruits $ updatePacman $ updateMove state move

updateCherry :: State -> State
updateCherry state
  | pacmanHasFruit (pacman state) (cherries state) = state {
      cherries = [cherry | cherry <- (cherries state), cherry /= (pacman state)],
      points = (points state) + 10
    }
  | otherwise = state

updateFruits :: State -> State
updateFruits state
  | pacmanHasFruit (pacman state) (fruits state) = state {
      fruits = [fruit | fruit <- (fruits state), fruit /= (pacman state)],
      points = (points state) + 1
    }
  | otherwise = state

pacmanHasFruit :: Vector -> [Vector] -> Bool
pacmanHasFruit pacmanPosition fruits
  = pacmanPosition `elem` fruits

updateMove :: State -> Maybe Vector -> State
updateMove state inputMove@(Just move) = state { move = inputMove }
updateMove state _ = state { move = Nothing }

updatePacman :: State -> State
updatePacman state@(State { move = (Just moveVector) })
  | hitWall (pacman state) moveVector (width state) (height state) (walls state) = state
  | otherwise = state { pacman = (pacman state) `vectorAdd` moveVector }
updatePacman state = state

hitWall :: Vector -> Vector -> Int -> Int -> [Vector] -> Bool
hitWall pacmanCurrent@(pacmanX, pacmanY) moveVector@(moveX, moveY) width height walls
  | pacmanX + moveX < 0 || pacmanY + moveY < 0 = True
  | pacmanX + moveX >= width || pacmanY + moveY >= height = True
  | pacmanCurrent `vectorAdd` moveVector `elem` walls = True
  | otherwise = False

vectorAdd :: Vector -> Vector -> Vector
vectorAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

displayState :: Window -> State -> IO State
displayState win state = do
  wclear win
  wAddStr win (renderBoard state)
  addLn
  wAddStr win (renderPoints state)
  refresh
  return state

renderPoints :: State -> String
renderPoints state =
  "Points: " ++ show (points state) ++ "\n"

renderBoard :: State -> String
renderBoard state
  = unlines $ applyBorder (width state)
            $ map (renderRow state)
            $ blankBoard (width state) (height state)

applyBorder :: Int -> [String] -> [String]
applyBorder width rows =
  border ++ map (("#" ++) . (++ "#")) rows ++ border
  where border = [replicate (width + 2) '#']

renderRow :: State -> [Vector] -> String
renderRow state =
  map (characterForPosition state)

characterForPosition :: State -> Vector -> Char
characterForPosition state position
  | (pacman state) == position = 'U'
  | position `elem` walls state = '#'
  | position `elem` fruits state = '.'
  | position `elem` ghosts state = 'G'
  | position `elem` cherries state = 'C'
  | otherwise = ' '

blankBoard :: Int -> Int -> [[Vector]]
blankBoard width height =
  [[(x, y) | x <- [0 .. width - 1]] | y <- [0 .. height - 1]]

