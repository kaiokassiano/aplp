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
cherriesPositions = []

data State = State {
  input :: Maybe Char,
  height :: Int,
  width :: Int,
  move :: Maybe Vector,
  pacman :: Vector,
  ghosts :: [Vector],
  walls :: [Vector],
  fruits :: [Vector],
  cherries :: [Vector]
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
  fruits = getAvailableVectors boardWidth boardHeight $ [pacmanPosition] ++ wallPositions ++ cherriesPositions
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
inputToTuple (KeyChar 'w') = Just (-1, 0)
inputToTuple (KeyChar 's') = Just (1, 0)
inputToTuple (KeyChar 'a') = Just (0, -1)
inputToTuple (KeyChar 'd') = Just (0, 1)
inputToTuple _ = Nothing

updateState :: State -> Maybe Vector -> State
updateState state move
  = updatePacman $ updateMove state move

updateMove :: State -> Maybe Vector -> State
updateMove state inputMove@(Just move) = state { move = inputMove }
updateMove state _ = state

updatePacman :: State -> State
updatePacman state@(State { move = (Just vector) }) = state

displayState :: Window -> State -> IO State
displayState win state = do
  wclear win
  wAddStr win (renderBoard state)
  refresh
  return state

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

