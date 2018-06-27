module Pakmen (
  pakmen,
  State
) where

import UI.HSCurses.Curses hiding (board)
import UI.HSCurses.CursesHelper
import UI.HSCurses.Logging
import qualified UI.HSCurses.Widgets
import System.Exit
import Control.Monad.Loops
import Control.Monad

data Cell = Wall | Food | Cherry | Pacman | Ghost
type Row = [Cell]
type Board = [Row]
type Vector = (Int, Int)

instance Show Cell where
  show Wall = "#"
  show Food = "."
  show Cherry = "C"
  show Pacman = "U"
  show Ghost = "G"

boardHeight = 13
boardWidth = 9

boardCells = [
    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall],
    [Wall, Food, Food, Food, Food, Food, Food, Food, Wall],
    [Wall, Food, Food, Food, Food, Food, Food, Food, Wall],
    [Wall, Food, Food, Food, Food, Food, Food, Food, Wall],
    [Wall, Food, Wall, Wall, Food, Wall, Wall, Food, Wall],
    [Wall, Food, Food, Wall, Pacman, Wall, Food, Food, Wall],
    [Wall, Food, Wall, Wall, Food, Wall, Wall, Food, Wall],
    [Wall, Food, Food, Food, Food, Food, Food, Food, Wall],
    [Wall, Food, Food, Food, Food, Food, Food, Food, Wall],
    [Wall, Food, Food, Food, Food, Food, Food, Food, Wall],
    [Wall, Food, Food, Food, Food, Food, Food, Food, Wall],
    [Wall, Food, Food, Food, Food, Food, Food, Food, Wall],
    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall]]

data State = State {
  board :: Board,
  input :: Maybe Char,
  height :: Int,
  width :: Int
}

pakmen :: IO State
pakmen = do
    win <- initScr
    echo False
    keypad win True
    initialState >>= iterateUntilM gameOver (loopStep win)

initialState :: IO State
initialState = return State {
  board = boardCells,
  input = Nothing,
  height = boardHeight,
  width = boardWidth
}

cell :: Int -> Int -> String
cell x y = show $ boardCells !! x !! y

gameOver :: State -> Bool
gameOver (State {
    board = boardCurrent
  })
  | otherwise = False

loopStep :: Window -> State -> IO State
loopStep win state = do
  displayState win state
  getCh >>= \ input ->
    return $ updateState state (inputToTuple input)

inputToTuple :: Key -> Vector
inputToTuple (KeyChar 'w') = (-1, 0)
inputToTuple (KeyChar 's') = (1, 0)
inputToTuple (KeyChar 'a') = (0, -1)
inputToTuple (KeyChar 'd') = (0, 1)

updateState :: State -> Vector -> State
updateState state key = state

displayState :: Window -> State -> IO State
displayState win state = do
  wclear win
  wAddStr win (renderBoard state)
  refresh
  return state

renderBoard :: State -> String
renderBoard state =
  unlines $ map (foldl (\acc x -> acc ++ show x) []) (board state)

