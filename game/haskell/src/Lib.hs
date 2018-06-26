module Lib (
  pakmen,
  State
) where

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import UI.HSCurses.Logging
import UI.HSCurses.Widgets
import System.Exit
import Control.Monad

type Cell = String
type Row = [Cell]
type Board = [Lib.Row]

wallCell = "#"
eatableCell = "."
userCell = "U"
boardHeight = 13
boardWidth = 9
boardCells = [
    [wallCell, wallCell, wallCell, wallCell, wallCell, wallCell, wallCell, wallCell, wallCell],
    [wallCell, eatableCell, eatableCell, eatableCell, eatableCell, eatableCell, eatableCell, eatableCell, wallCell],
    [wallCell, eatableCell, eatableCell, eatableCell, eatableCell, eatableCell, eatableCell, eatableCell, wallCell],
    [wallCell, eatableCell, eatableCell, eatableCell, eatableCell, eatableCell, eatableCell, eatableCell, wallCell],
    [wallCell, eatableCell, wallCell, wallCell, eatableCell, wallCell, wallCell, eatableCell, wallCell],
    [wallCell, eatableCell, eatableCell, wallCell, userCell, wallCell, eatableCell, eatableCell, wallCell],
    [wallCell, eatableCell, wallCell, wallCell, eatableCell, wallCell, wallCell, eatableCell, wallCell],
    [wallCell, eatableCell, eatableCell, eatableCell, eatableCell, eatableCell, eatableCell, eatableCell, wallCell],
    [wallCell, eatableCell, eatableCell, eatableCell, eatableCell, eatableCell, eatableCell, eatableCell, wallCell],
    [wallCell, eatableCell, eatableCell, eatableCell, eatableCell, eatableCell, eatableCell, eatableCell, wallCell],
    [wallCell, eatableCell, eatableCell, eatableCell, eatableCell, eatableCell, eatableCell, eatableCell, wallCell],
    [wallCell, eatableCell, eatableCell, eatableCell, eatableCell, eatableCell, eatableCell, eatableCell, wallCell],
    [wallCell, wallCell, wallCell, wallCell, wallCell, wallCell, wallCell, wallCell, wallCell]]

data State = State {
  board :: Board,
  input :: Maybe Char
}

pakmen :: IO State
pakmen = do
    putStrLn $ cell 0 0
    mainwin <- initScr
    echo False
    keypad mainwin True

    initScreen mainwin
    let y = boardHeight + 1
    mvWAddStr mainwin y 0 "Press key 'q' to quit..."
    refresh
    forever $ do
        c <- getCh
        if c == KeyChar 'q'
          then delWin mainwin >> endWin >> exitWith ExitSuccess
        --   else if c `elem` ['w', 's', 'a', 'd']
        --     then move c
            else do
                refresh

initialState :: IO State
initialState = return State {
  Lib.board = boardCells,
  input = Nothing
}

cell :: Int -> Int -> Cell
cell x y = boardCells !! x !! y

gameOver :: State -> Bool
gameOver (State {
    Lib.board = boardCurrent
  })
  | otherwise = False

printBoard :: Window -> Int -> Int -> IO()
printBoard window x y =
    when(x < boardHeight && y < boardWidth) $ do
    mvWAddStr window x y $ cell x y
    let x' = x + 1
    let y' = y + 1
    printBoard window x' y
    printBoard window x y'

initScreen mainwin = do
    printBoard mainwin 0 0
