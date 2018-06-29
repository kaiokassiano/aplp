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

boardHeight = 15
boardWidth = 13
pacmanPosition = (6, 3)
wallPositions = [
  (2, 1), (3, 1), (2, 2), (6, 1), (9, 1), (10, 1), (10, 2),
  (4, 3), (8, 3),
  (2, 6), (3, 6), (6, 5), (6, 6), (9, 6), (10, 6),
  (2, 8), (3, 8), (6, 9), (6, 8), (9, 8), (10, 8),
  (4, 11), (8, 11),
  (2, 13), (3, 13), (2, 12), (6, 13), (9, 13), (10, 13), (10, 12)]
cherriesPositions = [(9, 12), (3, 12)]
ghostsPositions = [(1, 7), (11, 7)]
powerPositions= [(6, 14)]

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
  points :: Int,
  powers :: [Vector],
  -- How many turns left of invencibility
  superTurns :: Int
}

pakmen :: IO State
pakmen = do
    win <- initScr
    echo False
    keypad win True
    initialState
      >>= iterateUntilM gameOver (loopStep win)
      >>= \ state -> endWin
        >> delWin win
        >> if length (fruits state) == 0 then
          displayVictoryScreen state
        else do
          displayLoseScreen
          return state

initialState :: IO State
initialState = return State {
  input = Nothing,
  height = boardHeight,
  width = boardWidth,
  move = Nothing,
  pacman = pacmanPosition,
  ghosts = ghostsPositions,
  walls = wallPositions,
  cherries = cherriesPositions,
  fruits = getAvailableVectors boardWidth boardHeight
    $ [pacmanPosition] ++ wallPositions ++ cherriesPositions ++ ghostsPositions ++ powerPositions,
  points = 0,
  powers = powerPositions,
  superTurns = 0
}

getAvailableVectors :: Int -> Int -> [Vector] -> [Vector]
getAvailableVectors width height occupied =
  [(x, y) | x <- [0 .. width - 1], y <- [0 .. height - 1], not $ (x, y) `elem` occupied]

gameOver :: State -> Bool
gameOver (State {
    pacman = pacmanCurrent,
    ghosts = ghostsCurrent,
    fruits = fruitsCurrent,
    superTurns = superTurnsCurrent
  })
  | superTurnsCurrent == 0 && pacmanCurrent `elem` ghostsCurrent = True
  | length fruitsCurrent == 0 = True
  | otherwise = False

loopStep :: Window -> State -> IO State
loopStep win state = do
  displayState win state
  input <- getCh
  if inputToTuple input == Nothing then
    return state
  else
    return $ updateState state (inputToTuple input)

inputToTuple :: Key -> Maybe Vector
inputToTuple (KeyChar 'w') = Just (0, -1)
inputToTuple (KeyChar 's') = Just (0, 1)
inputToTuple (KeyChar 'a') = Just (-1, 0)
inputToTuple (KeyChar 'd') = Just (1, 0)
inputToTuple _ = Nothing

updateState :: State -> Maybe Vector -> State
updateState state move = updatePower
  $ updateCherry
  $ updateFruits
  $ updatePacman
  $ updateMove state move

updatePower :: State -> State
updatePower state
  | pacmanHasFruit (pacman state) (powers state) = state {
      superTurns = 10,
      powers = [power | power <- (powers state), power /= (pacman state)]
    }
  | (superTurns state) > 0 = state { superTurns = (superTurns state) - 1 }
  | otherwise = state

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

displayVictoryScreen :: State -> IO State
displayVictoryScreen state = do
  putStrLn $ "Congratulations! You've won with a score of " ++ show (points state)
  return state

displayLoseScreen :: IO ()
displayLoseScreen = do
  putStrLn "You've lost! Run the game again if you wish to play more"

displayState :: Window -> State -> IO State
displayState win state = do
  wclear win
  wAddStr win $ renderBoard state
  addLn
  wAddStr win $ renderPoints state
  refresh
  return state

renderPoints :: State -> String
renderPoints state =
  "Score: " ++ show (points state) ++ "\n"

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
  map $ characterForPosition state

characterForPosition :: State -> Vector -> Char
characterForPosition state position
  | (pacman state) == position && (superTurns state) == 0 = 'U'
  | (pacman state) == position = 'Û'
  | position `elem` walls state = '#'
  | position `elem` fruits state = '.'
  | position `elem` ghosts state = 'G'
  | position `elem` cherries state = 'C'
  | position `elem` powers state = '*'
  | otherwise = ' '

blankBoard :: Int -> Int -> [[Vector]]
blankBoard width height =
  [[(x, y) | x <- [0 .. width - 1]] | y <- [0 .. height - 1]]

