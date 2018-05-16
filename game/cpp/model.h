#ifndef PAKMEN_MODEL_H
#define PAKMEN_MODEL_H

namespace Pakmen {

static const int WALL_CELL = -1;
static const int EMPTY_CELL = 0;
static const int USER_CELL = 1;
static const int GHOST_CELL = 2;
static const int CHERRY_CELL = 3;

static const int BOARD_WIDTH = 7;
static const int BOARD_HEIGHT = 8;

/**
 * Represents the game board and current overall state of the game.
 */
class GameBoard {
public:
  bool is_finished = false;

  int board[BOARD_HEIGHT][BOARD_WIDTH] = {
    {WALL_CELL, WALL_CELL , WALL_CELL , WALL_CELL  , WALL_CELL , WALL_CELL , WALL_CELL},
    {WALL_CELL, EMPTY_CELL, GHOST_CELL, EMPTY_CELL , GHOST_CELL, EMPTY_CELL, WALL_CELL},
    {WALL_CELL, EMPTY_CELL, EMPTY_CELL, EMPTY_CELL , EMPTY_CELL, EMPTY_CELL, WALL_CELL},
    {WALL_CELL, EMPTY_CELL, WALL_CELL , EMPTY_CELL , WALL_CELL , EMPTY_CELL, WALL_CELL},
    {WALL_CELL, EMPTY_CELL, WALL_CELL , USER_CELL  , WALL_CELL , EMPTY_CELL, WALL_CELL},
    {WALL_CELL, EMPTY_CELL, EMPTY_CELL, EMPTY_CELL , EMPTY_CELL, EMPTY_CELL, WALL_CELL},
    {WALL_CELL, EMPTY_CELL, EMPTY_CELL, CHERRY_CELL, EMPTY_CELL, EMPTY_CELL, WALL_CELL},
    {WALL_CELL, WALL_CELL , WALL_CELL , WALL_CELL  , WALL_CELL , WALL_CELL , WALL_CELL},
  };
};

} // namespace

#endif // PAKMEN_MODEL_H

