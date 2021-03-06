#include "query.h"

bool Pakmen::is_game_over(GameBoard *board) {
  for (int i = 0; i < Pakmen::BOARD_HEIGHT; i++) {
    for (int j = 0; j < Pakmen::BOARD_WIDTH; j++) {
      if (board->board[i][j] == Pakmen::USER_CELL)
        return false;
    }
  }
  return true;
}

std::tuple<int, int> Pakmen::find_object(Pakmen::GameBoard* board, int object) {
  for (int i = 0; i < Pakmen::BOARD_HEIGHT; i++) {
    for (int j = 0; j < Pakmen::BOARD_WIDTH; j++) {
      if (board->board[i][j] == object)
        return std::make_tuple(i, j);
    }
  }

  return std::make_tuple(-1, -1);
}

std::vector<std::tuple<int, int>> Pakmen::find_all_objects(Pakmen::GameBoard* board, int object) {
  std::vector<std::tuple<int, int>> list;

  for (int i = 0; i < Pakmen::BOARD_HEIGHT; i++) {
    for (int j = 0; j < Pakmen::BOARD_WIDTH; j++) {
      if (board->board[i][j] == object)
        list.push_back(std::make_tuple(i, j));
    }
  }

  return list;
}

bool Pakmen::is_player_cell(Pakmen::GameBoard* board, std::tuple<int, int> pos)  {
  int x, y;

  std::tie (y, x) = pos;
  return board->board[y][x] == Pakmen::USER_CELL;
}

bool Pakmen::is_ghost_movable_cell(Pakmen::GameBoard* board, std::tuple<int, int> pos) {
  int x, y;

  std::tie (y, x) = pos;

  if (x < 0 || y < 0)
    return false;

  int board_cell = board->board[y][x];

  return board_cell != Pakmen::WALL_CELL;
}

bool Pakmen::is_movable_cell(Pakmen::GameBoard* board, std::tuple<int, int> pos) {
  int x, y;

  std::tie (y, x) = pos;

  if (x < 0 || y < 0)
    return false;

  int board_cell = board->board[y][x];

  if (board_cell == Pakmen::WALL_CELL)
    return false;

  return (board_cell != Pakmen::GHOST_CELL && board_cell != Pakmen::DUMMIE_GHOST_CELL) || board->powered != 0;
}

bool Pakmen::is_eatable_cell(Pakmen::GameBoard* board, std::tuple<int, int> pos) {
  int x, y;

  std::tie (y, x) = pos;

  if (x < 0 || y < 0) {
    return false;
  }

  int board_cell = board->board[y][x];

  if (board_cell == Pakmen::WALL_CELL){
    return false;
  }

  return (board_cell == Pakmen::EATABLE_CELL || board_cell == Pakmen::POWER_CELL);
}

bool Pakmen::is_eatable_cherry_cell(Pakmen::GameBoard* board, std::tuple<int, int> pos) {
  int x, y;

  std::tie (y, x) = pos;

  if (x < 0 || y < 0) {
    return false;
  }

  int board_cell = board->board[y][x];

  if (board_cell == Pakmen::WALL_CELL){
    return false;
  }

  return (board_cell == Pakmen::CHERRY_CELL);
}
