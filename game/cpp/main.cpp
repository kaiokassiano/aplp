#include "screen.h"
#include "model.h"
#include "query.h"

using namespace Pakmen;

GameBoard* init() {
  init_screen();

  auto board = new GameBoard();

  return board;
}

void finish(GameBoard* board) {
  delete board;

  clear_screen();
  finish_screen();
}

int main() {
  auto board = init();

  while (true) {
    clear_screen();
    print_board(board);

    auto input = get_input();

    if (!move_user(board, input)) {
      print_invalid_input();
    }

    if (board->is_finished())
      break;
  }

  finish(board);

  return 0;
}

