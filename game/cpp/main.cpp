#include "screen.h"
#include "model.h"
#include "query.h"
#include "utils.h"

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
  bool finished = 0;

  while(1){
    auto board = init();

    while (!finished) {
      clear_screen();
      print_board(board);

      auto input = get_input();

      if (!move_user(board, input)) {
        print_invalid_input();
      }

      if (is_finished(board))
        finished = 1;
    }

    finish(board);
  }

  return 0;
}

