#include "screen.h"
#include "model.h"
#include "query.h"
#include "utils.h"

using namespace Pakmen;

GameBoard *init()
{
  init_screen();

  auto board = new GameBoard();

  return board;
}

void finish(GameBoard *board, bool lost)
{
  delete board;

  clear_screen();
  finish_screen(lost);
}

int main()
{
  while (1)
  {
    int finished = 0;
    int lost = 0;

    auto board = init();
    while (!finished)
    {
      clear_screen();
      print_board(board);

      auto input = get_input();

      if (!move_user(board, input))
      {
        print_invalid_input();
      }


      if(Pakmen::is_game_over(board)){
        finished = 1;
        lost = 1;
      }
      if (is_finished(board))
        finished = 1;
    }
    if (lost) {
      finish(board, true);
    } else {
      finish(board, false);
    }
  }

  return 0;
}
