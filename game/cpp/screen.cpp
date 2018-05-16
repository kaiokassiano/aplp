#include "screen.h"

static const int BLUE_FOREGROUND = 1;
static const int RED_FOREGROUND = 2;
static const int MAGENTA_FOREGROUND = 3;
static const int YELLOW_FOREGROUND = 4;

void Pakmen::init_screen() {
  initscr();
  cbreak();
  echo();

  start_color();

  init_pair(BLUE_FOREGROUND, COLOR_BLUE, COLOR_BLACK);
  init_pair(RED_FOREGROUND, COLOR_RED, COLOR_BLACK);
  init_pair(MAGENTA_FOREGROUND, COLOR_MAGENTA, COLOR_BLACK);
  init_pair(YELLOW_FOREGROUND, COLOR_YELLOW, COLOR_BLACK);
}

void Pakmen::finish_screen() {
  endwin();
}

void Pakmen::clear_screen() {
  clear();
}

void Pakmen::print_board(Pakmen::GameBoard* board) {
  for (int i = 0; i < Pakmen::BOARD_HEIGHT; i++) {
    for (int j = 0; j < Pakmen::BOARD_WIDTH; j++) {
      int board_cell = board->board[i][j];

      if (board_cell == Pakmen::USER_CELL) {
        attron(COLOR_PAIR(YELLOW_FOREGROUND));
        printw("%s", "P");
        attroff(COLOR_PAIR(YELLOW_FOREGROUND));
      } else if (board_cell == Pakmen::GHOST_CELL) {
        attron(COLOR_PAIR(MAGENTA_FOREGROUND));
        printw("%s", "G");
        attroff(COLOR_PAIR(MAGENTA_FOREGROUND));
      } else if (board_cell == Pakmen::CHERRY_CELL) {
        attron(COLOR_PAIR(RED_FOREGROUND));
        printw("%s", "C");
        attroff(COLOR_PAIR(RED_FOREGROUND));
      } else if (board_cell == Pakmen::EMPTY_CELL) {
        printw("%s", ".");
      } else if (board_cell == Pakmen::WALL_CELL) {
        attron(A_BOLD);
        attron(COLOR_PAIR(BLUE_FOREGROUND));
        printw("%s", "#");
        attroff(COLOR_PAIR(BLUE_FOREGROUND));
        attroff(A_BOLD);
      }

      if (j == Pakmen::BOARD_WIDTH - 1) {
        printw("\n");
      }
    }
  }

  refresh();
}

string getstring()
{
  string input;

  nocbreak();
  echo();

  int ch = getch();

  while (ch != '\n') {
    input.push_back( ch );
    ch = getch();
  }

  cbreak();

  return input;
}

string Pakmen::get_input() {
  printw("Where do you want to go? (up, down, left, right)\n");

  return getstring();
}

void Pakmen::print_invalid_input() {
  printw("Invalid input.");
  refresh();
}

bool Pakmen::move_user(Pakmen::GameBoard* board, string action) {
  auto user_pos = Pakmen::find_object(board, Pakmen::USER_CELL);

  int x, y;

  std::tie (y, x) = user_pos;

  std::tuple<int, int> new_pos;

  if (action == "up" || action == "u")
    new_pos = std::make_tuple(y - 1, x);
  else if (action == "down" || action == "d")
    new_pos = std::make_tuple(y + 1, x);
  else if (action == "left" || action == "l")
    new_pos = std::make_tuple(y, x - 1);
  else if (action == "right" || action == "r")
    new_pos = std::make_tuple(y, x + 1);
  else
    return false;

  if (Pakmen::is_movable_cell(board, new_pos)) {
    board->board[y][x] = Pakmen::EMPTY_CELL;
    board->board[std::get<0>(new_pos)][std::get<1>(new_pos)] = Pakmen::USER_CELL;
  }

  return true;
}
