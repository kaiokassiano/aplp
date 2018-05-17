#include "screen.h"
#include <string.h>

static const int BLUE_FOREGROUND = 1;
static const int RED_FOREGROUND = 2;
static const int MAGENTA_FOREGROUND = 3;
static const int YELLOW_FOREGROUND = 4;
static const std::vector<std::string> MOVES = {"RIGHT", "UP", "DOWN", "LEFT"};

int current_score = 0;
int highest_score = 0;

void Pakmen::init_screen()
{
  initscr();
  cbreak();
  noecho();

  start_color();

  init_pair(BLUE_FOREGROUND, COLOR_BLUE, COLOR_BLACK);
  init_pair(RED_FOREGROUND, COLOR_RED, COLOR_BLACK);
  init_pair(MAGENTA_FOREGROUND, COLOR_MAGENTA, COLOR_BLACK);
  init_pair(YELLOW_FOREGROUND, COLOR_YELLOW, COLOR_BLACK);
}

void Pakmen::finish_screen()
{
  if (current_score > highest_score)
  {
    highest_score = current_score;
  }

  echo();
  printw("Congratulations! You won!\nPress ENTER to finish!");
  getch();
  endwin();
}

void Pakmen::clear_screen()
{
  clear();
}

void Pakmen::print_board(Pakmen::GameBoard *board)
{
  for (int i = 0; i < Pakmen::BOARD_HEIGHT; i++)
  {
    for (int j = 0; j < Pakmen::BOARD_WIDTH; j++)
    {
      int board_cell = board->board[i][j];

      if (board_cell == Pakmen::USER_CELL)
      {
        attron(COLOR_PAIR(YELLOW_FOREGROUND));
        printw("%s", "P");
        attroff(COLOR_PAIR(YELLOW_FOREGROUND));
      }
      else if (board_cell == Pakmen::GHOST_CELL || board_cell == Pakmen::DUMMIE_GHOST_CELL)
      {
        if (Pakmen::powered != 1)
          attron(COLOR_PAIR(Pakmen::powered ? BLUE_FOREGROUND : MAGENTA_FOREGROUND));
        printw("%s", "G");
        if (Pakmen::powered != 1)
          attroff(COLOR_PAIR(Pakmen::powered ? BLUE_FOREGROUND : MAGENTA_FOREGROUND));
      }
      else if (board_cell == Pakmen::CHERRY_CELL)
      {
        attron(COLOR_PAIR(RED_FOREGROUND));
        printw("%s", "C");
        attroff(COLOR_PAIR(RED_FOREGROUND));
      }
      else if (board_cell == Pakmen::EMPTY_CELL)
      {
        printw("%s", " ");
      }
      else if (board_cell == Pakmen::EATABLE_CELL)
      {
        printw("%s", ".");
      }
      else if (board_cell == Pakmen::POWER_CELL)
      {
        attron(A_BOLD);
        printw("%s", "O");
        attroff(A_BOLD);
      }
      else if (board_cell == Pakmen::WALL_CELL)
      {
        attron(A_BOLD);
        attron(COLOR_PAIR(BLUE_FOREGROUND));
        printw("%s", "#");
        attroff(COLOR_PAIR(BLUE_FOREGROUND));
        attroff(A_BOLD);
      }

      if (j == Pakmen::BOARD_WIDTH - 1)
      {
        printw("\n");
      }
    }
  }

  print_score();
  refresh();
}

string getstring()
{
  string input;

  int ch = getch();
  input.push_back(ch);

  return input;
}

string Pakmen::get_input()
{
  printw("\nWhere do you want to go? (move with w, a, s, d)\n");

  return getstring();
}

void Pakmen::print_invalid_input()
{
  printw("Invalid input.");
  refresh();
}

bool Pakmen::move_user(Pakmen::GameBoard *board, string action)
{
  char c = std::tolower(action.c_str()[0]);

  auto user_pos = Pakmen::find_object(board, Pakmen::USER_CELL);

  int x, y;

  std::tie(y, x) = user_pos;

  std::tuple<int, int> new_pos;

  if (c == 'w')
    new_pos = std::make_tuple(y - 1, x);
  else if (c == 's')
    new_pos = std::make_tuple(y + 1, x);
  else if (c == 'a')
    new_pos = std::make_tuple(y, x - 1);
  else if (c == 'd')
    new_pos = std::make_tuple(y, x + 1);
  else
    return false;

  if (Pakmen::is_eatable_cell(board, new_pos))
  {
    current_score += 1;
  }

  if (Pakmen::is_eatable_cherry_cell(board, new_pos))
  {
    current_score += 10;
  }

  if (Pakmen::is_movable_cell(board, new_pos))
  {
    if (board->board[std::get<0>(new_pos)][std::get<1>(new_pos)] == Pakmen::POWER_CELL)
    {
      Pakmen::powered = Pakmen::POWERED_TIME;
    }
    board->board[y][x] = Pakmen::EMPTY_CELL;
    board->board[std::get<0>(new_pos)][std::get<1>(new_pos)] = Pakmen::USER_CELL;
    if (Pakmen::powered > 0)
    {
      Pakmen::powered--;
    }
  }
  move_ghosts(board, new_pos);
  return true;
}

void Pakmen::move_ghosts(Pakmen::GameBoard *board, std::tuple<int, int> user_pos)
{
  auto dummie_pos = Pakmen::find_object(board, Pakmen::DUMMIE_GHOST_CELL);
  auto ghost_pos = Pakmen::find_object(board, Pakmen::GHOST_CELL);

  int x, y;

  std::tie(y, x) = dummie_pos;

  std::tuple<int, int> new_dummie_pos;
  int move = rand() % MOVES.size();
  if (strcmp(MOVES[move].c_str(), "UP"))
  {
    new_dummie_pos = std::make_tuple(y - 1, x);
  }
  else if (strcmp(MOVES[move].c_str(), "DOWN"))
  {
    new_dummie_pos = std::make_tuple(y + 1, x);
  }
  else if (strcmp(MOVES[move].c_str(), "LEFT"))
  {
    new_dummie_pos = std::make_tuple(y, x - 1);
  }
  else
  {
    new_dummie_pos = std::make_tuple(y, x + 1);
  }

  if (Pakmen::is_movable_cell(board, new_dummie_pos))
  {
    board->board[y][x] = Pakmen::EMPTY_CELL;
    board->board[std::get<0>(new_dummie_pos)][std::get<1>(new_dummie_pos)] = Pakmen::DUMMIE_GHOST_CELL;
  }
}

void Pakmen::print_score()
{
  printw("\nYour score: %d", current_score);
  printw("\nHighest score: %d\n", highest_score);
}