#include "screen.h"
#include <string.h>

static const int BLUE_FOREGROUND = 1;
static const int RED_FOREGROUND = 2;
static const int MAGENTA_FOREGROUND = 3;
static const int YELLOW_FOREGROUND = 4;
static const int RIGHT = 0;
static const int UP = 1;
static const int DOWN = 2;
static const int LEFT = 3;

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

void Pakmen::finish_screen(bool lost)
{
  echo();
  if (current_score > highest_score)
  {
    highest_score = current_score;
  }

  if(lost){
    current_score = 0;
    printw("Perdeu irmao!");
  }else{
    printw("Congratulations! You won!\nPress ENTER to finish!");
  }

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
        if (board->powered != 1)
          attron(COLOR_PAIR(board->powered ? BLUE_FOREGROUND : MAGENTA_FOREGROUND));
        printw("%s", "G");
        if (board->powered != 1)
          attroff(COLOR_PAIR(board->powered ? BLUE_FOREGROUND : MAGENTA_FOREGROUND));
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
      board->powered = Pakmen::POWERED_TIME;
    }
    board->board[y][x] = Pakmen::EMPTY_CELL;
    board->board[std::get<0>(new_pos)][std::get<1>(new_pos)] = Pakmen::USER_CELL;
    if (board->powered > 0)
    {
      board->powered--;
    }
  }
  move_ghosts(board, user_pos);
  if(std::get<0>(Pakmen::find_object(board, Pakmen::GHOST_CELL)) == -1){
    board->board[3][9] = Pakmen::GHOST_CELL;
  }
  if(std::get<0>(Pakmen::find_object(board, Pakmen::DUMMIE_GHOST_CELL)) == -1){
    board->board[3][3] = Pakmen::DUMMIE_GHOST_CELL;
  }
  return true;
}

void move_dummy_ghost(Pakmen::GameBoard* board, std::tuple<int, int> user_pos)
{
  auto dummie_pos = Pakmen::find_object(board, Pakmen::DUMMIE_GHOST_CELL);

  int x, y;

  std::tie(y, x) = dummie_pos;
  std::tuple<int, int> new_dummie_pos;

  int move = rand() % 4;

  if (move == UP)
  {
    new_dummie_pos = std::make_tuple(y - 1, x);
  }
  else if (move == DOWN)
  {
    new_dummie_pos = std::make_tuple(y + 1, x);
  }
  else if (move == LEFT)
  {
    new_dummie_pos = std::make_tuple(y, x - 1);
  }
  else
  {
    new_dummie_pos = std::make_tuple(y, x + 1);
  }

  if (Pakmen::is_ghost_movable_cell(board, new_dummie_pos))
  {
    board->board[y][x] = board->temp_dummie;
    board->temp_dummie = board->board[std::get<0>(new_dummie_pos)][std::get<1>(new_dummie_pos)];
    board->board[std::get<0>(new_dummie_pos)][std::get<1>(new_dummie_pos)] = Pakmen::DUMMIE_GHOST_CELL;
  }
}

void move_normal_ghost(Pakmen::GameBoard* board, std::tuple<int, int> user_pos)
{
  auto ghost_pos = Pakmen::find_object(board, Pakmen::GHOST_CELL);

  int ghost_move = Pakmen::get_move(board, ghost_pos, user_pos);
  std::tuple<int, int> new_ghost_pos;
  int ghost_x, ghost_y;

  std::tie(ghost_y, ghost_x) = ghost_pos;
  if (ghost_move ==  UP)
  {
    new_ghost_pos = std::make_tuple(ghost_y - 1, ghost_x);
  }
  else if (ghost_move ==  DOWN)
  {
    new_ghost_pos = std::make_tuple(ghost_y + 1, ghost_x);
  }
  else if(ghost_move ==  LEFT)
  {
    new_ghost_pos = std::make_tuple(ghost_y, ghost_x - 1);
  }
  else
  {
    new_ghost_pos = std::make_tuple(ghost_y, ghost_x + 1);
  }

  if (Pakmen::is_ghost_movable_cell(board, new_ghost_pos))
  {
    board->board[ghost_y][ghost_x] = board->temp_ghost;
    board->temp_ghost = board->board[std::get<0>(new_ghost_pos)][std::get<1>(new_ghost_pos)];
    board->board[std::get<0>(new_ghost_pos)][std::get<1>(new_ghost_pos)] = Pakmen::GHOST_CELL;
  }
}

void Pakmen::move_ghosts(Pakmen::GameBoard *board, std::tuple<int, int> user_pos)
{
  move_dummy_ghost(board, user_pos);
  move_normal_ghost(board, user_pos);
}

int Pakmen::get_move(Pakmen::GameBoard *board, std::tuple<int, int> ghost, std::tuple<int, int> user){
  int ghost_x, ghost_y, user_x, user_y;
  std::tie(ghost_y, ghost_x) = ghost;
  std::tie(user_y, user_x) = user;

  if(ghost_y == user_y){ // Mesma linha
    if (ghost_x < user_x){
      return RIGHT;
    }
    return LEFT;
  }
  if (ghost_x == user_x){ //Mesma coluna
    if (ghost_y < user_y){
      // baixo
      return DOWN;
    }
    return UP;
    //cima
  }
  if(ghost_x < user_x){
    return RIGHT;
    // direita
  }
  return LEFT;
}

void Pakmen::print_score()
{
  printw("\nYour score: %d", current_score);
  printw("\nHighest score: %d\n", highest_score);
}
