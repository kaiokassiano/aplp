#ifndef PAKMEN_SCREEN_H
#define PAKMEN_SCREEN_H

#include <ncurses.h>
#include <string>
#include "query.h"
#include "model.h"

using std::string;

namespace Pakmen {

/**
 * Initialized the screen with the ncurses library, allocating
 * required resources
 */
void init_screen();

/**
 * Finalized the screen, freeing up used memory
 */
void finish_screen();

/**
 * Clears the screen
 */
void clear_screen();

/**
 * Get the user input
 */
string get_input();

/**
 * Print an invalid input message
 */
void print_invalid_input();

/**
 * Print the game board
 */
void print_board(GameBoard* board);

/**
 * Move the user position. Return false if the input isn't recognized
 */
bool move_user(GameBoard* board, string action);

} // Namespace

#endif // PAKMEN_SCREEN_H
