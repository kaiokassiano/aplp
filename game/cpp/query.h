#ifndef PAKMEN_QUERY_H
#define PAKMEN_QUERY_H

#include <vector>
#include <tuple>
#include "model.h"

namespace Pakmen {

/** 
 * Find the first occurrence of the object from the Pakmen::GameBoard 
 */
std::tuple<int, int> find_object(GameBoard* board, int object);

/**
 * Find all the occurrences of the object from the Pakmen::GameBoard
 */
std::vector<std::tuple<int, int>> find_all_objects(GameBoard* board, int object);

/**
 * Check if the specified position is a cell the user can move to
 */
bool is_movable_cell(GameBoard* board, std::tuple<int, int> pos);

} // Namespace

#endif // PAKMEN_QUERY_H
