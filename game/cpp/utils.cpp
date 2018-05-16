#include "utils.h"

bool Pakmen::is_finished(Pakmen::GameBoard* board) {
  return Pakmen::find_all_objects(board, Pakmen::EATABLE_CELL).empty();
}
