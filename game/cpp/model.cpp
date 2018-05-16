#include "model.h"

bool Pakmen::GameBoard::is_finished() {
  return Pakmen::find_all_objects(this, EATABLE_CELL).size() == 0;
}
