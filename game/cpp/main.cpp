#include <iostream>
#include <ncurses.h>

int main() {

    initscr();
    clear();
    raw();
    noecho();

    printw("Press something\n");
    char c = getch();
    printw("Pressed: %c\n", c);

    printw("Press anything to end");
    getch();
    endwin();

    return 0;
}
