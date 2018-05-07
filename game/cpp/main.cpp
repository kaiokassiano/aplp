#include <iostream>

void clear_screen();

int main() {
    std::string x;

    clear_screen();

    printf("Press ENTER");
    std::cin.get();

    clear_screen();

    printf("Press ENTER again");
    std::cin.get();

    std::cout << "Bye :) " << std::endl;

    return 0;
}

void clear_screen() {
    std::cout << "\033[2J\033[1;1H";
}
