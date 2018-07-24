% Declaração do tabuleiro

height(14).
width(11).

pacman(pos(6, 3)).

wall(pos(2, 1)).
wall(pos(3, 1)).
wall(pos(2, 2)).
wall(pos(6, 1)).
wall(pos(9, 1)).
wall(pos(10, 1)).
wall(pos(10, 2)).
wall(pos(4, 3)).
wall(pos(8, 3)).
wall(pos(2, 6)).
wall(pos(3, 6)).
wall(pos(6, 5)).
wall(pos(6, 6)).
wall(pos(9, 6)).
wall(pos(10, 6)).
wall(pos(2, 8)).
wall(pos(3, 8)).
wall(pos(6, 9)).
wall(pos(6, 8)).
wall(pos(9, 8)).
wall(pos(10, 8)).
wall(pos(4, 11)).
wall(pos(8, 11)).
wall(pos(2, 13)).
wall(pos(3, 13)).
wall(pos(2, 12)).
wall(pos(6, 13)).
wall(pos(9, 13)).
wall(pos(10, 13)).
wall(pos(10, 12)).

cherry(pos(9, 12)).
cherry(pos(3, 12)).

ghost(pos(1, 7)).
ghost(pos(11, 7)).

power(pos(6, 14)).

% Display

new_line(Y):-
  Y > 0 -> nl; true.

print_matrix(_, -1).
print_matrix(0, Y):-
  K is Y - 1,
  width(W),
  print_matrix(W, K),
  new_line(Y).

print_matrix(X, Y):-
  K is X - 1,
  print_matrix(K, Y),
  (
    wall(pos(X, Y)), write("#");
    cherry(pos(X, Y)), write("C");
    pacman(pos(X, Y)), write("P");
    write(".")
  ).

print_matrix:-
  height(H),
  width(W),
  print_matrix(W, H),
  nl.

% Main

:- initialization(main).
main:-
  print_matrix,
  halt(0).

