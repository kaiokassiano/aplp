% Declaração do tabuleiro

height(15).
width(13).

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

% Fruits

create_fruit(X, Y):-
  (
    not(call(wall(pos(X, Y)))),
    not(call(pacman(pos(X, Y)))),
    not(call(cherry(pos(X, Y)))),
    not(call(power(pos(X, Y)))),
    assertz(fruit(pos(X, Y)))
  );
  true.

initialize_fruits(_, -1).
initialize_fruits(-1, Y):-
  K is Y - 1,
  width(W),
  initialize_fruits(W, K).

initialize_fruits(X, Y):-
  create_fruit(X, Y),
  K is X - 1,
  initialize_fruits(K, Y).

initialize_fruits:-
  width(W),
  height(H),
  J is W - 1,
  K is H - 1,
  initialize_fruits(J, K).

% Display

new_line(Y):-
  Y > 0 -> write("#"), nl; print_horizontal_border.

print_horizontal_border:-
  write("###############"),
  nl.

left_wall(K):-
  K == -1 -> write("#"); true.

print_matrix(_, -1).
print_matrix(-1, Y):-
  K is Y - 1,
  width(W),
  J is W - 1,
  print_matrix(J, K),
  new_line(Y).

print_matrix(X, Y):-
  K is X - 1,
  print_matrix(K, Y),
  left_wall(K),
  (
    wall(pos(X, Y)), write("#");
    pacman(pos(X, Y)), write("P");
    ghost(pos(X, Y)), write("G");
    cherry(pos(X, Y)), write("C");
    power(pos(X, Y)), write("*");
    fruit(pos(X, Y)), write(".");
    write(" ")
  ).

display_state:-
  tty_clear,
  height(H),
  width(W),
  J is W - 1,
  K is H - 1,
  print_matrix(J, K),
  write("#"),
  nl,
  print_horizontal_border.

% Main

can_move(X, Y):-
  not(call(wall(pos(X, Y)))),
  X >= 0,
  Y >= 0,
  width(W),
  height(H),
  X < W,
  Y < H.

move_pacman(X, Y):-
  can_move(X, Y) ->
    retractall(pacman(pos(_, _))),
    assertz(pacman(pos(X, Y)));
  true.

% Up
update_pacman(8):-
  pacman(pos(PacmanX, PacmanY)),
  K is PacmanY - 1,
  move_pacman(PacmanX, K).

% Left
update_pacman(4):-
  pacman(pos(PacmanX, PacmanY)),
  J is PacmanX - 1,
  move_pacman(J, PacmanY).

% Down
update_pacman(2):-
  pacman(pos(PacmanX, PacmanY)),
  K is PacmanY + 1,
  move_pacman(PacmanX, K).

% Right
update_pacman(6):-
  pacman(pos(PacmanX, PacmanY)),
  J is PacmanX + 1,
  move_pacman(J, PacmanY).

update_pacman(_).

update_fruits:-
  pacman(pos(X, Y)),
  fruit(pos(X, Y)) ->
    retract(fruit(pos(X, Y))); % TODO: also update score
  true.

play:-
  game_over ->
    write("Game Over!"),
    nl,
    halt(0);
  read(P),
  update_pacman(P),
  update_fruits,
  % move_ghosts,
  display_state,
  play.

game_over:-
  not(call(fruit(_)));
  not(call(pacman(_)));
  pacman(pos(PacmanX, PacmanY)),
  ghost(pos(PacmanX, PacmanY)).

:- initialization(main).
main:-
  assertz(pacman(pos(6, 3))),
  initialize_fruits,
  display_state,
  play,
  halt(0).

