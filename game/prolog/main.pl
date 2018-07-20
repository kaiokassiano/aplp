:-['map'].

finished :- false.

initGame:-nl,
  startEngine(),
  nl.

startEngine :- tty_clear,
  writeln('Welcome to pakmen!'),
  buildMap,
  writeln('Type W, S, A, D to move.'),
  % processMove(getMove),
  checkUser,
  startEngine.

% getMove :-

% processMove(MOVE) :-

checkUser :- (
    finished ->
      write("Game over."),
      nl,
      writeln("Thanks for playing."),
      fail
    ;
    true
  ).
