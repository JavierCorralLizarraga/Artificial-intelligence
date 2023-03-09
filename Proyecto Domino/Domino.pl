
% main inicializa el estado del juego
% game_state guarda todo lo que se conoce acerca del estado actual: fichas que no se han jugado, número de fichas por jugador, 
% número de fichas en el pozo, fichas que tenemos nosotros y las dos fichas abiertas en el tablero
main:-
  % Guarda qué fichas quedan
  % TODO: Cómo manejamos el orden de las entradas?
  game_state(rem_pieces, [(0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (2, 2), (2, 3), (2, 4), (2, 5), (2, 6), (3, 3), (3, 4), (3, 5), (3, 6), (4, 4), (4, 5), (4, 6), (5, 5), (5, 6), (6, 6)]),
  game_state(num_player_pieces, 7),
  game_state(num_op_pieces, 7),
  game_state(player_pieces, []),
  game_state(open_pieces, [])
  .

% Lee y guarda las fichas que tiene el jugador
read_ini_player_hand():-
  write("Ingresa cada ficha con el formato "(N1, N2)" y oprime enter"),
  read(Piece),
  order_input(Piece, Ordered_piece),
  read_ini_player_hand([Ordered_piece])
  .
read_ini_player_hand([Prev_piece | _]):-
  read(Piece),
  read_ini_player_hand([Piece, Prev_piece | _])
  .

% Se asegura que las fichas que se ingresen tengan el número menor a la izquierda
order_input_aux(Out, Out). % Iguala Out a la entrada
order_input([N1, N2], Out):-
  ( N1 > N2 ->
    % reordena
    order_input_aux([N2, N1], Out);
    % deja el orden igual
    order_input_aux([N1, N2], Out)
  ).

% Coloca la primera ficha
place_first(Ficha):-
  game_state
  .
  

% TODO: ordenar una ficha de menor entrada a mayor antes de utilizarla / quitarla de la mesa
% TODO: hacer que el sistema confirme la entrada o permita deshacer la jugada, en caso de que nos equivoquemos al ingresarla
% Side: "L" (Left) o "R" (R)
place_piece([N1, N2], Side):-
  retract(rem_pieces( (N1, N2) )), % quita la ficha del pool disponible
  .

% Métodos de interacción con el usuario
create_



