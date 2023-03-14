% main inicializa el estado del juego
% game_state guarda todo lo que se conoce acerca del estado actual: fichas que no se han jugado, número de fichas por jugador, 
% número de fichas en el pozo, fichas que tenemos nosotros y las dos fichas abiertas en el tablero
main:-
  % Guarda qué fichas quedan
  asserta(game_state(rem_pieces, [(0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (2, 2), (2, 3), (2, 4), (2, 5), (2, 6), (3, 3), (3, 4), (3, 5), (3, 6), (4, 4), (4, 5), (4, 6), (5, 5), (5, 6), (6, 6)])),
  asserta(game_state(num_player_pieces, 0)),
  asserta(game_state(num_op_hand, 7)),
  asserta(game_state(player_hand, [])),
  asserta(game_state(open_pieces, [])),
  read_ini_player_hand,
  set_starting_player,
  %first_turn,
  !
  %next_turn,
  %turn(Turn),
  %( Turn=1 -> player_turn),
  % game_loop
  .

not(game_over).

% Regresa verdadero si el juego no se ha acabado
next_turn:-
	not(game_over).

% ------------------- Métodos de interacción con el usuario --------------------
%create_

% Lee y guarda las fichas que tiene el jugador
read_ini_player_hand():-
 	% ------- Lectura de entrada --------
 	game_state(num_player_pieces, Piece_count), % Consulta cuántas piezas llevamos
	( Piece_count = 0 ->
  	% Si es la primera ficha
  	write("Ingresa cada ficha con el formato 'N1, N2.' y oprime enter"), nl;
  	% En caso contrario
  	write("Tienes "), write(Piece_count), write(" fichas en tu mano. Ingresa otra"), nl
  ),
  read(Piece),
  write(Piece),
  order_input(Piece, Ordered_piece),
  write(Ordered_piece),
  
  % -------- Confirmación de la entrada ---------
  ( conf_input(Ordered_piece) ->
  	!, read_ini_player_hand;
  	!
  ),
  
  % -------- Revisión de la ficha ingresada ---------
  game_state(rem_pieces, Rem_pieces),
  game_state(player_hand, Player_hand),
  ( not(member(Ordered_piece, Rem_pieces)) ->
  	!, write("La ficha ingresada no está disponible"), nl, read_ini_player_hand;
  	!
  ),
  ( member(Ordered_piece, Player_hand) ->
  	!, write("Ya tienes la ficha ingresada"), nl, read_ini_player_hand;
  	!
  ),
  
  % -------- Actualización de game_state ---------
  % Actualiza las fichas disponibles
  retract(game_state(rem_pieces, Rem_pieces)),
  delete(Rem_pieces, Ordered_piece, New_rem_pieces),
  asserta(game_state(rem_pieces, New_rem_pieces)),
  
  % Actualiza la mano del jugador
  retract(game_state(player_hand, Player_hand)),
  append([Ordered_piece], Player_hand, New_player_hand),
  asserta(game_state(player_hand, New_player_hand)),
  
  % Actualiza el número de fichas
  retract(game_state(num_player_pieces, Piece_count)),
  New_piece_count is Piece_count+1,
  asserta(game_state(num_player_pieces, New_piece_count)),
  % Se detiene cuando llegamos a 7 fichas
  (New_piece_count < 7 ->
  	read_ini_player_hand
  ).

% Se asegura que las fichas que se ingresen tengan el número menor a la izquierda
order_input_aux(Out, Out). % Iguala Out a la entrada
order_input((N1, N2), Out):-
  ( N1 > N2 ->
    % reordena
    order_input_aux((N2, N1), Out);
    % deja el orden igual
    order_input_aux((N1, N2), Out)
  ).

% Establece qué jugador empieza
set_starting_player():-
	write("Escribe '0.' si empezamos nosotros o '1.' si empieza el oponente"),
	read(Turn),
	asserta(turn(Turn)).

% Confirma una entrada cualquiera
conf_input(Inp):-
	write("Ingresaste: "), write(Inp), 
  write("; escribe 'n.' para reintentar o cualquier otra tecla para confirmar"), nl,
  read(Conf),
  Conf = n. % regresa true si el usuario va a reingresar la entrada



%%%%%%%%%%%%%%%%%%%%%
% Coloca la primera ficha
place_first(Ficha):-
  game_state
  .
  

% TODO: ordenar una ficha de menor entrada a mayor antes de utilizarla / quitarla de la mesa
% TODO: hacer que el sistema confirme la entrada o permita deshacer la jugada, en caso de que nos equivoquemos al ingresarla
% Side: "L" (Left) o "R" (R)
place_piece([N1, N2], Side):-
  retract(rem_pieces( (N1, N2) )) % quita la ficha del pool disponible
  .





