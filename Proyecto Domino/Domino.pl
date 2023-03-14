% ------------------- Estado de juego --------------------
% main inicializa el estado del juego
% 
% game_state guarda todo lo que se conoce acerca del estado actual: fichas que no se han jugado, número de fichas por jugador, 
% número de fichas en el pozo, fichas que tenemos nosotros y las dos fichas abiertas en el tablero
:- dynamic game_state/2, turn/1, next_turn/0, game_over/0.
main:-
  % Guarda qué fichas no se han jugado
  assert(game_state(rem_tiles, [(0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (2, 2), (2, 3), (2, 4), (2, 5), (2, 6), (3, 3), (3, 4), (3, 5), (3, 6), (4, 4), (4, 5), (4, 6), (5, 5), (5, 6), (6, 6)])),
  assert(game_state(num_player_tiles, 0)), % inicia en 0 para contarlas después
  assert(game_state(num_op_tiles, 7)),
  assert(game_state(player_hand, [])),
  assert(game_state(open_tiles, [])), % las dos fichas junto a las que se puede colocar una nueva
  read_ini_player_hand,
  print_player_hand,
  set_starting_player,
  %first_turn,
  !,
  game_loop
  .

% Regresa verdadero si el juego no se ha acabado
:- dynamic game_over/0.
game_over:-false.

% Invierte el valor de turn
next_turn:-
	turn(Turn),
	New_turn is (Turn+1) mod 2,
	retract(turn(Turn)),
	assert(turn(New_turn))
	( New_turn =:= 0 ->
			write("Tu turno"), nl;
			write("Turno del oponente"), nl
	).

% 
game_loop:-
	not(game_over),
  turn(Turn),
  ( Turn = 1 -> 
  		player_turn;
   		op_turn
  ),
  game_state(num_player_tiles, Num_player_tiles),
  game_state(num_op_tiles, Num_op_tiles),
  ( Num_player_tiles = 0; Num_op_tiles = 0 ->
   		end_game, !;
   		!
  ),
  next_turn,
  game_loop.

% 
player_turn:-
	fail
	.

% 
op_turn:-
	op_steal,
	write("¿Qué tiró el oponente? Si no tuvo jugadas legales, escribe '7,7.'"), nl,
	read(Op_tile), conf(Op_tile),
	(	Op_tile =:= (7, 7) ->
			write("acabó"), end_game;
			op_pick_open(Op_tile)
	),
	op_play
	.

%
op_steal:-
	write("¿El oponente tomó del pozo? 's.' o 'n.'"),
	read(Op_stock),
	(	Op_stock =:= s ->
			% Suma la cantidad de fichas que tomó el oponente del pozo
			write("¿Cuántas?"), nl, read(Op_stock_num), conf(Op_stock_num),
			game_state(rem_pieces, Rem_pieces),
			(	length(Rem_pieces) < Op_stock_num ->
					write("No hay suficientes fichas en el pozo"), nl, op_steal;
					!
			),
			game_state(num_op_tiles, Num_op_tiles),
			New_num_op_tiles is Num_optiles + Op_stock_num,
			retract(game_state(num_op_tiles, Num_op_tiles)),
			assert(game_state(num_op_tiles, New_num_op_tiles))
	).

% 
op_pick_open(Op_tile):-
	game_state(open_tiles, [Open_tile1, Open_tile1]),
	write("Las fichas abiertas son: "), 
	write(Open_tile1), write("  "), write(Open_tile1), nl,	
	write("¿Junto a cuál ficha abierta tiró el oponente?"),
	read(Open_tile_op), conf(Open_tile_op),
	(	Open_tile_op =:= Open_tile1; Open_tile_op =:= Open_tile2 ->
			(	legal_move(Open_tile_op) ->
					place_tile(Open_tile_op, Op_tile);
					write("¡Jugada ilegal!"), nl, op_turn
			);
			write("No elegiste una ficha abierta en el tablero"), nl, op_play
	).

op_play(Op_tile):-


% Termina el juego y revisa quién ganó
end_game:-
	retract(not(game_over)), assert(game_over),
	game_state(num_player_tiles, Num_player_tiles),
	game_state(num_player_tiles, Num_op_tiles),
	(	Num_player_tiles < Num_op_tiles ->
			write("Ganaste"), nl;
		Num_player_tiles > Num_op_tiles ->
			write("Ganó el oponente"), nl;
			write("Empataron"), nl
	).

% ------------------- Interacción con el usuario --------------------
% Lee y guarda las fichas que tiene el jugador
read_ini_player_hand():-
 	% ------- Lectura de entrada --------
 	game_state(num_player_tiles, Tile_count), % Consulta cuántas piezas llevamos
	( tile_count = 0 ->
  	% Si es la primera ficha
  	write("Ingresa cada ficha con el formato 'N1, N2.' y oprime enter"), nl;
  	% En caso contrario
  	write("Tienes "), write(Tile_count), write(" fichas en tu mano. Ingresa otra"), nl
  ),
  read(Tile),
  order_input(Tile, Ordered_tile),
  
  % -------- Confirmación de la entrada ---------
  ( conf_input(Ordered_tile) ->
  	!, read_ini_player_hand;
  	!
  ),
  
  % -------- Revisión de la ficha ingresada ---------
  game_state(rem_tiles, Rem_tiles),
  game_state(player_hand, Player_hand),
  ( not(member(Ordered_tile, Rem_tiles)) ->
  	!, write("La ficha ingresada no está disponible"), nl, read_ini_player_hand;
  	!
  ),
  ( member(Ordered_tile, Player_hand) ->
  	!, write("Ya tienes la ficha ingresada"), nl, read_ini_player_hand;
  	!
  ),
  
  % -------- Actualización de game_state ---------
  % Actualiza las fichas disponibles
  retract(game_state(rem_tiles, Rem_tiles)),
  delete(Rem_tiles, Ordered_tile, New_rem_tiles),
  assert(game_state(rem_tiles, New_rem_tiles)),
  
  % Actualiza la mano del jugador
  retract(game_state(player_hand, Player_hand)),
  append([Ordered_tile], Player_hand, New_player_hand),
  assert(game_state(player_hand, New_player_hand)),
  
  % Actualiza el número de fichas
  retract(game_state(num_player_tiles, Tile_count)),
  New_tile_count is Tile_count+1,
  assert(game_state(num_player_tiles, New_tile_count)),
  % Se detiene cuando llegamos a 7 fichas
  (New_tile_count < 3 -> % cambiar a 7
  	read_ini_player_hand;
  	! % regresa verdadero sin importar el caso
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
	assert(turn(Turn)).

% Confirma una entrada cualquiera
conf_input(Inp):-
	write("Ingresaste: "), write(Inp), 
  write("; escribe 'n.' para reintentar o 's.' para confirmar"), nl,
  read(Conf),
  Conf = n. % regresa true si el usuario va a reingresar la entrada

% 
print_player_hand:-
	game_state(player_hand, Player_hand),
	nl, write("Tus fichas:   "),
	print_player_hand(Player_hand).

print_player_hand([]):-nl.
print_player_hand([Tile|Rest]):-
	write(Tile), write("  "),
	print_player_hand(Rest).

% ------------------- Actualización automática del tablero --------------------
% Coloca la primera ficha
place_first(Ficha):-
  game_state
  .
  
% Side: "L" (Left) o "R" (R)
place_tile([N1, N2], Side):-
  retract(rem_tiles( (N1, N2) )) % quita la ficha del pool disponible
  .

legal_move(Open_tile, [Player_tile_N1, Player_tile_N2]):-
	Open_tile =:= Player_tile_N1;
	Open_tile =:= Player_tile_N2.

legal_move([Player_tile_N1, Player_tile_N2]):-
	game_state(open_tiles, [Open_tile_N1, Open_tile_N2]),
	Open_tile_N1 = Player_tile_N1;
	Open_tile_N1 = Player_tile_N2;
	Open_tile_N2 = Player_tile_N1;
	Open_tile_N2 = Player_tile_N2.


% ------------------- Toma de decisiones --------------------
% Aquí va el árbol mamado







