%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Estado de juego   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% main inicializa el estado del juego
% 
% game_state guarda todo lo que se conoce acerca del estado actual: fichas que no se han jugado, número de fichas por jugador, 
% número de fichas en el pozo, fichas que tenemos nosotros y las dos fichas abiertas en el tablero
:- dynamic game_state/2, turn/1, next_turn/0, game_over/0.
main:-
  % Guarda qué fichas no se han jugado
  assert(game_state(rem_tiles, [(0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (1, 1), 
  															(1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (2, 2), (2, 3), (2, 4), 
  															(2, 5), (2, 6), (3, 3), (3, 4), (3, 5), (3, 6), (4, 4), (4, 5), 
  															(4, 6), (5, 5), (5, 6), (6, 6)])),
  assert(game_state(num_player_tiles, 0)), % inicia en 0 para contarlas después
  assert(game_state(num_oppo_tiles, 7)),
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
game_over:-false.

% Termina el juego y revisa quién ganó
end_game:-
	retract(not(game_over)), assert(game_over),
	game_state(num_player_tiles, Num_player_tiles),
	game_state(num_player_tiles, Num_oppo_tiles),
	(	Num_player_tiles < Num_oppo_tiles ->
			write("Ganaste"), nl;
		Num_player_tiles > Num_oppo_tiles ->
			write("Ganó el oponente"), nl;
			write("Empataron"), nl
	).

% Invierte el valor de turn
% 0: oponente; 1: jugador
next_turn:-
	turn(Turn),
	New_turn is (Turn+1) mod 2,
	retract(turn(Turn)),
	assert(turn(New_turn)),
	( New_turn =:= 0 ->
			write("Tu turno"), nl;
			write("Turno del oponente"), nl
	).

% Lleva a cabo todos los turnos después del primero hasta que acaba el juego
game_loop:-
	not(game_over),
  turn(Turn),
  ( Turn = 1 -> 
  		player_turn, !;
   		oppo_turn
  ),
  game_state(num_player_tiles, Num_player_tiles),
  game_state(num_oppo_tiles, Num_oppo_tiles),
  ( Num_player_tiles = 0; Num_oppo_tiles = 0 ->
   		end_game, !;
   		!
  ),
  next_turn,
  game_loop.

% Lleva a cabo un turno del jugador (excepto el primero del juego) 
player_turn:-
	fail
	.


% Lleva a cabo un turno del oponente (excepto el primero del juego)
oppo_turn:-
	oppo_steal,
	oppo_pick_tile(Oppo_tile),
	(	Oppo_tile \= (7, 7) ->
			oppo_pick_open(Oppo_tile, Open_tile),
			oppo_play(Oppo_tile, Open_tile);
			!
	).

% En caso de que robe algo del pozo
oppo_steal:-
	write("¿El oponente tomó del pozo? 's.' o 'n.'"), nl,
	read(Oppo_stock),
	(	Oppo_stock =:= s ->
			write("¿Cuántas?"), nl, read(Oppo_stock_num), conf(Oppo_stock_num),
			game_state(rem_pieces, Rem_pieces),
			
			(	length(Rem_pieces) < Oppo_stock_num ->
					write("No hay suficientes fichas en el pozo"), nl, oppo_steal, !;
					!
			),
			
			% Suma las fichas que tomó el oponente del pozo a su mano
			game_state(num_oppo_tiles, Num_oppo_tiles),
			New_num_oppo_tiles is Num_oppo_tiles + Oppo_stock_num,
			write("ahora tiene "), write(New_num_oppo_tiles), % TODO: borrar
			retract(game_state(num_oppo_tiles, Num_oppo_tiles)),
			assert(game_state(num_oppo_tiles, New_num_oppo_tiles));
			!
	).

% Selecciona qué ficha jugó
oppo_pick_tile(Oppo_tile):-
	write("¿Qué tiró el oponente? Si no tuvo jugadas legales, escribe '7,7.'"), nl,
	read(Tile), conf(Tile),
	game_state(rem_pieces, Rem_pieces),
	( Tile \= (7, 7), not(member(Tile, Rem_pieces)) ->
			write("La ficha no está disponible"), nl, oppo_pick_tile(Oppo_tile), !;
			Oppo_tile is Tile % iguala Oppo_tile a la ficha leída y la manda de regreso a oppo_turn
	).

% Elige junto a cuál de las fichas abiertas jugó
oppo_pick_open(Oppo_tile, Open_tile):-
	game_state(open_tiles, [Open_tile1, Open_tile2]),
	write("Las fichas abiertas son: "), 
	write(Open_tile1), write("  "), write(Open_tile2), nl,	
	write("¿Junto a cuál ficha abierta tiró el oponente?"),
	read(Tile), conf(Tile),
	(	Tile =:= Open_tile1; Tile =:= Open_tile2 ->
			(	legal_move(Tile) ->
					Open_tile is Tile, !; % iguala Open_tile a la ficha leída y la manda de regreso
					write("¡Jugada ilegal!"), nl, oppo_turn
			), !;
			write("No elegiste una ficha abierta en el tablero"), nl, 
			oppo_pick_open(Oppo_tile, Open_tile)
	).

% Actualiza el estado del juego
oppo_play([Oppo_tile1, Oppo_tile2], Open_tile):-
	% Actualiza cuántas fichas tiene el oponente
	game_state(num_oppo_tiles, Num_oppo_tiles),
	New_num_oppo_tiles is Num_oppo_tiles - 1,
	retract(game_state(num_oppo_tiles, Num_oppo_tiles)),
	assert(game_state(num_oppo_tiles, New_num_oppo_tiles)),
	
	% Actualiza qué fichas quedan
	game_state(rem_tiles, Rem_tiles),
	retract(game_state(rem_tiles, Rem_tiles)),
	delete(Rem_tiles, [Oppo_tile1, Oppo_tile2], New_rem_tiles),
  assert(game_state(rem_tiles, New_rem_tiles)),
	
	% Actualiza las fichas abiertas
	game_state(open_tiles, Open_tiles),
	retract(game_state(open_tiles, Open_tiles)),
	delete(Open_tiles, Open_tile, Open_tiles_short),
	(	Oppo_tile1 =:= Open_tile -> % revisa qué lado de la ficha queda abierto
			% El lado 2 queda abierto
			append(Open_tiles_short, [Oppo_tile2], New_open_tiles), !;
			% El lado 1 queda abierto
			append(Open_tiles_short, [Oppo_tile1], New_open_tiles), !
	),
  assert(game_state(open_tiles, New_open_tiles)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Interacción con el usuario   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
  ( member(Ordered_tile, Player_hand) ->
  	!, write("Ya tienes la ficha ingresada"), nl, read_ini_player_hand;
  	!
  ),
  ( not(member(Ordered_tile, Rem_tiles)) ->
  	!, write("La ficha ingresada no existe"), nl, read_ini_player_hand;
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
	write("Escribe '1.' si empezamos nosotros o '0.' si empieza el oponente"),
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
	print_list(Player_hand, "Tus fichas:").

% Imprime cualquier lista junto con un mensaje apropiado
print_list(List, Message):-
	write(Message), nl,
	print_list(List).
print_list([]):-nl.
print_list([Elem|Rest]):-
	write(Elem), write("   "),
	print_list(Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Actualización automática del tablero   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Coloca la primera ficha
first_turn:-
	write("¿Qué ficha se jugó?"), nl,
	read(Tile), conf(Tile),
	game_state(rem_tiles, Rem_tiles),
	(	not(member(Tile, Rem_tiles)) ->
			write("La ficha ingresada no existe"), nl, first_turn, !;
			!
	),
	turn(Turn),
	(	Turn =:= 0 ->
		% Empieza el oponente
		assert(game_state(open_pieces, Tile)),
		oppo_play(Tile, Tile), !;
		
		% Empieza el jugador
		!
	).
	

legal_move(Open_tile, [Player_tile1, Player_tile2]):-
	Open_tile =:= Player_tile1;
	Open_tile =:= Player_tile2.

legal_move([Player_tile1, Player_tile2]):-
	game_state(open_tiles, [Open_tile1, Open_tile2]), % TODO: warning: singleton variables
	Open_tile1 =:= Player_tile1;
	Open_tile1 =:= Player_tile2;
	Open_tile2 =:= Player_tile1;
	Open_tile2 =:= Player_tile2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Toma de decisiones   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Aquí va el árbol mamado

% Definimos el arbol de juego
% Arbol lista de nodos donde cada nodo es una lista de nodos hijos y sus puntuaciones asociadas
% El nodo raiz tiene una lista vacia de hijos y representa el estado inicial del juego
game_tree([
    [ % nodo raiz
        [[], 3],
        [[], 6],
        [[], 9]
    ],
    [ % nodos nivel 1
        [
            [[], 2],
            [[], 4]
        ],
        [
            [[], 6],
            [[], 8]
        ]
    ],
    [ % nodos nivel 2
        [
            [[], 1],
            [[], 3]
        ],
        [
            [[], 7],
            [[], 5]
        ]
    ]
]).

% Defininos el algo minimax
% input: arbol, nivel actual
% output: mejor puntuacion y movimiento asociado para el jugador actual
minimax(GameTree, Level, BestScore, BestMove) :-
    nth0(Level, GameTree, Node), % Get the current level node from the game tree
    findall(Score-Move, (
        nth0(_, Node, [ChildNode, Score]), % Get each child node and its score
        minimax(GameTree, Level+1, ChildScore, _), % Recursively evaluate the child node
        Score is -ChildScore, % Invert the child score, since we are evaluating the game from the perspective of the opponent
        Move is ChildNode % Set the move to the child node
    ), ScoreMoves), % Collect all the possible scores and moves
    (Level =:= 0 -> % If we are at the root level, choose the move with the highest score
        reverse(ScoreMoves, [_-BestMove|_]), % Sort the scores and moves in descending order and choose the first one
        BestScore is -Score % Invert the score, since it is evaluated from the opponent's perspective
    ; % If we are not at the root level, choose the move with the lowest score
        sort(ScoreMoves, [BestScore-BestMove|_]) % Sort the scores and moves in ascending order and choose the first one
    ).





% Example usage:
%?- game_tree(GameTree), minimax(GameTree, 0, BestScore, BestMove).
%BestScore = 3,
%BestMove = [].
