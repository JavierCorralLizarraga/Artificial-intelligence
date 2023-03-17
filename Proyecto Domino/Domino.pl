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
  first_turn,
  game_loop.

% Regresa verdadero si el juego no se ha acabado
%game_over:-false. % TODO: borrar

% Establece qué jugador empieza
set_starting_player:-
	write("Escribe '1.' si empezamos nosotros o '0.' si empieza el oponente"),
	read(Turn),
	assert(turn(Turn)).

% Invierte el valor de turn
% 0: oponente; 1: jugador
next_turn:-
	turn(Turn),
	New_turn is (Turn+1) mod 2,
	retract(turn(Turn)),
	assert(turn(New_turn)).

% Termina el juego y revisa quién ganó
end_game:-
	game_state(num_player_tiles, Num_player_tiles),
	game_state(num_player_tiles, Num_oppo_tiles),
	(	Num_player_tiles < Num_oppo_tiles ->
			write("Ganaste"), nl;
		Num_player_tiles > Num_oppo_tiles ->
			write("Ganó el oponente"), nl;
			write("Empataron"), nl
	),
	write("------------- Fin del juego ----------------").

% Lleva a cabo todos los turnos (excepto el primero) hasta que acaba el juego
game_loop:-
	%not(game_over),
  turn(Turn),
  ( Turn = 1 -> 
  		player_turn, !;
   		oppo_turn
  ),
  game_state(num_player_tiles, Num_player_tiles),
  game_state(num_oppo_tiles, Num_oppo_tiles),
  (
		( Num_player_tiles = 0; Num_oppo_tiles = 0 ->
		 		end_game, !
		);
		
		next_turn,
		game_loop
  ).

% Coloca la primera ficha
first_turn:-
	write("¿Qué ficha se jugó?"), nl,
	read((Open_tile_N1, Open_tile_N2)), % haremos que la ficha tirada se convierte en la abierta
	(
		(	reenter_input((Open_tile_N1, Open_tile_N2)) ->
			first_turn, !
		);
		
		game_state(rem_tiles, Rem_tiles),
		(	not(member((Open_tile_N1, Open_tile_N2), Rem_tiles)) ->
				write("La ficha ingresada no está disponible"), nl, first_turn, !
		);
		
		assert(game_state(open_pieces, [Open_tile_N1, Open_tile_N1])), % truco para poder usar place_tile
		place_tile((Open_tile_N1, Open_tile_N2), Open_tile_N1)
	).

% ----------------------- Jugador ---------------------
% Lleva a cabo un turno del jugador (excepto el primero del juego) 
player_turn:-
	nl, write("---------Turno del jugador--------"), nl,
	game_state(player_hand, Player_hand),
	include(tile_has_legal_move, Player_hand, Player_legal_tiles), % filtra qué fichas se pueden tirar
	player_choose_move(Player_legal_tiles, Player_tile, Open_tile_N),
	place_tile(Player_tile, Open_tile_N),
	write("---------Fin de turno del jugador--------"), nl, nl.

% ----------------------- Oponente ---------------------
% Lleva a cabo un turno del oponente (excepto el primero del juego)
oppo_turn:-
	nl, write("---------Turno del oponente---------"), nl,
	oppo_steal,
	oppo_pick_tile(Oppo_tile),
	(	Oppo_tile \= n ->
			oppo_pick_open(Oppo_tile, Open_tile),
			place_tile(Oppo_tile, Open_tile);
			write("El oponente pasa"), nl
	),
	write("---------Fin de turno del oponente---------"), nl, nl.

% En caso de que robe algo del pozo
oppo_steal:-
	write("¿El oponente tomó del pozo? 's.' o 'n.'"), nl,
	read(Oppo_stock),
	(
		(	reenter_input(Oppo_stock) ->
			oppo_steal, !
		);
		(	Oppo_stock =:= n ->
				! % si no tomó del pozo, regresa verdadero
		);
		
		% Si sí tomó, revisar cuántas
		write("¿Cuántas?"), nl, 
		read(Oppo_stock_num), 
		(
			(	reenter_input(Oppo_stock_num) ->
					oppo_steal, !
			);
			
			game_state(rem_pieces, Rem_pieces),
			
			(	length(Rem_pieces) < Oppo_stock_num ->
					write("No hay suficientes fichas en el pozo"), nl, oppo_steal, !
			);
			
			% Suma las fichas que tomó el oponente del pozo a su mano
			game_state(num_oppo_tiles, Num_oppo_tiles),
			New_num_oppo_tiles is Num_oppo_tiles + Oppo_stock_num,
			write("ahora tiene "), write(New_num_oppo_tiles), write("fichas"), % TODO: borrar
			retract(game_state(num_oppo_tiles, Num_oppo_tiles)),
			assert(game_state(num_oppo_tiles, New_num_oppo_tiles))
		)
	).

% Selecciona qué ficha tiró el oponente
% oppo_pick_tile(o)
oppo_pick_tile(Oppo_tile):-
	write("¿Qué tiró el oponente? Si no tuvo jugadas legales, escribe 'n.'"), nl,
	read(Tile), reenter_input(Tile),
	game_state(rem_pieces, Rem_pieces),
	( Tile \= n, not(member(Tile, Rem_pieces)) ->
			write("La ficha no está disponible"), nl, oppo_pick_tile(Oppo_tile), !;
			Oppo_tile is Tile % iguala Oppo_tile a la ficha leída y la manda de regreso a oppo_turn
	).

% Elige junto a cuál de las fichas abiertas jugó
% oppo_pick_open(i, o)
oppo_pick_open(Oppo_tile, Open_tile):-
	game_state(open_tiles, [Open_tile1, Open_tile2]),
	write("Las fichas abiertas son: "), 
	write(Open_tile1), write("  "), write(Open_tile2), nl,	
	write("¿Junto a cuál ficha abierta tiró el oponente?"),
	read(Tile), 
	(	
		(	reenter_input(Tile) ->
				oppo_pick_open(Oppo_tile, Open_tile), !
		);
		
		(	Tile =:= Open_tile1; Tile =:= Open_tile2 ->
			(	legal_move(Tile) ->
					Open_tile is Tile, !; % iguala Open_tile a la ficha leída y la manda de regreso
					write("¡Jugada ilegal!"), nl, oppo_turn, !
			);
			
			write("No elegiste una ficha abierta en el tablero"), nl, 
			oppo_pick_open(Oppo_tile, Open_tile)
		)
	).

% Actualiza el estado del juego después de colocar una ficha
% place_tile(i, i)
place_tile([Tile_N1, Tile_N2], Open_tile_N):-
	turn(Turn),
	(	Turn =:= 0 ->
		% Actualiza cuántas fichas tiene el oponente
		game_state(num_oppo_tiles, Num_oppo_tiles),
		New_num_oppo_tiles is Num_oppo_tiles - 1,
		retract(game_state(num_oppo_tiles, Num_oppo_tiles)),
		assert(game_state(num_oppo_tiles, New_num_oppo_tiles)), !;
		
		% Actualiza cuántas fichas (y cuáles) tiene el jugador
		game_state(num_player_tiles, Num_player_tiles),
		New_num_player_tiles is Num_player_tiles - 1,
		retract(game_state(num_player_tiles, Num_player_tiles)),
		assert(game_state(num_player_tiles, New_num_player_tiles)),
		game_state(player_hand, Player_hand),
		retract(game_state(player_hand, Player_hand)),
		delete(Player_hand, [Tile_N1, Tile_N2], New_player_hand),
		assert(game_state(player_hand, New_player_hand))
	),
	
	% Actualiza qué fichas quedan
	game_state(rem_tiles, Rem_tiles),
	retract(game_state(rem_tiles, Rem_tiles)),
	delete(Rem_tiles, (Tile_N1, Tile_N2), New_rem_tiles),
  assert(game_state(rem_tiles, New_rem_tiles)),
	
	% Actualiza las fichas abiertas
	game_state(open_tiles, Open_tiles),
	retract(game_state(open_tiles, Open_tiles)),
	delete(Open_tiles, Open_tile_N, Open_tiles_short),
	(	Tile_N1 =:= Open_tile_N -> % revisa qué lado de la ficha queda abierto
			% El lado 2 queda abierto
			append(Open_tiles_short, [Tile_N2], New_open_tiles), !;
			% El lado 1 queda abierto
			append(Open_tiles_short, [Tile_N1], New_open_tiles)
	),
  assert(game_state(open_tiles, New_open_tiles)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Utilidades   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Lee y guarda las fichas que tiene el jugador
read_ini_player_hand:-
 	% ------- Lectura de entrada --------
 	game_state(num_player_tiles, Num_player_tiles), % cuántas piezas llevamos
 	game_state(rem_tiles, Rem_tiles),
  game_state(player_hand, Player_hand),
	( tile_count = 0 ->
  	% Si es la primera ficha
  	write("Ingresa cada ficha con el formato 'N1, N2.' y oprime enter"), nl, !;
  	% En caso contrario
  	write("Tienes "), write(Num_player_tiles), write(" fichas en tu mano. Ingresa otra"), nl
  ),
  read(Tile),
  order_input(Tile, Ordered_tile),
  
  (
		% -------- Confirmación de la entrada ---------
		% Es falso si el usuario confirma que su entrada está bien
		( reenter_input(Ordered_tile) ->
			read_ini_player_hand, ! % si vuelve a leer la entrada, ya no revisa lo que sigue
		);
		
		% -------- Revisión de la ficha ingresada ---------
		%( member(Ordered_tile, Player_hand) ->
		%	write("Ya tienes la ficha ingresada"), nl, read_ini_player_hand, !
		%);
		( not(member(Ordered_tile, Rem_tiles)) ->
			write("La ficha ingresada no existe"), nl, read_ini_player_hand, !
		);
		
		% -------- Actualización de game_state ---------
		% Sólo llega a este punto si las condiciones anteriores fallaron
		% Actualiza las fichas disponibles
		retract(game_state(rem_tiles, Rem_tiles)),
		delete(Rem_tiles, Ordered_tile, New_rem_tiles),
		assert(game_state(rem_tiles, New_rem_tiles)),
		
		% Actualiza la mano del jugador
		retract(game_state(player_hand, Player_hand)),
		append([Ordered_tile], Player_hand, New_player_hand),
		assert(game_state(player_hand, New_player_hand)),
		
		% Actualiza el número de fichas
		retract(game_state(num_player_tiles, Num_player_tiles)),
		New_tile_count is Num_player_tiles+1,
		assert(game_state(num_player_tiles, New_tile_count)),
		
		% Se detiene cuando llegamos a 7 fichas
		(New_tile_count < 3 -> % cambiar a 7
			read_ini_player_hand;
			! % regresa verdadero sin importar el caso
		)
  ).

% Se asegura que las fichas que se ingresen tengan el número menor a la izquierda
% order_input_aux(i, o)
order_input_aux(Out, Out). % Iguala Out a la entrada
% order_input(i, 0)
order_input((N1, N2), Out):-
  ( N1 > N2 ->
    % reordena
    order_input_aux((N2, N1), Out), !; % el ! es por eficiencia
    % deja el orden igual
    order_input_aux((N1, N2), Out)
  ).
% Si no entra en los casos anteriores, la entrada es errónea;
% regresa true y deja que el método principal lo maneje
order_input(Out, Out). 

% Confirma una entrada cualquiera
% reenter_input(i)
reenter_input(Inp):-
	write("Ingresaste: "), write(Inp), 
  write("; escribe 'n.' para reintentar o 's.' para confirmar"), nl,
  read(Reenter),
  Reenter = n. % regresa true si el usuario va a reingresar la entrada

% 
print_player_hand:-
	game_state(player_hand, Player_hand),
	print_list(Player_hand, "Tus fichas:").

% legal_move(i, i)
legal_move(Open_tile, [Player_tile1, Player_tile2]):-
	Open_tile =:= Player_tile1;
	Open_tile =:= Player_tile2.

% 
% tile_has_legal_move(i)
tile_has_legal_move([Tile_N1, Tile_N2]):-
	game_state(open_tiles, [Open_tile1, Open_tile2]), % TODO: warning: singleton variables
	(	Open_tile1 =:= Tile_N1;
		Open_tile1 =:= Tile_N2;
		Open_tile2 =:= Tile_N1;
		Open_tile2 =:= Tile_N2).

% Imprime cualquier lista junto con un mensaje apropiado
% print_list(i, i)
print_list(List, Message):-
	write(Message), nl,
	print_list(List).
print_list([]):-nl.
print_list([Elem|Rest]):-
	write(Elem), write("   "),
	print_list(Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Toma de decisiones   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Escoge qué jugada hacer
% player_choose_move(i, o, o)
player_choose_move([(Tile_N1, Tile_N2)|Rest], Player_tile, Open_tile_N):-
	% Llamar al árbol
	Player_tile is Tile,
	% Escoge la primera ficha
	game_state(open_tiles, [Open_tile_N1, Open_tile_N2]),
	(	Tile_N1 =:= Open_tile_N1; Tile =:= Open_tile_N1 ->
		Open_tile_N is Open_tile_N1, !;
		Open_tile_N is Open_tile_N2
	)
	
	% Usar la heurística para decidir
	
	
	.


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
