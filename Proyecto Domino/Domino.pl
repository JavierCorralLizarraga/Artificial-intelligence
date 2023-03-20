%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Estado de juego   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% main inicializa el estado del juego
% 
% game_state guarda todo lo que se conoce acerca del estado actual: fichas que no se han jugado, número de fichas por jugador, 
% número de fichas en el pozo, fichas que tenemos nosotros y las dos fichas abiertas en el tablero
:- dynamic game_state/1, game_state/2, turn/1, next_turn/0.
main:-
  % Guarda las fichas del oponente y el pozo
  assert(game_state(rem_tiles, [(0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (1, 1), 
                                (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (2, 2), (2, 3), (2, 4), 
                                (2, 5), (2, 6), (3, 3), (3, 4), (3, 5), (3, 6), (4, 4), (4, 5), 
                                (4, 6), (5, 5), (5, 6), (6, 6)])),
  assert(game_state(num_player_tiles, 0)), % inicia en 0 para contarlas después
  assert(game_state(num_oppo_tiles, 7)),
  assert(game_state(player_hand, [])),
  assert(game_state(open_tiles, [])), % las dos fichas junto a las que se puede colocar una nueva
  assert(game_state(stock_size, 21)), % fichas en el pozo (inicialmente incluye las del jugador)
  assert((game_state(oppo_passed):- fail, !)), % true si el oponente pasó el turno pasado
  assert((game_state(player_passed):- fail, !)), % true si el jugador pasó el turno pasado
  read_ini_player_hand,
  set_starting_player,
  first_turn,
  game_loop.

% ------------------------- Flujo del juego -------------------------
% Lee y guarda las fichas que tiene el jugador
read_ini_player_hand:-
 	game_state(num_player_tiles, Num_player_tiles), % cuántas fichas llevamos
	( Num_player_tiles = 0 ->
  	% Si es la primera ficha
  	writeln("Ingresa cada ficha con el formato 'N1, N2.' y oprime enter"), !;
  	% En caso contrario
  	write("Tienes "), write(Num_player_tiles), write(" fichas en tu mano. Ingresa otra"), nl
  ),
  read(Tile),
  order_input(Tile, Ordered_tile),
  
  (
		% Confirma la entrada
		reenter_input(Ordered_tile, read_ini_player_hand);
		% Revisa la ficha ingresada
		tile_in_stock_or_oppo(Ordered_tile, read_ini_player_hand);
		
		% Actualiza el estado del juego
		% Sólo llega a este punto si la entrada es correcta
		add_player_tile(Ordered_tile),
		
		% Se detiene cuando llegamos a 7 fichas
		(Num_player_tiles+1 < 7 ->
			read_ini_player_hand;
			! % regresa verdadero una vez que acaba
		)
  ).

% Establece qué jugador empieza
set_starting_player:-
	writeln("Escribe '1.' si empezamos nosotros o '0.' si empieza el oponente"),
	read(Turn),
	assert(turn(Turn)).

% Coloca la primera ficha
first_turn:-
	write("¿Qué ficha se jugó?"), nl,
	read((Open_tile_N1, Open_tile_N2)), % haremos que la ficha tirada se convierte en la abierta
	order_input((Open_tile_N1, Open_tile_N2), (Ordered_open_N1, Ordered_open_N2)),
	(
		reenter_input((Ordered_open_N1, Ordered_open_N2), first_turn);
		tile_exists((Ordered_open_N1, Ordered_open_N2), first_turn);
		
		retract(game_state(open_tiles, [])),
		assert(game_state(open_tiles, [Ordered_open_N1, Ordered_open_N1])), % truco para usar place_tile
		place_tile((Ordered_open_N1, Ordered_open_N2), Ordered_open_N1)
	).

% Lleva a cabo todos los turnos (excepto el primero) hasta que acaba el juego
game_loop:-
	print_game_state,
  next_turn,
  turn(Turn),
  ( Turn = 1 ->
  		% Decide a quién le toca
  		player_turn, !;
   		oppo_turn
  ),
  game_state(num_player_tiles, Num_player_tiles),
  game_state(num_oppo_tiles, Num_oppo_tiles),
  (	(Num_player_tiles = 0; Num_oppo_tiles = 0; (game_state(oppo_passed), game_state(player_passed))) ->
			% El juego acaba si alguno no tiene fichas o los dos pasaron de forma consecutiva
			end_game, !;
			game_loop
  ).

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
	game_state(num_oppo_tiles, Num_oppo_tiles),
	(	Num_player_tiles = 0 ->
			writeln("Ganaste");
		Num_oppo_tiles = 0 ->
			writeln("Ganó el oponente");
			writeln("Empataron")
	),
	write("------------- Fin del juego ----------------").

% ------------------------- Jugador -------------------------
% Lleva a cabo un turno del jugador (excepto el primero del juego) 
player_turn:-
	nl, write("---------Turno del jugador--------"), nl,
	game_state(player_hand, Player_hand),
	include(tile_has_legal_move, Player_hand, Player_legal_tiles), % filtra qué fichas se pueden tirar
	length(Player_legal_tiles, Num_legal_tiles),
	game_state(stock_size, Stock_size),
	(	Num_legal_tiles = 0 ->
			(	Stock_size > 0 ->
					% Si quedan fichas, roba y vuelve a intentar
					player_steal, player_turn, !;
					% En caso contrario, pierde el turno
					write("El jugador pasa"), nl,
					% Actualiza si el jugador pasó o no
					(	not(game_state(player_passed)) -> 
							player_pass_switch; !
					), !
			);
			% Actualiza si el jugador pasó o no
			(	game_state(player_passed) -> 
					player_pass_switch
			),
			% Escoge una jugada (si tiene jugadas legales)
			player_choose_move(Player_legal_tiles, Player_tile, Open_tile_N),
			place_tile(Player_tile, Open_tile_N),
			write("Coloca "), write(Player_tile), write(" junto a "), write(Open_tile_N), nl
	),
	% TODO: esto se imprime varias veces si robamos
	write("---------Fin de turno del jugador--------"), nl, nl.

% 
player_pass_switch:-
	(	game_state(player_passed) -> 
			retract( (game_state(player_passed):- true) ),
			assert( (game_state(player_passed):- fail, !) ), !;
			
			retract( (game_state(player_passed):- fail, !) ),
			assert( (game_state(player_passed):- true) )
	).

% Roba del pozo si no tiene jugadas legales
player_steal:-
	writeln("Roba una ficha. ¿Cuál salió?"),
	read(Tile),
	order_input(Tile, Ordered_tile),
	(
		% Revisa la entrada
		reenter_input(Ordered_tile, player_steal);
		tile_in_stock_or_oppo(Ordered_tile, player_steal);
		
		% Agrega la ficha que tomó el jugador del pozo a su mano
		add_player_tile(Ordered_tile)
	).

% ------------------------- Oponente -------------------------
% Lleva a cabo un turno del oponente (excepto el primero del juego)
oppo_turn:-
	nl, write("---------Turno del oponente---------"), nl,
	oppo_steal,
	oppo_pick_tile(Oppo_tile),
	(	Oppo_tile \= n ->
			% Si no pasó
			oppo_pick_open(Oppo_tile, Open_tile),
			place_tile(Oppo_tile, Open_tile),
			% Actualiza si el oponente pasó o no
			(	game_state(oppo_passed) -> 
					oppo_pass_switch; !
			), !;
			
			% Si sí pasó
			write("El oponente pasa"), nl,
			% Actualiza si el oponente pasó o no
			(	not(game_state(oppo_passed)) -> 
					oppo_pass_switch; !
			)
	),
	write("---------Fin de turno del oponente---------"), nl, nl.

% En caso de que robe fichas del pozo
oppo_steal:-
	game_state(stock_size, Stock_size),
	game_state(num_oppo_tiles, Num_oppo_tiles),
	write("¿El oponente robó del pozo? 's.' o 'n.'"), nl,
	read(Oppo_stock),
	(
		reenter_input(Oppo_stock, oppo_steal);
		
		(	Oppo_stock = n ->
				! % si no tomó del pozo, regresa verdadero
		);
		
		% Si sí tomó, revisa cuántas
		write("¿Cuántas?"), nl, 
		read(Oppo_stock_num), 
		(
			reenter_input(Oppo_stock_num, oppo_steal);
			
			(	Stock_size < Oppo_stock_num ->
					write("No hay suficientes fichas en el pozo"), nl, oppo_steal, !
			);
			
			% Suma las fichas que tomó el oponente del pozo a su mano
			New_num_oppo_tiles is Num_oppo_tiles + Oppo_stock_num,
			retract(game_state(num_oppo_tiles, Num_oppo_tiles)),
			assert(game_state(num_oppo_tiles, New_num_oppo_tiles)),
			% Resta las fichas que tomó el oponente del pozo
			New_stock_size is Stock_size - Oppo_stock_num,
			retract(game_state(stock_size, Stock_size)),
			assert(game_state(stock_size, New_stock_size))
		)
	).

% Selecciona qué ficha tiró el oponente
% oppo_pick_tile(o)
oppo_pick_tile(Oppo_tile):-
	game_state(rem_tiles, Rem_tiles),
	write("¿Qué tiró el oponente? Si no tuvo jugadas legales, escribe 'n.'"), nl,
	read(Tile),
	order_input(Tile, Ordered_tile),
	(
		reenter_input(Ordered_tile, oppo_pick_tile(Oppo_tile));
		
		( Ordered_tile \= n, not(member(Ordered_tile, Rem_tiles)) ->
				write("La ficha no está disponible"), nl, oppo_pick_tile(Oppo_tile), !
		);
		Oppo_tile = Ordered_tile % iguala Oppo_tile a la ficha leída y la manda de regreso a oppo_turn
	).

% Elige junto a cuál de las fichas abiertas jugó
% oppo_pick_open(i, o)
oppo_pick_open(Oppo_tile, Open_tile_N):-
	game_state(open_tiles, [Open_tile_N1, Open_tile_N2]),
	(	(not(legal_move(Oppo_tile, Open_tile_N1)); Open_tile_N1 =:= Open_tile_N2) ->
			% Si sólo hay una jugada legal
			Open_tile_N = Open_tile_N2, !;
		not(legal_move(Oppo_tile, Open_tile_N2)) ->
			% Si sólo hay una jugada legal
			Open_tile_N = Open_tile_N1, !;
			
			% Si puede jugar en ambos lados
			write("Las fichas abiertas son: "), 
			write(Open_tile_N1), write("  "), write(Open_tile_N2), nl,	
			write("¿Junto a cuál ficha abierta tiró el oponente?"),
			read(Open),
			(
				reenter_input(Open, oppo_pick_open(Oppo_tile, Open_tile_N));
				
				(	(Open = Open_tile_N1; Open = Open_tile_N2) ->
						Open_tile_N = Open, !; % iguala Open_tile_N a la ficha leída y la manda de regreso
						writeln("No elegiste una ficha abierta en el tablero"),
						oppo_pick_open(Oppo_tile, Open_tile_N)
				)
			)
	).

% Cambia el estado de oppo_passed
oppo_pass_switch:-
	(	game_state(oppo_passed) -> 
			retract( (game_state(oppo_passed):- true) ),
			assert( (game_state(oppo_passed):- fail, !) ), !;
	
			retract( (game_state(oppo_passed):- fail, !) ),
			assert( (game_state(oppo_passed):- true) )
	).

% Actualiza el estado del juego después de colocar una ficha
% place_tile(i, i)
place_tile((Tile_N1, Tile_N2), Open_tile_N):-
	turn(Turn),
	(	Turn = 0 ->
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
		delete(Player_hand, (Tile_N1, Tile_N2), New_player_hand),
		retract(game_state(player_hand, Player_hand)),
		assert(game_state(player_hand, New_player_hand))
	),
	
	% Actualiza qué fichas quedan
	game_state(rem_tiles, Rem_tiles),
	delete(Rem_tiles, (Tile_N1, Tile_N2), New_rem_tiles),
	retract(game_state(rem_tiles, Rem_tiles)),
  assert(game_state(rem_tiles, New_rem_tiles)),
	
	% Actualiza las fichas abiertas
	game_state(open_tiles, [Open_tile_N1, Open_tile_N2]),
	retract(game_state(open_tiles, [Open_tile_N1, Open_tile_N2])),
	%delete(Open_tiles, Open_tile_N, Open_tiles_short),
	
	(	Open_tile_N1 = Open_tile_N ->
		append([], [Open_tile_N2], Open_tiles_short), !; % Open_tiles_short = [Open_tile_N2]
		append([], [Open_tile_N1], Open_tiles_short)     % Open_tiles_short = [Open_tile_N1]
	),
	
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
% Se asegura de que las fichas que se ingresen tengan el número menor a la izquierda
% order_input_aux(i, o)
order_input_aux(Out, Out). % Iguala Out a la entrada
% order_input(i, o)
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

% Permite reingresar una entrada si se cometió un error
% reenter_input_(i, i)
reenter_input(Inp, Method):-
	write("Ingresaste: "), write(Inp), 
  write("; escribe 'n.' para reintentar o 's.' para confirmar"), nl,
  read(Reenter),
  (	Reenter = n ->
  		% Vuelve a llamar al método si ingrsó 'n'; si no, regresa false
  		Method, !
  ).

% Quita una ficha del pool disponible
% remove_tile(i)
remove_tile(Tile):-
	game_state(rem_tiles, Rem_tiles),
	delete(Rem_tiles, Tile, New_rem_tiles),
	retract(game_state(rem_tiles, Rem_tiles)),
	assert(game_state(rem_tiles, New_rem_tiles)).

% Agrega una ficha a la mano del jugador
% add_player_tile(i)
add_player_tile(Tile):-
 	game_state(rem_tiles, Rem_tiles),
 	game_state(stock_size, Stock_size),
  game_state(player_hand, Player_hand),
  game_state(num_player_tiles, Num_player_tiles),
	% Actualiza las fichas disponibles
	delete(Rem_tiles, Tile, New_rem_tiles),
	retract(game_state(rem_tiles, Rem_tiles)),
	assert(game_state(rem_tiles, New_rem_tiles)),
	% Actualiza el número de fichas en el pozo
	New_stock_size is Stock_size-1,
	retract(game_state(stock_size, Stock_size)),
	assert(game_state(stock_size, New_stock_size)),
	% Actualiza la mano del jugador
	append([Tile], Player_hand, New_player_hand),
	retract(game_state(player_hand, Player_hand)),
	assert(game_state(player_hand, New_player_hand)),
	% Actualiza el número de fichas del jugador
	New_num_player_tiles is Num_player_tiles+1,
	retract(game_state(num_player_tiles, Num_player_tiles)),
	assert(game_state(num_player_tiles, New_num_player_tiles)).

% Si una ficha no está en el pozo ni la mano del oponente, vuelve a correr la cláusula que lo llamó;
% en caso contrario, regresa false
% tile_in_stock_or_oppo(i, i)
tile_in_stock_or_oppo(Tile, Method):-
	game_state(rem_tiles, Rem_tiles),
	(	not(member(Tile, Rem_tiles)) ->
			writeln("La ficha ingresada no está disponible"), Method, !
	).

% Si una ficha ya se jugó o no existe, vuelve a correr la cláusula que lo llamó;
% en caso contrario, regresa false
% tile_exists(i, i)
tile_exists(Tile, Method):-
	game_state(rem_tiles, Rem_tiles),
	game_state(player_hand, Player_hand),
	(	(not(member(Tile, Rem_tiles)), not(member(Tile, Player_hand))) ->
			writeln("La ficha ingresada no está disponible"), Method, !
	).

% Regresa true si la ficha puede colocarse junto a la ficha abierta dada
% legal_move(i, i)
legal_move((Tile_N1, Tile_N2), Open_tile_N):-
	Open_tile_N =:= Tile_N1;
	Open_tile_N =:= Tile_N2.

% Regresa true si la ficha dada puede colocarse en algún lado del tablero
% tile_has_legal_move(i)
tile_has_legal_move((Tile_N1, Tile_N2)):-
	game_state(open_tiles, [Open_tile_N1, Open_tile_N2]),
	(	Open_tile_N1 =:= Tile_N1;
		Open_tile_N1 =:= Tile_N2;
		Open_tile_N2 =:= Tile_N1;
		Open_tile_N2 =:= Tile_N2).

% Imprime toda la información sobre el estado del juego
print_game_state:-
	nl, write("---Estado del juego---"), nl,
	game_state(player_hand, Player_hand),
	include(tile_has_legal_move, Player_hand, Player_legal_tiles),
	game_state(num_player_tiles, Num_player_tiles),
	game_state(num_oppo_tiles, Num_oppo_tiles),
	game_state(open_tiles, Open_tiles),
	game_state(stock_size, Stock_size),
	game_state(rem_tiles, Rem_tiles),
	
	print_player_hand,
	write("Fichas legales: "), write(Player_legal_tiles), nl,
	write("El jugador tiene "), write(Num_player_tiles), write(" fichas"), nl,
	write("El oponente tiene "), write(Num_oppo_tiles), write(" fichas"), nl,
	write("Fichas abiertas: "), write(Open_tiles), nl,
	write("Hay "), write(Stock_size), write(" fichas en el pozo"), nl,
	
	write("El jugador pasó: "),
	(	game_state(player_passed) ->
			write("Sí"), nl; write("No"), nl
	),
	write("El oponente pasó: "),
	(	game_state(oppo_passed) ->
			write("Sí"), nl; write("No"), nl
	),
	
	write("Fichas del oponente/pozo: "), write(Rem_tiles), nl,
	write("---Fin de estado del juego---"), nl.

% Imprime la mano del jugador
print_player_hand:-
	game_state(player_hand, Player_hand),
	print_list(Player_hand, "Tus fichas:").

% Imprime cualquier lista junto con un mensaje apropiado
% print_list(i, i)
print_list(List, Message):-
	write(Message), nl,
	print_list(List), nl.
print_list([]):-nl.
print_list([Elem|Rest]):-
	write(Elem), write("   "),
	print_list(Rest).


% TODO: borrar
% Revisa si el jugador tiene algún movimiento legal
player_has_legal_move:-
	game_state(player_hand, Player_hand),
	include(tile_has_legal_move, Player_hand, Player_legal_tiles), % filtra qué fichas se pueden tirar
	length(Player_legal_tiles, Num_player_legal_tiles),
	Num_player_legal_tiles = 0. % regresa true si no hay jugadas legales

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Toma de decisiones   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Escoge qué jugada hacer
% player_choose_move(i, o, o)
player_choose_move([(Tile_N1, Tile_N2)|Rest], Player_tile, Open_tile_N):-
	% Llamar al árbol
	
	% Escoge la primera ficha
	Player_tile = (Tile_N1, Tile_N2),
	
	% Escoge de qué lado tirarla
	game_state(open_tiles, [Open_tile_N1, Open_tile_N2]),
	(	Tile_N1 = Open_tile_N1; Tile_N2 = Open_tile_N1 ->
		Open_tile_N = Open_tile_N1, !;
		Open_tile_N = Open_tile_N2
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
