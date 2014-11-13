-module(katamino). 
-export([leerArchivo/1]). 
-export([principal/4]).
-export([katamino/1]).
-export([listaColores/1]).
-export([listaFiguras/1]).
-export([quitarSaltoLinea/1]).
-export([fix/1]).
-export([transpuesta/1]).
-export([inversa/1]).
-export([colocarID/1]).
-export([remove_duplicates/1]).
-export([colocarPieza/2]).
-export([generarMatriz/2]).

%Entrada, N * M, Salida
principal(E, N, M, S) -> principal_aux(quitarSaltoLinea(listaColores(leerArchivo(E))), 
				colocarID(fix(quitarSaltoLinea(listaFiguras(leerArchivo(E))))),N, M, S).
principal_aux(Colores, Figuras, _N, _M, _S) -> katamino(Figuras).

generarAuxMatriz(0)->[];
generarAuxMatriz(N)->[0|generarAuxMatriz(N-1)].

generarMatriz(0,_M)->[];
generarMatriz(N,M)->[generarAuxMatriz(M)|generarMatriz(N-1,M)].


%Generar katamino
katamino([]) -> [];
katamino([H|T]) -> katamino_auxi(H, T, 1).

katamino_auxi([_H|T], R, C) -> katamino_aux(T, R, C).

katamino_aux(_L, R, 9) -> katamino(R);
katamino_aux([H|_T], R, C) when C == 1 -> [H|katamino_aux(H, R, C + 1)];
katamino_aux(T, R, C) when C rem 2 == 0 -> L = transpuesta(T), [L| katamino_aux(L, R, C + 1)];
katamino_aux(T, R, C) -> L = inversa(T),[L| katamino_aux(L, R, C + 1)].


%colocar Pieza en Matriz
colocarPieza(Pieza,Matriz)->colPieza(Pieza,Matriz,[],[]).



colPieza(_,_,_,false)->false;
colPieza([],T,M,_)->M++T;
colPieza([H1|T1],[H2|T2],M,_flag)
->L=colPiezaAux(H1,H2,[]),colPieza(T1,T2,M++[L],L).

colPiezaAux([],T,F)->F++T;
colPiezaAux(_,[],_)->false;
colPiezaAux([_H1|_T1],[H2|_T2],_F)when H2 == 1 -> false;
colPiezaAux([H1|T1],[_|T2],F)->colPiezaAux(T1,T2,F++[H1]).


%Borrado de elementos repetidos al realizar los giros;
delete_all(Item, [Item | Rest_of_list]) ->
    delete_all(Item, Rest_of_list);
delete_all(Item, [Another_item| Rest_of_list]) ->
    [Another_item | delete_all(Item, Rest_of_list)];
delete_all(_, []) -> [].

remove_duplicates(List)-> removing(List,[]). 
removing([],This) -> lists:reverse(This);
removing([A|Tail],Acc) -> 
    removing(delete_all(A,Tail),[A|Acc]).

%Construir la lista de los colores
listaColores([]) -> [];
listaColores([H|T]) -> [[H]|listaColores_aux(T)].

listaColores_aux([]) -> [];
listaColores_aux([H|T]) when H == "\n" -> [lists:sublist(T, 1)|listaColores_aux(T)];
listaColores_aux([_H|T]) -> listaColores_aux(T).

%Construir la lista de las figuras
listaFiguras([]) -> [];
listaFiguras([_H|T]) -> [lists:sublist(T, 1)|listaFiguras_aux(T)].

listaFiguras_aux([]) -> [];
listaFiguras_aux([H|T]) when H == "\n" -> listaFiguras(T);
listaFiguras_aux([_H|T]) -> [lists:sublist(T, 1)|listaFiguras_aux(T)].

fix([]) -> [];
fix(L) -> fix(L, [], []).

fix([], _R, F) -> F;
fix([H|T], R, F) when H == [] -> fix(T, [], [lists:reverse(R)]++F);
fix([H|T], R, F) -> X = lists:map(fun(X) -> {Int, _} = string:to_integer(X), Int end, string:tokens(H, " ")), fix(T, [X|R], F).

%Colocar identificador a la pieza para saber sus vecionos
colocarID([]) -> [];
colocarID(M) -> colocarID(M, 1).

colocarID([], _Cont) -> [];
colocarID([H|T], Cont) -> X = [Cont] ++ [H], [X|colocarID(T, Cont +1)].

%Quitar \n
quitarSaltoLinea([]) -> [];
quitarSaltoLinea([H|T]) -> [re:replace(H, "\n", "", [global,{return,list}])|quitarSaltoLinea(T)].

%Leer del txt
leerArchivo(Archivo) ->
    {ok, D} = file:open(Archivo, [read]),
    get_all_lines(D, []).
 
get_all_lines(D, Accum) ->
    case io:get_line(D, "") of
        eof  -> file:close(D), Accum;
        Linea -> get_all_lines(D, Accum ++ [Linea])
    end.

%Transpuesta, hd = head, tl = tail
transpuesta([[]|_]) -> [];
transpuesta(M) -> [lists:map(fun hd/1, M) | transpuesta(lists:map(fun tl/1, M))].

%Inversa de la matriz (reflejo)
inversa([]) -> [];
inversa(M)  -> lists:reverse(M).
