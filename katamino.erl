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

%Entrada, N * M, Salida
principal(E, N, M, S) -> principal_aux(quitarSaltoLinea(listaColores(leerArchivo(E))), 
				colocarID(fix(quitarSaltoLinea(listaFiguras(leerArchivo(E))))),N, M, S).
principal_aux(Colores, Figuras, _N, _M, _S) -> katamino(Figuras).

%Generar katamino
katamino([]) -> [];
katamino([H|T]) -> katamino_auxi(H, T, 1).

katamino_auxi([_H|T], R, C) -> katamino_aux(T, R, C).

katamino_aux(_L, R, 9) -> katamino(R);
katamino_aux([H|_T], R, C) when C == 1 -> io:format("~p~n", [H]), katamino_aux(H, R, C + 1);
katamino_aux(T, R, C) when C rem 2 == 0 -> L = inversa(T), io:format("~p~n", [L]), katamino_aux(L, R, C + 1);
katamino_aux(T, R, C) -> L = transpuesta(T), io:format("~p~n", [L]), katamino_aux(L, R, C + 1).

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