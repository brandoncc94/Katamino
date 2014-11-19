-module(katamino). 
-export([leerArchivo/1]). 
-export([principal/4]).
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
-export([permutaciones/1]).
-export([generarLisN/1]).
-export([combinacionesPieza/1]).
-export([combinarPiezas/1]).


%Entrada, N * M, Salida

katamino(PermFig,Matriz,Figuras,Colores,CFiguras,LCFig,Sal)->LCFig.

principal(E, N, M, S) -> principal_aux(quitarSaltoLinea(listaColores(leerArchivo(E))),
						lists:reverse(fix(quitarSaltoLinea(listaFiguras(leerArchivo(E))))),N, M, S).
principal_aux(Colores, Figuras, N, M, S)->CFiguras=combinarPiezas(Figuras),LCFig=[length(F)||F<-CFiguras],
		katamino(permutaciones(generarLisN(Figuras)),generarMatriz(N,M),Figuras,Colores,CFiguras,LCFig,S).


generarAuxMatriz(0)->[];
generarAuxMatriz(N)->[0|generarAuxMatriz(N-1)].

generarMatriz(0,_M)->[];
generarMatriz(N,M)->[generarAuxMatriz(M)|generarMatriz(N-1,M)].


generarLisN([])->[];
generarLisN(L)->generarLisN(L,1).

generarLisN([],_)->[];
generarLisN([H|T],N)->[N]++generarLisN(T,N+1).


combinarPiezas([])->[];
combinarPiezas([H|T])->[combinacionesPieza(H)|combinarPiezas(T)].

combinacionesPieza(P)->remove_duplicates(combPiezas(P,1)).

combPiezas(_L, 9) ->[];
combPiezas(L, C) when C == 1 -> [L|combPiezas(L, C + 1)];
combPiezas(L, C) when C rem 2 == 0 -> L1 = transpuesta(L), [L1| combPiezas(L1,C + 1)];
combPiezas(L, C) -> L1 = inversa(L),[L1| combPiezas(L1, C + 1)].




%remove_duplicates(List)

%Generar katamino
%katamino([]) -> [];
%katamino([H|T]) -> katamino_auxi(H, T, 1).

%katamino_auxi([_H|T], R, C) -> katamino_aux(T, R, C).

%katamino_aux(_L, R, 9) -> katamino(R);
%katamino_aux([H|_T], R, C) when C == 1 -> [H|katamino_aux(H, R, C + 1)];
%katamino_aux(T, R, C) when C rem 2 == 0 -> L = transpuesta(T), [L| katamino_aux(L, R, C + 1)];
%katamino_aux(T, R, C) -> L = inversa(T),[L| katamino_aux(L, R, C + 1)].


%colocar Pieza en Matriz

colocarPieza(Pieza,[HM|TM])-> LC= length(HM),LF = length([HM|TM]),
				colocarPieza(Pieza,[HM|TM],0,0,LF-1,LC-1,false).

colocarPieza(_Pieza,_Matriz,X1,_Y1,X2,_Y2,false) when X1 > X2 ->false;
colocarPieza(Pieza,Matriz,X1,Y1,X2,Y2,false)when Y1 > Y2 -> colocarPieza(Pieza,Matriz,X1+1,0,X2,Y2,false);
colocarPieza(Pieza,Matriz,X1,Y1,X2,Y2,false)-> NM = colocarPieza(Pieza,Matriz,X1,Y1,[]),
						    colocarPieza(Pieza,Matriz,X1,Y1+1,X2,Y2,NM);
colocarPieza(_,_,_,_,_,_,NM)->NM.

colocarPieza(Pieza,Matriz,0,Y,NM)->colPieza(Pieza,Matriz,NM,[],Y);
colocarPieza(Pieza,[HM|TM],X,Y,NM)->colocarPieza(Pieza,TM,X-1,Y,NM++[HM]).

%katamino:colocarPieza([[1,1]],[],1,0,[[1,1,0],[0,0,0]])
%katamino:colPieza([[1],[1]],[],[[1,1,0],[1,1,0]],[],0)

colPieza(_P,_M,_NM,false,_)->false;
%colPieza(P,[],_NM,_,_)when not(P==[]) ->false;
colPieza([],T,M,_F,_Y)->M++T;
colPieza([H1|T1],[H2|T2],M,_Flag,Y)
->L=colPiezaAux(H1,H2,[],Y),colPieza(T1,T2,M++[L],L,Y).

colPiezaAux([],T,F,0)->F++T;
colPiezaAux(_P,[],_F,_N)->false;
colPiezaAux([_H1|_T1],[H2|_T2],_F,0)when H2 == 1 -> false;
colPiezaAux([H1|T1],[H2|T2],F,0)when H1 == 0 ->colPiezaAux(T1,T2,F++[H2],0);
colPiezaAux([H1|T1],[_H2|T2],F,0)->colPiezaAux(T1,T2,F++[H1],0);
colPiezaAux(P,[H|T],F,N)->colPiezaAux(P,T,F++[H],N-1).


%Generacion de Posibles combinaciones de las figuras
permutaciones([]) -> [[]];
permutaciones(L) -> [ [H|T] || H <- L, T <- permutaciones(L--[H]) ].


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

%Construir la lista de los 

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
