%the lookup function

lookup([],X,Y) :- fail.
lookup([(X,Y)|XS],X,Y) :- !.
lookup([X|XS],Y,Z) :- lookup(XS,Y,Z).

%------------------------------------------
%the has type_nth function: N is integer

type_nth([H|_],0,H) :- !.
type_nth([_|HS],N,H) :- N > 0, N1 is N-1, type_nth(HS,N1,H).

%------------------------------------------
%hastype facts

hastype(Gamma,true,boolT).
hastype(Gamma,true,boolT).
hastype(Gamma,const(Integer),intT).
hastype(Gamma, tuple([]), cartesianT([])).

%-------------------------------------------
%typeElaborates rules : here typeElaborates acts as a wrapper function for elaborates
typeElaborates(Gamma,D,Gamma2) :- elaborates(Gamma,Gamma,D,Gamma2).

%----------------------------------------------
%elaborates : for different types of definitions
elaborates(Gamma,Gamma1,let(X,E),[(X,T)|Gamma]) :- hastype(Gamma1,E,T);

%note the key difference in the 2nd evaluate of sequnetial vs parallel definitions

elaborates(Gamma,Gamma1,sequential(D1,D2),Gamma2) :- elaborates(Gamma,Gamma1,D1,GammaTemp),elaborates(GammaTemp,GammaTemp,D2,Gamma2).
elaborates(Gamma,Gamma1,parallel(D1,D2),Gamma2) :- elaborates(Gamma,Gamma1,D1,GammaTemp),elaborates(GammaTemp,Gamma1,D2,Gamma2).

%in parallel def, 2nd def is type-checked w.r.t origninal table
%in sequential call, 2nd def is type-checked w.r.t updated table

elaborates(Gamma,Gamma1,letloc(D1,D2),Gamma2) :- elaborates(Gamma,Gamma1,D1,GammaTemp) , elaborates (Gamma, GammaTemp, D2, Gamma2).

%---------------------------------------------
%hastype rules

hastype(Gamma,id(V),Y) :- lookup(Gamma,V,Y).

hastype(Gamma,abs(X),intT) :- hastype(Gamma,X,intT).
hastype(Gamma,sum(X,Y),intT) :- hastype(Gamma,X,intT) , hastype(Gamma,Y,intT).
hastype(Gamma,diff(X,Y),intT) :- hastype(Gamma,X,intT) , hastype(Gamma,Y,intT).
hastype(Gamma,prod(X,Y),intT) :- hastype(Gamma,X,intT) , hastype(Gamma,Y,intT).
hastype(Gamma,div(X,Y),intT) :- hastype(Gamma,X,intT) , hastype(Gamma,Y,intT).
hastype(Gamma,mod(X,Y),intT) :- hastype(Gamma,X,intT) , hastype(Gamma,Y,intT).

hastype(Gamma,gt(X,Y),boolT) :- hastype(Gamma,X,intT) , hastype(Gamma,Y,intT).
hastype(Gamma,ge(X,Y),boolT) :- hastype(Gamma,X,intT) , hastype(Gamma,Y,intT).
hastype(Gamma,lt(X,Y),boolT) :- hastype(Gamma,X,intT) , hastype(Gamma,Y,intT).
hastype(Gamma,le(X,Y),boolT) :- hastype(Gamma,X,intT) , hastype(Gamma,Y,intT).

hastype(Gamma,and(X,Y),boolT) :- hastype(Gamma,X,boolT) , hastype(Gamma,Y,boolT).
hastype(Gamma,or(X,Y),boolT) :- hastype(Gamma,X,boolT) , hastype(Gamma,Y,boolT).
hastype(Gamma,imply(X,Y),boolT) :- hastype(Gamma,X,boolT) , hastype(Gamma,Y,boolT).
hastype(Gamma,not(X),boolT) :- hastype(Gamma,X,boolT).

hastype(Gamma, tuple([X|XS]), cartesianT([T|TS])) :- hastype(Gamma,X,T) , hastype(Gamma, tuple(XS), cartesianT(TS)).
hastype(Gamma, proj(N,Tup), T) :- hastype(Gamma, Tup, cartesianT(X)) , type_nth(X,N,T).
%type_nth takes N and X as input and checks if the type comes out to be T, N has to be an integer

hastype(Gamma, if(X,Y,Z), T) :- hastype(Gamma,X,boolT), hastype(Gamma,Y,T), hastype(Gamma,Z,T).
hastype(Gamma, let(D,E), T) :- typeElaborates(Gamma,D,Gamma2) , hastype(Gamma2,E,T).

hastype(Gamma,lambda(X,E),arrowT(T1,T2)) :- hastype([(X,T1)|Gamma],E,T2).
hastype(Gamma,call(E1,E2),T) :- hastype(Gamma,E1,arrowT(P,T)) , hastype(Gamma,E2,P).

