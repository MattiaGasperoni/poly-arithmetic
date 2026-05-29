% ============================================================
% Polinomi in Prolog
% Conversione da Haskell
% Rappresentazione: lista di coefficienti in ordine crescente
% es. 3 + 2x + x^2  =>  [3.0, 2.0, 1.0]
% ============================================================


% ---------------------------------------------------------------------------
% Utility e Normalizzazione
% ---------------------------------------------------------------------------

% limite/1
% Soglia per considerare un coefficiente come zero
limite(1.0e-6).

% is_zero/1: vero se il valore assoluto è sotto la soglia
is_zero(C) :-
    limite(Lim),
    abs(C) < Lim.

% normalizza(+P, -NP)
% Haskell: normalizza :: [Double] -> [Double]
% Rimuove i coefficienti finali prossimi a zero
normalizza([], []).
normalizza(P, NP) :-
    P \= [],
    reverse(P, Rev),
    drop_leading_zeros(Rev, Trimmed),
    reverse(Trimmed, NP).

drop_leading_zeros([], []).
drop_leading_zeros([H|T], Result) :-
    ( is_zero(H)
    -> drop_leading_zeros(T, Result)
    ;  Result = [H|T]
    ).

% calcolo_grado(+P, -G)
% Haskell: calcoloGrado :: [Double] -> Int
calcolo_grado(P, G) :-
    normalizza(P, NP),
    length(NP, L),
    G is L - 1.

% converti(+X, -S)
% Haskell: converti :: Double -> String
% Stampa come intero se la parte decimale è trascurabile
converti(X, S) :-
    limite(Lim),
    R is round(X),
    ( abs(X - float(R)) < Lim
    -> ( R < 0
       -> format(atom(S), "~w", [R])
       ;  format(atom(S), "~w", [R])
       )
    ;  format(atom(S), "~w", [X])
    ).


% ---------------------------------------------------------------------------
% Visualizzazione
% ---------------------------------------------------------------------------

% stampa_polinomio(+P, -Stringa)
% Haskell: stampaPolinomio :: [Double] -> String
stampa_polinomio(P, "0") :-
    normalizza(P, []), !.
stampa_polinomio(P, Str) :-
    normalizza(P, NP),
    length(NP, L),
    MaxExp is L - 1,
    pairs_exp(NP, 0, MaxExp, Pairs),   % [(Exp, Coeff), ...]
    reverse(Pairs, RevPairs),          % ordine decrescente
    format_poly(RevPairs, true, Str).

% pairs_exp(+Coeffs, +CurExp, +MaxExp, -Pairs)
pairs_exp([], _, _, []).
pairs_exp([C|Cs], E, MaxExp, [(E,C)|Rest]) :-
    E1 is E + 1,
    pairs_exp(Cs, E1, MaxExp, Rest).

% format_poly(+[(Exp,Coeff)], +IsFirst, -Str)
format_poly([], _, "").
format_poly([(E,C)|Rest], IsFirst, Str) :-
    abs(C) =:= 1, E > 0, !,           % coefficiente ±1 con variabile
    ( IsFirst = true
    -> ( C < 0 -> Sign = "-" ; Sign = "" )
    ;  ( C > 0 -> Sign = " + " ; Sign = " - " )
    ),
    format_variable(E, Var),
    format_poly(Rest, false, RestStr),
    atom_concat(Sign, Var, Piece),
    atom_concat(Piece, RestStr, Str).
format_poly([(E,C)|Rest], IsFirst, Str) :-
    ( IsFirst = true
    -> ( C < 0 -> Sign = "-" ; Sign = "" )
    ;  ( C > 0 -> Sign = " + " ; Sign = " - " )
    ),
    AbsC is abs(C),
    converti(AbsC, CoeffStr),
    format_variable(E, Var),
    format_poly(Rest, false, RestStr),
    atom_concat(Sign, CoeffStr, T1),
    atom_concat(T1, Var, T2),
    atom_concat(T2, RestStr, Str).

% format_variable(+Exp, -Str)
format_variable(0, "")  :- !.
format_variable(1, "x") :- !.
format_variable(E, S)   :-
    format(atom(S), "x^~w", [E]).


% ---------------------------------------------------------------------------
% Aritmetica Polinomiale
% ---------------------------------------------------------------------------

% zip_with_all(+F, +Xs, +Ys, -Zs)
% Haskell: zipWithAll :: (Double->Double->Double) -> [Double] -> [Double] -> [Double]
zip_with_all(_, [], [], []).
zip_with_all(F, [X|Xs], [], [Z|Zs]) :-
    call(F, X, 0.0, Z),
    zip_with_all(F, Xs, [], Zs).
zip_with_all(F, [], [Y|Ys], [Z|Zs]) :-
    call(F, 0.0, Y, Z),
    zip_with_all(F, [], Ys, Zs).
zip_with_all(F, [X|Xs], [Y|Ys], [Z|Zs]) :-
    call(F, X, Y, Z),
    zip_with_all(F, Xs, Ys, Zs).

% helper aritmetici per call/3
add(X, Y, Z) :- Z is X + Y.
sub(X, Y, Z) :- Z is X - Y.

% addizione(+A, +B, -C)
% Haskell: addizione :: [Double] -> [Double] -> [Double]
addizione(A, B, C) :-
    zip_with_all(add, A, B, Raw),
    normalizza(Raw, C).

% sottrazione(+A, +B, -C)
% Haskell: sottrazione :: [Double] -> [Double] -> [Double]
sottrazione(A, B, C) :-
    zip_with_all(sub, A, B, Raw),
    normalizza(Raw, C).

% scala(+Scalar, +Poly, -Result)
% Moltiplica ogni coefficiente per uno scalare
scala(_, [], []).
scala(A, [B|Bs], [C|Cs]) :-
    C is A * B,
    scala(A, Bs, Cs).

% shift(+N, +Poly, -Shifted)
% Aggiunge N zeri in testa (moltiplica per x^N)
shift(0, P, P) :- !.
shift(N, P, [0.0|Shifted]) :-
    N > 0,
    N1 is N - 1,
    shift(N1, P, Shifted).

% moltiplicazione(+A, +B, -C)
% Haskell: moltiplicazione :: [Double] -> [Double] -> [Double]
moltiplicazione([], _, []).
moltiplicazione([A|As], Bs, C) :-
    scala(A, Bs, Term),
    moltiplicazione(As, Bs, RestPoly),
    shift(1, RestPoly, ShiftedRest),
    addizione(Term, ShiftedRest, C).

% divisione_con_resto(+N, +D, -Q, -R)
% Haskell: divisioneConResto :: [Double] -> [Double] -> ([Double], [Double])
divisione_con_resto(_, D, _, _) :-
    normalizza(D, []),
    throw(error(divisione_per_zero)).
divisione_con_resto(N, D, Q, R) :-
    normalizza(N, NN),
    normalizza(D, ND),
    calcolo_grado(NN, GN),
    calcolo_grado(ND, GD),
    ( GN < GD
    -> Q = [], R = NN
    ;  divisione_step(NN, ND, GN, GD, Q, R)
    ).

divisione_step(NN, ND, GN, GD, Q, R) :-
    Diff is GN - GD,
    nth0(GN, NN, LeadN),
    nth0(GD, ND, LeadD),
    QCoeff is LeadN / LeadD,
    % costruisce il monomio quoziente parziale
    length(Zeros, Diff),
    maplist(=(0.0), Zeros),
    append(Zeros, [QCoeff], QMonomio),
    moltiplicazione(QMonomio, ND, Prod),
    sottrazione(NN, Prod, RestoParziale),
    divisione_con_resto(RestoParziale, ND, QRest, RFinal),
    addizione(QMonomio, QRest, Q),
    R = RFinal.

% calcolo_mcd(+A, +B, -MCD)
% Haskell: calcoloMCD :: [Double] -> [Double] -> [Double]
calcolo_mcd(A, B, MCD) :-
    normalizza(A, NA),
    normalizza(B, NB),
    ( NB = []
    -> monico(NA, MCD)
    ;  divisione_con_resto(NA, NB, _, Resto),
       calcolo_mcd(NB, Resto, MCD)
    ).

% monico(+P, -M): normalizza a polinomio monico (coeff. direttore = 1)
monico([], []).
monico(P, M) :-
    P \= [],
    last(P, Lead),
    maplist(divide_by(Lead), P, M).

divide_by(D, X, Y) :- Y is X / D.


% ---------------------------------------------------------------------------
% Main e I/O
% ---------------------------------------------------------------------------

% leggi(+Nome, -Coeffs)
% Haskell: leggi :: String -> IO [Double]
leggi(Nome, Coeffs) :-
    format("Inserisci coeff ~w (ordine crescente): ", [Nome]),
    read_line_to_string(user_input, Line),
    ( parse_doubles(Line, Raw)
    -> normalizza(Raw, Coeffs)
    ;  writeln("Errore input!"), leggi(Nome, Coeffs)
    ).

% parse_doubles(+Stringa, -Lista)
parse_doubles(Line, Doubles) :-
    split_string(Line, " \t", " \t", Tokens),
    include([T]>>(T \= ""), Tokens, NonEmpty),
    maplist([T, D]>>(number_string(D, T)), NonEmpty, Doubles).

% main/0
main :-
    leggi("A", PA),
    leggi("B", PB),
    stampa_polinomio(PA, SA), format("~nA: ~w~n", [SA]),
    stampa_polinomio(PB, SB), format("B: ~w~n", [SB]),
    addizione(PA, PB, Somma),
    stampa_polinomio(Somma, SSomma), format("Somma: ~w~n", [SSomma]),
    moltiplicazione(PA, PB, Prodotto),
    stampa_polinomio(Prodotto, SProdotto), format("Prodotto: ~w~n", [SProdotto]),
    divisione_con_resto(PA, PB, Q, Resto),
    stampa_polinomio(Q, SQ), format("Quoziente: ~w~n", [SQ]),
    stampa_polinomio(Resto, SR), format("Resto: ~w~n", [SR]),
    calcolo_mcd(PA, PB, MCD),
    stampa_polinomio(MCD, SMCD), format("MCD: ~w~n", [SMCD]).