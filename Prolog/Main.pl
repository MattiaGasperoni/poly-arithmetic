/* =========================================================================
   Programma Prolog per operazioni ed algoritmi avanzati su polinomi.
   Pienamente conforme ai criteri DICHIARATIVI delle dispense (No ->, No if).
   ========================================================================= */

-- Predicato principale (Entry Point) per eseguire l'intero workflow di test.
main :- 
    leggi('A', PA),
    leggi('B', PB),
    nl, write('Polinomio A: '), stampa_polinomio(PA), nl,
    write('Polinomio B: '), stampa_polinomio(PB), nl,
    addizione(PA, PB, S), write('Somma: '), stampa_polinomio(S), nl,
    sottrazione(PA, PB, D), write('Differenza: '), stampa_polinomio(D), nl,
    moltiplicazione(PA, PB, P), write('Prodotto: '), stampa_polinomio(P), nl,
    divisione_con_resto(PA, PB, Q, R),
    write('Quoziente: '), stampa_polinomio(Q), nl,
    write('Resto: '), stampa_polinomio(R), nl,
    calcolo_mcd(PA, PB, M), write('MCD: '), stampa_polinomio(M), nl.

-- Costante di tolleranza per il floating point.
limite(1e-6).

-- Normalizza il polinomio eliminando gli zeri in coda (che corrispondono ai gradi massimi).
normalizza(P, P_Norm) :- reverse(P, Rev), rimuovi_zeri_iniziali(Rev, Rev_Pulito), reverse(Rev_Pulito, P_Norm).

-- Rimuove ricorsivamente gli elementi vicini allo zero dall'inizio della lista (sfrutta il Cut '!').
rimuovi_zeri_iniziali([X|XS], R) :- limite(L), abs(X) < L, !, rimuovi_zeri_iniziali(XS, R).
rimuovi_zeri_iniziali(L, L).

-- Calcola il grado del polinomio (Lunghezza della lista normalizzata - 1).
calcolo_grado([], 0) :- !.
calcolo_grado(P, G) :- normalizza(P, P_N), length(P_N, Len), Len > 0, !, G is Len - 1.
calcolo_grado(_, 0).

-- Gestore della formattazione numerica: decide se arrotondare a intero o a 4 decimali.
scrivi_valore(X) :-
    limite(L),
    Diff is abs(X - round(X)),
    scrivi_valore_scelta(Diff, L, X).

scrivi_valore_scelta(Diff, L, X) :- Diff < L, !, R is round(X), write(R).
scrivi_valore_scelta(_, _, X) :- Val is round(X * 10000) / 10000, write(Val).

% -------------------------------------------------------------------------
% Visualizzazione Algebrica Canonica (Clausole multiple separate, NO ->)
% -------------------------------------------------------------------------

-- Predicato principale per la stampa algebrica strutturata.
stampa_polinomio(P) :-
    normalizza(P, P_N),
    reverse_zip_gradi(P_N, RevZipped),
    formatta_p(RevZipped, 1).

-- Caso base 1: Polinomio nullo stampato come "0".
formatta_p([], 1) :- !, write('0').
-- Caso base 2: Termine della stampa.
formatta_p([], 0) :- !.
-- Salta la stampa se il coefficiente corrente è nullo.
formatta_p([(G, C)|XS], Primo) :-
    limite(L), abs(C) < L, !,
    formatta_p(XS, Primo).
-- Stampa il monomio corrente calcolandone segno e stringa algebrica.
formatta_p([(G, C)|XS], Primo) :-
    stampa_segno(C, Primo),
    AbsC is abs(C),
    mostra_monomio(G, AbsC),
    formatta_p(XS, 0).

-- Costruisce le coppie (Grado, Coefficiente) e le inverte (stampa dal grado massimo).
reverse_zip_gradi(P, RevZipped) :-
    costruisci_coppie(P, 0, Zipped),
    reverse(Zipped, RevZipped).

costruisci_coppie([], _, []).
costruisci_coppie([C|CS], G, [(G, C)|Resto]) :- G1 is G + 1, costruisci_coppie(CS, G1, Resto).

-- Clausole separate per la gestione puramente dichiarativa dei segni.
stampa_segno(C, 1) :- C < 0, !, write('-').
stampa_segno(_, 1) :- !.
stampa_segno(C, 0) :- C < 0, !, write(' - ').
stampa_segno(_, 0) :- write(' + ').

-- Formattazione dei singoli monomi (omissione di x^0, x^1 e dei coefficienti unitari).
mostra_monomio(0, C) :- !, scrivi_valore(C).
mostra_monomio(1, C) :- limite(L), abs(C - 1.0) < L, !, write('x').
mostra_monomio(1, C) :- !, scrivi_valore(C), write('x').
mostra_monomio(G, C) :- limite(L), abs(C - 1.0) < L, !, write('x^'), write(G).
mostra_monomio(G, C) :- scrivi_valore(C), write('x^'), write(G).

% -------------------------------------------------------------------------
% Operazioni Aritmetiche Elementari
% -------------------------------------------------------------------------
addizione([], Y, R) :- !, normalizza(Y, R).
addizione(X, [], R) :- !, normalizza(X, R).
addizione([X|XS], [Y|YS], [Z|ZS]) :- Z is X + Y, addizione(XS, YS, ZS).

sottrazione([], Y, R) :- !, mappa_negate(Y, R).
sottrazione(X, [], R) :- !, normalizza(X, R).
sottrazione([X|XS], [Y|YS], [Z|ZS]) :- Z is X - Y, sottrazione(XS, YS, ZS).

mappa_negate([], []).
mappa_negate([X|XS], [Y|YS]) :- Y is -X, mappa_negate(XS, YS).

moltiplicazione([], _, []) :- !.
moltiplicazione(_, [], []) :- !.
moltiplicazione([X|XS], YS, Prod) :-
    mul_scalare(YS, X, Mux),
    moltiplicazione(XS, YS, Resto),
    addizione(Mux, [0.0|Resto], Prod).

mul_scalare([], _, []).
mul_scalare([Y|YS], X, [Z|ZS]) :- Z is Y * X, mul_scalare(YS, X, ZS).

% -------------------------------------------------------------------------
% Divisione Euclidea (STRUTTURA CORRETTA A 5 ARGOMENTI)
% -------------------------------------------------------------------------

-- Interfaccia di scomposizione: attiva dividi_ric passando Quoziente e Resto.
divisione_con_resto(Dividendo, Divisore, Quoziente, Resto) :-
    normalizza(Dividendo, Div), normalizza(Divisore, Divis),
    dividi_ric(Div, Divis, [], Quoziente, Resto).

-- Caso base: Grado del dividendo residuo < grado del divisore. 
-- Unifica qui il quoziente accumulato e il rimanente dividendo (che è il vero Resto).
dividi_ric(Divid, Divis, Q_Acc, Quoziente, Resto) :-
    length(Divid, LD), length(Divis, LDivis),
    LD < LDivis, !,
    normalizza(Q_Acc, Quoziente),
    normalizza(Divid, Resto).

-- Passo ricorsivo: esegue la scomposizione algebrica standard.
dividi_ric(Divid, Divis, Q_Acc, Quoziente, Resto) :-
    last(Divid, CoeffDivid), last(Divis, CoeffDivis),
    length(Divid, LD), length(Divis, LDivis),
    CoeffQ is CoeffDivid / CoeffDivis,
    DiffGrado is LD - LDivis,
    costruisci_monomio(DiffGrado, CoeffQ, Monomio),
    moltiplicazione(Divis, Monomio, Sottraendo),
    sottrazione(Divid, Sottraendo, NuovoDivid),
    addizione(Q_Acc, Monomio, NuovoQAcc),
    normalizza(NuovoDivid, NuovoDividP),
    dividi_ric(NuovoDividP, Divis, NuovoQAcc, Quoziente, Resto).

-- Costruisce un monomio singolo posizionando zeri nelle posizioni di grado inferiore.
costruisci_monomio(0, Coeff, [Coeff]) :- !.
costruisci_monomio(N, Coeff, [0.0|Resto]) :- N > 0, N1 is N - 1, costruisci_monomio(N1, Coeff, Resto).

% -------------------------------------------------------------------------
% MCD (Algoritmo di Euclide, reso monico)
% -------------------------------------------------------------------------
calcolo_mcd(A, B, M) :-
    normalizza(A, NA), normalizza(B, NB),
    mcd_euclide(NA, NB, M).

-- Quando il secondo polinomio diventa [], il primo viene normalizzato a monico ed è il MCD.
mcd_euclide(A, [], M) :- !, monico(A, M).
mcd_euclide(A, B, M) :-
    divisione_con_resto(A, B, _, Resto), % Estrae il vero resto tramite la divisione corretta.
    mcd_euclide(B, Resto, M).

-- Rende monico dividendo la lista per il coefficiente di grado massimo.
monico([], []) :- !.
monico(Xs, Ms) :- last(Xs, Lead), Inv is 1.0 / Lead, mul_scalare(Xs, Inv, Ms).

% -------------------------------------------------------------------------
% Lettura Input (Senza parentesi o costrutti imperativi)
% -------------------------------------------------------------------------
leggi(Nome, P) :-
    write('Inserisci i coefficienti del polinomio '), write(Nome),
    write(' separati da spazi (ordine crescente): '), nl,
    read_line_to_string(user_input, Stringa),
    split_string(Stringa, " ", " ", ListaStringhe),
    mappa_float(ListaStringhe, Coeffs),
    normalizza(Coeffs, P).

mappa_float([], []).
mappa_float([S|SS], [F|FF]) :- S \= "", !, number_string(F, S), mappa_float(SS, FF).
mappa_float([_|SS], FF) :- mappa_float(SS, FF).