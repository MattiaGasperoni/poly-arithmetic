/* =========================================================================
   Programma Prolog per operazioni ed algoritmi avanzati su polinomi.
   Pienamente conforme ai criteri DICHIARATIVI delle dispense (No ->, No if).
   ========================================================================= */
 
% Predicato principale (Entry Point) per eseguire l'intero workflow di test.
main :- 
    leggiPolinomio('A', PoliA),
    leggiPolinomio('B', PoliB),
    nl, write('Polinomio A: '), mostraPolinomio(PoliA), nl,
    write('Polinomio B: '), mostraPolinomio(PoliB), nl,
    sommaPolinomi(PoliA, PoliB, Somma), write('Somma: '), mostraPolinomio(Somma), nl,
    sottraiPolinomi(PoliA, PoliB, Differenza), write('Differenza: '), mostraPolinomio(Differenza), nl,
    moltiplPolinomi(PoliA, PoliB, Prodotto), write('Prodotto: '), mostraPolinomio(Prodotto), nl,
    divisioneConResto(PoliA, PoliB, Quoziente, Resto),
    write('Quoziente: '), mostraPolinomio(Quoziente), nl,
    write('Resto: '), mostraPolinomio(Resto), nl,
    calcolaMCD(PoliA, PoliB, MCD), write('MCD: '), mostraPolinomio(MCD), nl.
 
% Costante di tolleranza per confronti in virgola mobile.
tolleranzaNumerica(1e-6).
 
% Normalizza il polinomio eliminando i coefficienti nulli dal grado massimo.
rimuoviZeriInTesta(Polinomio, PolinomioNorm) :-
    reverse(Polinomio, Invertito),
    rimuoviZeriDaListaInvertita(Invertito, InvertitoRipulito),
    reverse(InvertitoRipulito, PolinomioNorm).
 
% Rimuove ricorsivamente i valori vicini allo zero dall'inizio della lista.
rimuoviZeriDaListaInvertita([X|Resto], Risultato) :-
    tolleranzaNumerica(T), abs(X) < T, !,
    rimuoviZeriDaListaInvertita(Resto, Risultato).
rimuoviZeriDaListaInvertita(Lista, Lista).
 
% Calcola il grado del polinomio (lunghezza della lista normalizzata meno 1).
gradoPolinomio([], 0) :- !.
gradoPolinomio(Polinomio, Grado) :-
    rimuoviZeriInTesta(Polinomio, PoliNorm),
    length(PoliNorm, Lunghezza),
    Lunghezza > 0, !,
    Grado is Lunghezza - 1.
gradoPolinomio(_, 0).
 
% Gestisce la formattazione numerica: intero se vicino a un intero, altrimenti 4 decimali.
scriviValore(X) :-
    tolleranzaNumerica(T),
    Diff is abs(X - round(X)),
    scriviValoreConPrecisione(Diff, T, X).
 
scriviValoreConPrecisione(Diff, T, X) :- Diff < T, !, R is round(X), write(R).
scriviValoreConPrecisione(_, _, X) :- Val is round(X * 10000) / 10000, write(Val).
 
% -------------------------------------------------------------------------
% Visualizzazione Algebrica Canonica
% -------------------------------------------------------------------------
 
% Predicato principale per la stampa algebrica strutturata.
mostraPolinomio(Polinomio) :-
    rimuoviZeriInTesta(Polinomio, PoliNorm),
    costruisciCoppieGradoCoeff(PoliNorm, CoppieInvertite),
    formattaTermini(CoppieInvertite, 1).
 
% Caso base 1: Polinomio nullo, stampa "0".
formattaTermini([], 1) :- !, write('0').
% Caso base 2: Fine della stampa.
formattaTermini([], 0) :- !.
% Salta i monomi con coefficiente nullo.
formattaTermini([(_, Coefficiente)|Resto], EPrimoTermine) :-
    tolleranzaNumerica(T), abs(Coefficiente) < T, !,
    formattaTermini(Resto, EPrimoTermine).
% Stampa il monomio corrente con segno e forma algebrica.
formattaTermini([(Grado, Coefficiente)|Resto], EPrimoTermine) :-
    formattaSegno(Coefficiente, EPrimoTermine),
    ValoreAssoluto is abs(Coefficiente),
    formattaMonomio(Grado, ValoreAssoluto),
    formattaTermini(Resto, 0).
 
% Costruisce le coppie (Grado, Coefficiente) partendo dal grado massimo.
costruisciCoppieGradoCoeff(Polinomio, CoppieInvertite) :-
    accumulaCoppie(Polinomio, 0, Coppie),
    reverse(Coppie, CoppieInvertite).
 
accumulaCoppie([], _, []).
accumulaCoppie([Coeff|Resto], Grado, [(Grado, Coeff)|CoppieResto]) :-
    GradoSucc is Grado + 1,
    accumulaCoppie(Resto, GradoSucc, CoppieResto).
 
% Gestione dichiarativa del segno da anteporre a ogni termine.
formattaSegno(Coefficiente, 1) :- Coefficiente < 0, !, write('-').
formattaSegno(_, 1) :- !.
formattaSegno(Coefficiente, 0) :- Coefficiente < 0, !, write(' - ').
formattaSegno(_, 0) :- write(' + ').
 
% Formattazione dei singoli monomi (omette x^0, x^1 e coefficienti unitari).
formattaMonomio(0, Coefficiente) :- !, scriviValore(Coefficiente).
formattaMonomio(1, Coefficiente) :-
    tolleranzaNumerica(T), abs(Coefficiente - 1.0) < T, !, write('x').
formattaMonomio(1, Coefficiente) :- !, scriviValore(Coefficiente), write('x').
formattaMonomio(Grado, Coefficiente) :-
    tolleranzaNumerica(T), abs(Coefficiente - 1.0) < T, !, write('x^'), write(Grado).
formattaMonomio(Grado, Coefficiente) :-
    scriviValore(Coefficiente), write('x^'), write(Grado).
 
% -------------------------------------------------------------------------
% Operazioni Aritmetiche di Base
% -------------------------------------------------------------------------
 
sommaPolinomi([], SecondoPoli, Risultato) :- !, rimuoviZeriInTesta(SecondoPoli, Risultato).
sommaPolinomi(PrimoPoli, [], Risultato) :- !, rimuoviZeriInTesta(PrimoPoli, Risultato).
sommaPolinomi([C1|Resto1], [C2|Resto2], [Somma|SommeResto]) :-
    Somma is C1 + C2,
    sommaPolinomi(Resto1, Resto2, SommeResto).
 
sottraiPolinomi([], SecondoPoli, Risultato) :- !, negaCoefficienti(SecondoPoli, Risultato).
sottraiPolinomi(PrimoPoli, [], Risultato) :- !, rimuoviZeriInTesta(PrimoPoli, Risultato).
sottraiPolinomi([C1|Resto1], [C2|Resto2], [Diff|DiffResto]) :-
    Diff is C1 - C2,
    sottraiPolinomi(Resto1, Resto2, DiffResto).
 
negaCoefficienti([], []).
negaCoefficienti([X|Resto], [NegX|RestoNeg]) :- NegX is -X, negaCoefficienti(Resto, RestoNeg).
 
moltiplPolinomi([], _, []) :- !.
moltiplPolinomi(_, [], []) :- !.
moltiplPolinomi([CoeffTesta|CoeffResto], SecondoPoli, Prodotto) :-
    moltiplicaPerScalare(SecondoPoli, CoeffTesta, ProdottoTesta),
    moltiplPolinomi(CoeffResto, SecondoPoli, ProdottoResto),
    sommaPolinomi(ProdottoTesta, [0.0|ProdottoResto], Prodotto).
 
moltiplicaPerScalare([], _, []).
moltiplicaPerScalare([Y|YResto], Scalare, [Prod|ProdResto]) :-
    Prod is Y * Scalare,
    moltiplicaPerScalare(YResto, Scalare, ProdResto).
 
% -------------------------------------------------------------------------
% Divisione Euclidea
% -------------------------------------------------------------------------
 
% Interfaccia pubblica: normalizza gli input e avvia la divisione ricorsiva.
divisioneConResto(Dividendo, Divisore, Quoziente, Resto) :-
    rimuoviZeriInTesta(Dividendo, DividendoNorm),
    rimuoviZeriInTesta(Divisore, DivisoreNorm),
    passoDiv(DividendoNorm, DivisoreNorm, [], Quoziente, Resto).
 
% Caso base: grado del dividendo residuo < grado del divisore.
% Il quoziente accumulato e il dividendo residuo diventano rispettivamente quoziente e resto.
passoDiv(DividendoCorrente, Divisore, QuozienteParziale, Quoziente, Resto) :-
    length(DividendoCorrente, LunghDividendo),
    length(Divisore, LunghDivisore),
    LunghDividendo < LunghDivisore, !,
    rimuoviZeriInTesta(QuozienteParziale, Quoziente),
    rimuoviZeriInTesta(DividendoCorrente, Resto).
 
% Passo ricorsivo: calcola il termine del quoziente e riduce il dividendo.
passoDiv(DividendoCorrente, Divisore, QuozienteParziale, Quoziente, Resto) :-
    last(DividendoCorrente, CoeffDirettoreDividendo),
    last(Divisore, CoeffDirettoreDivisore),
    length(DividendoCorrente, LunghDividendo),
    length(Divisore, LunghDivisore),
    CoefficienteDelPasso is CoeffDirettoreDividendo / CoeffDirettoreDivisore,
    DifferenzaDiGrado is LunghDividendo - LunghDivisore,
    costruisciMonomio(DifferenzaDiGrado, CoefficienteDelPasso, TermineCorrente),
    moltiplPolinomi(Divisore, TermineCorrente, TermineDaSottrarre),
    sottraiPolinomi(DividendoCorrente, TermineDaSottrarre, DividendoRidotto),
    sommaPolinomi(QuozienteParziale, TermineCorrente, QuozienteAggiornato),
    rimuoviZeriInTesta(DividendoRidotto, DividendoRidottoNorm),
    passoDiv(DividendoRidottoNorm, Divisore, QuozienteAggiornato, Quoziente, Resto).
 
% Costruisce un monomio come lista densa con zeri nelle posizioni di grado inferiore.
costruisciMonomio(0, Coefficiente, [Coefficiente]) :- !.
costruisciMonomio(N, Coefficiente, [0.0|Resto]) :-
    N > 0, N1 is N - 1,
    costruisciMonomio(N1, Coefficiente, Resto).
 
% -------------------------------------------------------------------------
% MCD (Algoritmo di Euclide, reso monico)
% -------------------------------------------------------------------------
 
calcolaMCD(PoliA, PoliB, MCD) :-
    rimuoviZeriInTesta(PoliA, PoliANorm),
    rimuoviZeriInTesta(PoliB, PoliBNorm),
    algoritmoEuclide(PoliANorm, PoliBNorm, MCD).
 
% Caso base: quando il secondo polinomio è vuoto, l'MCD è il primo reso monico.
algoritmoEuclide(PoliA, [], MCD) :- !, rendiMonico(PoliA, MCD).
algoritmoEuclide(PoliA, PoliB, MCD) :-
    divisioneConResto(PoliA, PoliB, _, Resto),
    algoritmoEuclide(PoliB, Resto, MCD).
 
% Divide tutti i coefficienti per il coefficiente direttore (l'ultimo della lista).
rendiMonico([], []) :- !.
rendiMonico(Coefficienti, Monico) :-
    last(Coefficienti, CoeffDirettore),
    InversoCoefficiente is 1.0 / CoeffDirettore,
    moltiplicaPerScalare(Coefficienti, InversoCoefficiente, Monico).
 
% -------------------------------------------------------------------------
% Lettura Input
% -------------------------------------------------------------------------

leggiPolinomio(Etichetta, Polinomio) :-
    write('Inserisci i coefficienti del polinomio '), write(Etichetta),
    write(' separati da spazi (ordine crescente): '), nl,
    read_line_to_string(user_input, RigaInput),
    split_string(RigaInput, " ", " ", ListaStringhe),
    elaboraInput(Etichetta, ListaStringhe, Polinomio).
 
% Clausola 1: L'input è valido. Procediamo alla conversione e alla normalizzazione.
elaboraInput(_, ListaStringhe, Polinomio) :-
    stringheValide(ListaStringhe), !,
    convertiStringheInFloat(ListaStringhe, Coefficienti),
    rimuoviZeriInTesta(Coefficienti, Polinomio).
 
% Clausola 2: L'input NON è valido. Segnala l'errore e fallisci ricorsivamente per riprovare.
elaboraInput(Etichetta, _, Polinomio) :-
    write('*** ERRORE: L\'input contiene caratteri non numerici o non validi. Riprova. ***'), nl, nl,
    leggiPolinomio(Etichetta, Polinomio).
 
% Verifica ricorsiva: controlla se ogni elemento della lista è un numero valido.
stringheValide([]).
stringheValide([S|Resto]) :-
    S == "", !,
    stringheValide(Resto). % Salta gli spazi vuoti multipli inseriti per errore.
stringheValide([S|Resto]) :-
    catch(number_string(_, S), _, fail), % Tenta la conversione: se fallisce o lancia eccezione, fallisce la clausola.
    stringheValide(Resto).

% Mantiene intatta la conversione effettiva che avevi già implementato
convertiStringheInFloat([], []).
convertiStringheInFloat([S|Resto], [F|FloatResto]) :-
    S \= "", !,
    number_string(F, S),
    convertiStringheInFloat(Resto, FloatResto).
convertiStringheInFloat([_|Resto], FloatResto) :-
    convertiStringheInFloat(Resto, FloatResto).