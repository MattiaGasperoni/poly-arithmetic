/* Programma Prolog per operazioni ed algoritmi avanzati su polinomi. */

/* La costante tolleranza rappresenta la soglia al di sotto della quale un valore Double
   viene considerato pari a zero, al fine di compensare gli errori di arrotondamento
   tipici dell'aritmetica in virgola mobile. */
tolleranza_numerica(1e-6).

main :-
    acquisisci_polinomio('A', Poli_a),
    acquisisci_polinomio('B', Poli_b),
    nl, write('Polinomio A:  '), mostra(Poli_a), nl,
    write('Polinomio B:  '), mostra(Poli_b), nl,
    grado_polinomio(Poli_a, Grado_a), write('Grado A:      '), write(Grado_a), nl,
    grado_polinomio(Poli_b, Grado_b), write('Grado B:      '), write(Grado_b), nl,
    somma(Poli_a, Poli_b, Somma), write('Somma:        '), mostra(Somma), nl,
    differenza(Poli_a, Poli_b, Differenza), write('Differenza:   '), mostra(Differenza), nl,
    prodotto(Poli_a, Poli_b, Prodotto), write('Prodotto:     '), mostra(Prodotto), nl,
    ( divisione(Poli_a, Poli_b, Quoziente, Resto) ->
        write('Quoziente:    '), mostra(Quoziente), nl,
        write('Resto:        '), mostra(Resto), nl
    ; write('Errore:       impossibile dividere per il polinomio nullo.'), nl
    ),
    mcd(Poli_a, Poli_b, Mcd), write('MCD:          '), mostra(Mcd), nl.

/* Il predicato acquisisci_polinomio acquisisce un polinomio leggendo i suoi coefficienti da tastiera,
   separati da spazi, in ordine crescente di grado:
   - il primo argomento è una stringa che specifica di quale polinomio si tratta;
   - il secondo argomento è la lista dei coefficienti del polinomio acquisito. */
acquisisci_polinomio(Etichetta, Polinomio) :-
    write('Inserisci i coefficienti del polinomio '), write(Etichetta),
    write(' separati da spazi (ordine crescente): '), nl,
    read_line_to_string(user_input, Riga_input),
    split_string(Riga_input, " ", " ", Lista_stringhe),
    elabora_input(Etichetta, Lista_stringhe, Polinomio).

% Elabora e converte l'input testuale in coefficienti numerici, ripetendo l'acquisizione in caso di formato non valido.
elabora_input(_, Lista_stringhe, Polinomio) :-
    converti_input(Lista_stringhe, Coefficienti), !,
    rimuovi_zeri_in_testa(Coefficienti, Polinomio).

elabora_input(Etichetta, _, Polinomio) :-
    write('*** ERRORE: L\'input contiene caratteri non numerici o non validi. Riprova. ***'), nl, nl,
    acquisisci_polinomio(Etichetta, Polinomio).

% Converti_input converte le stringhe non vuote in coefficienti numerici, scartando le stringhe vuote e fallendo se una stringa non è un numero valido.
converti_input(Lista_stringhe, Coefficienti) :-
    converti_input(Lista_stringhe, [], Coefficienti_invertiti),
    reverse(Coefficienti_invertiti, Coefficienti).

converti_input([], Acc, Acc).
converti_input([S|Resto], Acc, Coefficienti) :-
    S == "", !,
    converti_input(Resto, Acc, Coefficienti).
converti_input([S|Resto], Acc, Coefficienti) :-
    catch(number_string(F, S), _, fail),
    converti_input(Resto, [F|Acc], Coefficienti).

/* Il predicato mostra stampa la rappresentazione algebrica di un polinomio, dal grado massimo al minimo:
   - il suo unico argomento è la lista dei coefficienti del polinomio in ordine crescente di grado.
   I predicati ausiliari formatta_segno e formatta_monomio sono usati solo qui. */
mostra(Polinomio) :-
    rimuovi_zeri_in_testa(Polinomio, Poli_norm),
    costruisci_coppie_grado_coeff(Poli_norm, Coppie_invertite),
    formatta_termini(Coppie_invertite, 1).

/* Il predicato rimuovi_zeri_in_testa elimina gli zeri di testa (grado massimo) di un polinomio:
   - il primo argomento è la lista dei coefficienti in ordine crescente di grado;
   - il secondo argomento è la lista risultante, priva degli zeri di testa. */
rimuovi_zeri_in_testa(Polinomio, Polinomio_norm) :-
    reverse(Polinomio, Invertito),
    rimuovi_zeri_da_lista_invertita(Invertito, Invertito_ripulito),
    reverse(Invertito_ripulito, Polinomio_norm).

% rimuovi_zeri_da_lista_invertita elimina ricorsivamente i coefficienti prossimi a zero a partire dall'inizio della lista invertita dei coefficienti.
rimuovi_zeri_da_lista_invertita([X|Resto], Risultato) :-
    tolleranza_numerica(T), abs(X) < T, !,
    rimuovi_zeri_da_lista_invertita(Resto, Risultato).
rimuovi_zeri_da_lista_invertita(Lista, Lista).

% costruisci_coppie_grado_coeff costruisce le coppie (grado, coefficiente) di un polinomio, ordinate dal grado massimo al grado minimo.
costruisci_coppie_grado_coeff(Polinomio, Coppie_invertite) :-
    accumula_coppie(Polinomio, 0, Coppie),
    reverse(Coppie, Coppie_invertite).

% accumula_coppie costruisce ricorsivamente le coppie (grado, coefficiente), associando ad ogni coefficiente il proprio grado.
accumula_coppie([], _, []).
accumula_coppie([Coeff|Resto], Grado, [(Grado, Coeff)|Coppie_resto]) :-
    Grado_succ is Grado + 1,
    accumula_coppie(Resto, Grado_succ, Coppie_resto).

% formatta_termini stampa ricorsivamente i termini di un polinomio a partire dalle coppie (grado, coefficiente), dal grado massimo al minimo.
formatta_termini([], 1) :- !, write('0').
formatta_termini([], 0) :- !.
formatta_termini([(_, Coefficiente)|Resto], E_primo_termine) :-
    tolleranza_numerica(T), abs(Coefficiente) < T, !,
    formatta_termini(Resto, E_primo_termine).
formatta_termini([(Grado, Coefficiente)|Resto], E_primo_termine) :-
    formatta_segno(Coefficiente, E_primo_termine),
    Valore_assoluto is abs(Coefficiente),
    formatta_monomio(Grado, Valore_assoluto),
    formatta_termini(Resto, 0).

% formatta_segno stampa il segno da anteporre al termine corrente.
formatta_segno(Coefficiente, 1) :- Coefficiente < 0, !, write('-').
formatta_segno(_, 1) :- !.
formatta_segno(Coefficiente, 0) :- Coefficiente < 0, !, write(' - ').
formatta_segno(_, 0) :- write(' + ').

% formatta_monomio stampa grado e coefficiente di un monomio, omettendo i coefficienti unitari e le potenze 0/1.
formatta_monomio(0, Coefficiente) :- !, scrivi_valore(Coefficiente).
formatta_monomio(1, Coefficiente) :-
    tolleranza_numerica(T), abs(Coefficiente - 1.0) < T, !, write('x').
formatta_monomio(1, Coefficiente) :- !, scrivi_valore(Coefficiente), write('x').
formatta_monomio(Grado, Coefficiente) :-
    tolleranza_numerica(T), abs(Coefficiente - 1.0) < T, !, write('x^'), write(Grado).
formatta_monomio(Grado, Coefficiente) :-
    scrivi_valore(Coefficiente), write('x^'), write(Grado).

/* Il predicato scrivi_valore stampa la rappresentazione testuale di un coefficiente,
   come intero se la parte decimale è trascurabile, altrimenti arrotondato a 4 cifre:
   - il suo unico argomento è il valore del coefficiente da stampare. */
scrivi_valore(X) :-
    tolleranza_numerica(T),
    Diff is abs(X - round(X)),
    scrivi_valore_con_precisione(Diff, T, X).

% scrivi_valore_con_precisione stampa X come intero se la differenza dall'intero più vicino è sotto la tolleranza, altrimenti arrotondato a 4 cifre decimali.
scrivi_valore_con_precisione(Diff, T, X) :- Diff < T, !, R is round(X), write(R).
scrivi_valore_con_precisione(_, _, X) :- Val is round(X * 10000) / 10000, write(Val).

grado_polinomio([], 0) :- !.
grado_polinomio(Polinomio, Grado) :-
    rimuovi_zeri_in_testa(Polinomio, Poli_norm),
    length(Poli_norm, Lunghezza),
    Lunghezza > 0, !,
    Grado is Lunghezza - 1.
grado_polinomio(_, 0).

/* Il predicato somma calcola la somma di due polinomi:
   - il primo argomento è il primo polinomio;
   - il secondo argomento è il secondo polinomio;
   - il terzo argomento è il polinomio somma. */
somma([], Secondo_poli, Risultato) :- !, rimuovi_zeri_in_testa(Secondo_poli, Risultato).
somma(Primo_poli, [], Risultato) :- !, rimuovi_zeri_in_testa(Primo_poli, Risultato).
somma([C1|Resto1], [C2|Resto2], [Somma|Somme_resto]) :-
    Somma is C1 + C2,
    somma(Resto1, Resto2, Somme_resto).

/* Il predicato differenza calcola la differenza tra due polinomi:
   - il primo argomento è il primo polinomio;
   - il secondo argomento è il secondo polinomio;
   - il terzo argomento è il polinomio differenza. */
differenza([], Secondo_poli, Risultato) :- !, moltiplica_per_scalare(Secondo_poli, -1, Risultato).
differenza(Primo_poli, [], Risultato) :- !, rimuovi_zeri_in_testa(Primo_poli, Risultato).
differenza([C1|Resto1], [C2|Resto2], [Diff|Diff_resto]) :-
    Diff is C1 - C2,
    differenza(Resto1, Resto2, Diff_resto).

/* Il predicato prodotto calcola il prodotto di due polinomi:
   - il primo argomento è il primo polinomio;
   - il secondo argomento è il secondo polinomio;
   - il terzo argomento è il polinomio prodotto. */
prodotto([], _, []) :- !.
prodotto(_, [], []) :- !.
prodotto([Coeff_testa|Coeff_resto], Secondo_poli, Prodotto) :-
    moltiplica_per_scalare(Secondo_poli, Coeff_testa, Prodotto_testa),
    prodotto(Coeff_resto, Secondo_poli, Prodotto_resto),
    somma(Prodotto_testa, [0.0|Prodotto_resto], Prodotto).

% moltiplica_per_scalare moltiplica ogni coefficiente di un polinomio per uno scalare.
moltiplica_per_scalare([], _, []).
moltiplica_per_scalare([Y|Y_resto], Scalare, [Prod|Prod_resto]) :-
    Prod is Y * Scalare,
    moltiplica_per_scalare(Y_resto, Scalare, Prod_resto).

/* Il predicato divisione calcola il quoziente e il resto della divisione euclidea tra due polinomi:
   - il primo argomento è il dividendo;
   - il secondo argomento è il divisore;
   - il terzo argomento è il quoziente;
   - il quarto argomento è il resto.
   Il predicato fallisce se il divisore è il polinomio nullo. */
divisione(Dividendo, Divisore, Quoziente, Resto) :-
    rimuovi_zeri_in_testa(Divisore, Divisore_norm),
    Divisore_norm \= [], !,
    rimuovi_zeri_in_testa(Dividendo, Dividendo_norm),
    passo_div(Dividendo_norm, Divisore_norm, [], Quoziente, Resto).

% passo_div esegue la divisione lunga accumulando il quoziente.
passo_div(Dividendo_corrente, Divisore, Quoziente_parziale, Quoziente, Resto) :-
    length(Dividendo_corrente, Lungh_dividendo),
    length(Divisore, Lungh_divisore),
    Lungh_dividendo < Lungh_divisore, !,
    rimuovi_zeri_in_testa(Quoziente_parziale, Quoziente),
    rimuovi_zeri_in_testa(Dividendo_corrente, Resto).

passo_div(Dividendo_corrente, Divisore, Quoziente_parziale, Quoziente, Resto) :-
    last(Dividendo_corrente, Coeff_direttore_dividendo),
    last(Divisore, Coeff_direttore_divisore),
    length(Dividendo_corrente, Lungh_dividendo),
    length(Divisore, Lungh_divisore),
    Coefficiente_del_passo is Coeff_direttore_dividendo / Coeff_direttore_divisore,
    Differenza_di_grado is Lungh_dividendo - Lungh_divisore,
    costruisci_monomio(Differenza_di_grado, Coefficiente_del_passo, Termine_corrente),
    prodotto(Divisore, Termine_corrente, Termine_da_sottrarre),
    differenza(Dividendo_corrente, Termine_da_sottrarre, Dividendo_ridotto),
    somma(Quoziente_parziale, Termine_corrente, Quoziente_aggiornato),
    rimuovi_zeri_in_testa(Dividendo_ridotto, Dividendo_ridotto_norm),
    passo_div(Dividendo_ridotto_norm, Divisore, Quoziente_aggiornato, Quoziente, Resto).

% costruisci_monomio costruisce un monomio come lista densa di coefficienti, con zeri nelle posizioni di grado inferiore.
costruisci_monomio(0, Coefficiente, [Coefficiente]) :- !.
costruisci_monomio(N, Coefficiente, [0.0|Resto]) :-
    N > 0, N_1 is N - 1,
    costruisci_monomio(N_1, Coefficiente, Resto).

/* Il predicato mcd calcola il massimo comun divisore di due polinomi tramite l'algoritmo
   di Euclide, restituendo il risultato reso monico:
   - il primo argomento è il primo polinomio;
   - il secondo argomento è il secondo polinomio;
   - il terzo argomento è il massimo comun divisore. */
mcd(Poli_a, Poli_b, Mcd) :-
    rimuovi_zeri_in_testa(Poli_a, Poli_a_norm),
    rimuovi_zeri_in_testa(Poli_b, Poli_b_norm),
    algoritmo_euclide(Poli_a_norm, Poli_b_norm, Mcd).

algoritmo_euclide(Poli_a, [], Mcd) :- !, rendi_monico(Poli_a, Mcd).
algoritmo_euclide(Poli_a, Poli_b, Mcd) :-
    divisione(Poli_a, Poli_b, _, Resto),
    algoritmo_euclide(Poli_b, Resto, Mcd).

% rendi_monico divide tutti i coefficienti per il coefficiente direttore (l'ultimo della lista).
rendi_monico([], []) :- !.
rendi_monico(Coefficienti, Monico) :-
    last(Coefficienti, Coeff_direttore),
    Inverso_coefficiente is 1.0 / Coeff_direttore,
    moltiplica_per_scalare(Coefficienti, Inverso_coefficiente, Monico).