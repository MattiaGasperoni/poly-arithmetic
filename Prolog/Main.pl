/* Programma Prolog per operazioni ed algoritmi su polinomi. */

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
   separati da spazi, in ordine crescente di grado. */
acquisisci_polinomio(Etichetta, Polinomio) :-
    write('Inserisci i coefficienti del polinomio '), write(Etichetta),
    write(' separati da spazi (ordine crescente): '), nl,
    read_line_to_string(user_input, Riga_input),
    normalize_space(atom(Riga_normalizzata), Riga_input),
    ( Riga_normalizzata == '' ->
        write('ERRORE: Devi inserire almeno un coefficiente esplicito!'), nl, nl,
        acquisisci_polinomio(Etichetta, Polinomio)
    ; split_string(Riga_input, " ", " ", Lista_stringhe),
      elabora_input(Etichetta, Lista_stringhe, Polinomio)
    ).

% Elabora e converte l'input testuale in coefficienti numerici, ripetendo l'acquisizione in caso di formato non valido.
elabora_input(_, Lista_stringhe, Polinomio) :-
    converti_input(Lista_stringhe, Coefficienti), !,
    rimuovi_zeri_in_testa(Coefficienti, Polinomio).
elabora_input(Etichetta, _, Polinomio) :-
    write('ERRORE: L\'input contiene caratteri non numerici o non validi.'), nl, nl,
    acquisisci_polinomio(Etichetta, Polinomio).

% Converti_input converte le stringhe non vuote in coefficienti numerici, scartando le stringhe vuote.
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

/* Il predicato mostra stampa la rappresentazione algebrica di un polinomio, dal grado massimo al minimo. */
mostra(Polinomio) :-
    rimuovi_zeri_in_testa(Polinomio, Poli_norm),
    costruisci_coppie_grado_coeff(Poli_norm, Coppie_invertite),
    formatta_termini(Coppie_invertite, 1).

/* Il predicato rimuovi_zeri_in_testa elimina gli zeri di testa (grado massimo) di un polinomio. */
rimuovi_zeri_in_testa(Polinomio, Polinomio_norm) :-
    reverse(Polinomio, Invertito),
    rimuovi_zeri_da_lista_invertita(Invertito, Invertito_ripulito),
    reverse(Invertito_ripulito, Polinomio_norm).

% rimuovi_zeri_da_lista_invertita elimina ricorsivamente i coefficienti prossimi a zero.
rimuovi_zeri_da_lista_invertita([X|Resto], Risultato) :-
    tolleranza_numerica(T), abs(X) < T, !,
    rimuovi_zeri_da_lista_invertita(Resto, Risultato).
rimuovi_zeri_da_lista_invertita(Lista, Lista).

% costruisci_coppie_grado_coeff costruisce le coppie (grado, coefficiente) ordinate dal grado massimo al minimo.
costruisci_coppie_grado_coeff(Polinomio, Coppie_invertite) :-
    accumula_coppie(Polinomio, 0, Coppie),
    reverse(Coppie, Coppie_invertite).

% accumula_coppie associa ad ogni coefficiente il proprio grado.
accumula_coppie([], _, []).
accumula_coppie([Coeff|Resto], Grado, [(Grado, Coeff)|Coppie_resto]) :-
    Grado_succ is Grado + 1,
    accumula_coppie(Resto, Grado_succ, Coppie_resto).

% formatta_termini stampa ricorsivamente i termini di un polinomio.
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

% formatta_monomio stampa grado e coefficiente di un monomio.
formatta_monomio(0, Coefficiente) :- !, scrivi_valore(Coefficiente).
formatta_monomio(1, Coefficiente) :-
    tolleranza_numerica(T), abs(Coefficiente - 1.0) < T, !, write('x').
formatta_monomio(1, Coefficiente) :- !, scrivi_valore(Coefficiente), write('x').
formatta_monomio(Grado, Coefficiente) :-
    tolleranza_numerica(T), abs(Coefficiente - 1.0) < T, !, write('x^'), write(Grado).
formatta_monomio(Grado, Coefficiente) :-
    scrivi_valore(Coefficiente), write('x^'), write(Grado).

% scrivi_valore formatta l'output numerico come intero o decimale a 4 cifre.
scrivi_valore(X) :-
    tolleranza_numerica(T),
    Diff is abs(X - round(X)),
    ( Diff < T
    -> R is round(X), write(R)
    ;  Val is round(X * 10000) / 10000, write(Val)
    ).

/* Il predicato grado_polinomio calcola il grado di un polinomio. */
grado_polinomio(Polinomio, Grado) :-
    rimuovi_zeri_in_testa(Polinomio, Poli_norm),
    length(Poli_norm, Lunghezza),
    Lunghezza > 0, !,
    Grado is Lunghezza - 1.
grado_polinomio(_, 0).

/* Il predicato somma calcola la somma di due polinomi normalizzando alla fine O(n). */
somma(Primo_poli, Secondo_poli, Risultato) :-
    somma_liste(Primo_poli, Secondo_poli, R),
    rimuovi_zeri_in_testa(R, Risultato).

somma_liste([], P2, P2) :- !.
somma_liste(P1, [], P1) :- !.
somma_liste([C1|Resto1], [C2|Resto2], [Somma|Somme_resto]) :-
    Somma is C1 + C2,
    somma_liste(Resto1, Resto2, Somme_resto).

/* Il predicato differenza calcola la differenza tra due polinomi normalizzando alla fine O(n). */
differenza(Primo_poli, Secondo_poli, Risultato) :-
    differenza_liste(Primo_poli, Secondo_poli, R),
    rimuovi_zeri_in_testa(R, Risultato).

differenza_liste([], Secondo_poli, Risultato) :- !, moltiplica_per_scalare(Secondo_poli, -1, Risultato).
differenza_liste(Primo_poli, [], Primo_poli) :- !.
differenza_liste([C1|Resto1], [C2|Resto2], [Diff|Diff_resto]) :-
    Diff is C1 - C2,
    differenza_liste(Resto1, Resto2, Diff_resto).

/* Il predicato prodotto calcola il prodotto senza sovraccaricare la ricorsione interna. */
prodotto(Primo_poli, Secondo_poli, Risultato) :-
    prodotto_liste(Primo_poli, Secondo_poli, R),
    rimuovi_zeri_in_testa(R, Risultato).

prodotto_liste([], _, []) :- !.
prodotto_liste(_, [], []) :- !.
prodotto_liste([Coeff_testa|Coeff_resto], Secondo_poli, Prodotto) :-
    moltiplica_per_scalare(Secondo_poli, Coeff_testa, Prodotto_testa),
    prodotto_liste(Coeff_resto, Secondo_poli, Prodotto_resto),
    somma_liste(Prodotto_testa, [0.0|Prodotto_resto], Prodotto).

% moltiplica_per_scalare moltiplica ogni coefficiente per uno scalare.
moltiplica_per_scalare([], _, []).
moltiplica_per_scalare([Y|Y_resto], Scalare, [Prod|Prod_resto]) :-
    Prod is Y * Scalare,
    moltiplica_per_scalare(Y_resto, Scalare, Prod_resto).

/* Il predicato divisione calcola quoziente e resto. */
divisione(Dividendo, Divisore, Quoziente, Resto) :-
    rimuovi_zeri_in_testa(Divisore, Divisore_norm),
    Divisore_norm \= [], !,
    rimuovi_zeri_in_testa(Dividendo, Dividendo_norm),
    divisione_ricorsiva(Dividendo_norm, Divisore_norm, [], Quoziente, Resto).

% divisione_ricorsiva esegue la divisione lunga accumulando il quoziente.
divisione_ricorsiva(Dividendo_corrente, Divisore, Quoziente_parziale, Quoziente, Resto) :-
    length(Dividendo_corrente, Lungh_dividendo),
    length(Divisore, Lungh_divisore),
    Lungh_dividendo < Lungh_divisore, !,
    rimuovi_zeri_in_testa(Quoziente_parziale, Quoziente),
    rimuovi_zeri_in_testa(Dividendo_corrente, Resto).
divisione_ricorsiva(Dividendo_corrente, Divisore, Quoziente_parziale, Quoziente, Resto) :-
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
    divisione_ricorsiva(Dividendo_ridotto_norm, Divisore, Quoziente_aggiornato, Quoziente, Resto).

% costruisci_monomio genera una lista densa con zeri di grado inferiore.
costruisci_monomio(0, Coefficiente, [Coefficiente]) :- !.
costruisci_monomio(N, Coefficiente, [0.0|Resto]) :-
    N > 0, N_1 is N - 1,
    costruisci_monomio(N_1, Coefficiente, Resto).

/* Il predicato mcd calcola il massimo comun divisore di due polinomi. */
mcd(Poli_a, Poli_b, Mcd) :-
    rimuovi_zeri_in_testa(Poli_a, Poli_a_norm),
    rimuovi_zeri_in_testa(Poli_b, Poli_b_norm),
    algoritmo_euclide(Poli_a_norm, Poli_b_norm, Mcd).

algoritmo_euclide(Poli_a, [], Mcd) :- !, rendi_monico(Poli_a, Mcd).
algoritmo_euclide(Poli_a, Poli_b, Mcd) :-
    divisione(Poli_a, Poli_b, _, Resto),
    algoritmo_euclide(Poli_b, Resto, Mcd).

% rendi_monico divide tutti i coefficienti per il coefficiente direttore.
rendi_monico([], []) :- !.
rendi_monico(Coefficienti, Monico) :-
    last(Coefficienti, Coeff_direttore),
    Inverso_coefficiente is 1.0 / Coeff_direttore,
    moltiplica_per_scalare(Coefficienti, Inverso_coefficiente, Monico).