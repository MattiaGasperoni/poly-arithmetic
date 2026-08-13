/* Programma Prolog per operazioni ed algoritmi su polinomi. */

/* La costante tolleranza rappresenta la soglia al di sotto della quale un valore Double
   viene considerato pari a zero, al fine di compensare gli errori di arrotondamento
   tipici dell'aritmetica in virgola mobile. */
tolleranza_numerica(1e-6).

main :-
    acquisisci_polinomio('A', Primo_poli),
    acquisisci_polinomio('B', Secondo_poli),
    nl, write('Polinomio A:  '), mostra(Primo_poli), nl,
    write('Polinomio B:  '), mostra(Secondo_poli), nl,
    grado_polinomio(Primo_poli, Grado_a), write('Grado A:      '), write(Grado_a), nl,
    grado_polinomio(Secondo_poli, Grado_b), write('Grado B:      '), write(Grado_b), nl,
    somma(Primo_poli, Secondo_poli, Somma), write('Somma:        '), mostra(Somma), nl,
    differenza(Primo_poli, Secondo_poli, Differenza), write('Differenza:   '), mostra(Differenza), nl,
    prodotto(Primo_poli, Secondo_poli, Prodotto), write('Prodotto:     '), mostra(Prodotto), nl,
    ( divisione(Primo_poli, Secondo_poli, Quoziente, Resto) ->
        write('Quoziente:    '), mostra(Quoziente), nl,
        write('Resto:        '), mostra(Resto), nl
    ; write('Errore:       impossibile dividere per il polinomio nullo.'), nl
    ),
    mcd(Primo_poli, Secondo_poli, Mcd), write('MCD:          '), mostra(Mcd), nl.

/* Il predicato acquisisci_polinomio acquisisce un polinomio di coefficienti Double da tastiera,
   restituendo la lista dei coefficienti in ordine crescente di grado. */
acquisisci_polinomio(Etichetta, Polinomio) :-
    write('Inserisci i coefficienti del polinomio '), write(Etichetta),
    write(' separati da spazi (ordine crescente): '), nl,
    read_line_to_string(user_input, Riga_input),
    normalize_space(atom(Riga_normalizzata), Riga_input),
    ( Riga_normalizzata == '' ->
        write('ERRORE: Devi inserire almeno un coefficiente esplicito!'), nl, nl,
        acquisisci_polinomio(Etichetta, Polinomio)
    ; split_string(Riga_input, " \t", " \t", Lista_stringhe),
      elabora_input(Etichetta, Lista_stringhe, Polinomio)
    ).

/* Il predicato elabora_input converte l'input testuale in coefficienti numerici tramite converti_input,
   ripetendo l'acquisizione in caso di formato non valido. */
elabora_input(_, Lista_stringhe, Polinomio) :-
    converti_input(Lista_stringhe, Coefficienti), !,
    rimuovi_zeri_in_testa(Coefficienti, Polinomio).
elabora_input(Etichetta, _, Polinomio) :-
    write('ERRORE: L\'input contiene caratteri non numerici o non validi.'), nl, nl,
    acquisisci_polinomio(Etichetta, Polinomio).

/* Il predicato converti_input converte le stringhe non vuote in coefficienti numerici,
   scartando le stringhe vuote generate dagli spazi multipli. */
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

/* Il predicato rimuovi_zeri_in_testa elimina gli zeri di testa (grado massimo) di un polinomio:
   il suo primo argomento è la lista dei coefficienti in ordine crescente di grado. */
rimuovi_zeri_in_testa(Polinomio, Polinomio_norm) :-
    reverse(Polinomio, Invertito),
    rimuovi_zeri_da_lista_invertita(Invertito, Invertito_ripulito),
    reverse(Invertito_ripulito, Polinomio_norm).

% Il predicato ausiliario rimuovi_zeri_da_lista_invertita elimina ricorsivamente
% i coefficienti di testa (lista invertita) il cui valore assoluto è sotto tolleranza.
rimuovi_zeri_da_lista_invertita([X|Resto], Risultato) :-
    tolleranza_numerica(T), abs(X) < T, !,
    rimuovi_zeri_da_lista_invertita(Resto, Risultato).
rimuovi_zeri_da_lista_invertita(Lista, Lista).

/* Il grado viene calcolato come la lunghezza del polinomio normalizzato meno uno oppure 0 se nullo. */
grado_polinomio(Polinomio, Grado) :-
    rimuovi_zeri_in_testa(Polinomio, Poli_norm),
    length(Poli_norm, Lunghezza),
    Lunghezza > 0, !,
    Grado is Lunghezza - 1.
grado_polinomio(_, 0).

/* Il predicato mostra stampa la rappresentazione algebrica di un polinomio, dal grado massimo al minimo.
   I predicati ausiliari formatta_segno/2 e formatta_monomio/2 sono utilizzati esclusivamente da questo predicato. */
mostra(Polinomio) :-
    rimuovi_zeri_in_testa(Polinomio, Poli_norm),
    costruisci_coppie_grado_coeff(Poli_norm, Coppie_invertite),
    formatta_termini(Coppie_invertite, 1).

% Il predicato ausiliario costruisci_coppie_grado_coeff costruisce, tramite accumula_coppie,
% le coppie (grado, coefficiente) ordinate dal grado massimo al minimo.
costruisci_coppie_grado_coeff(Polinomio, Coppie_invertite) :-
    accumula_coppie(Polinomio, 0, Coppie),
    reverse(Coppie, Coppie_invertite).

% Il predicato ausiliario accumula_coppie associa ad ogni coefficiente il proprio grado,
% percorrendo il polinomio in ordine crescente a partire dal grado 0.
accumula_coppie([], _, []).
accumula_coppie([Coeff|Resto], Grado, [(Grado, Coeff)|Coppie_resto]) :-
    Grado_succ is Grado + 1,
    accumula_coppie(Resto, Grado_succ, Coppie_resto).

% Il predicato ausiliario formatta_termini stampa ricorsivamente i termini di un polinomio,
% dal grado massimo al minimo, saltando i coefficienti nulli.
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

% Il predicato ausiliario formatta_segno stampa il segno da anteporre al termine corrente.
formatta_segno(Coefficiente, 1) :- Coefficiente < 0, !, write('-').
formatta_segno(_, 1) :- !.
formatta_segno(Coefficiente, 0) :- Coefficiente < 0, !, write(' - ').
formatta_segno(_, 0) :- write(' + ').

% Il predicato ausiliario formatta_monomio stampa grado e coefficiente di un monomio,
% omettendo coefficienti unitari e potenze 0/1.
formatta_monomio(0, Coefficiente) :- !, scrivi_valore(Coefficiente).
formatta_monomio(1, Coefficiente) :-
    tolleranza_numerica(T), abs(Coefficiente - 1.0) < T, !, write('x').
formatta_monomio(1, Coefficiente) :- !, scrivi_valore(Coefficiente), write('x').
formatta_monomio(Grado, Coefficiente) :-
    tolleranza_numerica(T), abs(Coefficiente - 1.0) < T, !, write('x^'), write(Grado).
formatta_monomio(Grado, Coefficiente) :-
    scrivi_valore(Coefficiente), write('x^'), write(Grado).

/* Il predicato scrivi_valore restituisce la rappresentazione testuale di un coefficiente,
   come intero se la parte decimale è trascurabile, altrimenti arrotondato a 4 cifre decimali. */
scrivi_valore(X) :-
    tolleranza_numerica(T),
    Diff is abs(X - round(X)),
    ( Diff < T
    -> R is round(X), write(R)
    ;  Val is round(X * 10000) / 10000, write(Val)
    ).

/* Il predicato somma calcola la somma di due polinomi in tempo lineare O(n). */
somma(Primo_poli, Secondo_poli, Risultato) :-
    somma_liste(Primo_poli, Secondo_poli, R),
    rimuovi_zeri_in_testa(R, Risultato).

% Il predicato ausiliario somma_liste somma termine a termine le due liste di coefficienti,
% restituendo la coda residua della lista più lunga oltre la fine dell'altra.
somma_liste([], P2, P2) :- !.
somma_liste(P1, [], P1) :- !.
somma_liste([C1|Resto1], [C2|Resto2], [Somma|Somme_resto]) :-
    Somma is C1 + C2,
    somma_liste(Resto1, Resto2, Somme_resto).

/* Il predicato differenza calcola la differenza tra due polinomi in tempo lineare O(n). */
differenza(Primo_poli, Secondo_poli, Risultato) :-
    differenza_liste(Primo_poli, Secondo_poli, R),
    rimuovi_zeri_in_testa(R, Risultato).

% Il predicato ausiliario differenza_liste sottrae termine a termine le due liste di coefficienti,
% cambiando segno alla coda residua del secondo polinomio se il primo termina prima.
differenza_liste([], Secondo_poli, Risultato) :- !, moltiplica_per_scalare(Secondo_poli, -1, Risultato).
differenza_liste(Primo_poli, [], Primo_poli) :- !.
differenza_liste([C1|Resto1], [C2|Resto2], [Diff|Diff_resto]) :-
    Diff is C1 - C2,
    differenza_liste(Resto1, Resto2, Diff_resto).

/* Il predicato prodotto calcola il prodotto di due polinomi. */
prodotto(Primo_poli, Secondo_poli, Risultato) :-
    prodotto_liste(Primo_poli, Secondo_poli, R),
    rimuovi_zeri_in_testa(R, Risultato).

% Il predicato ausiliario prodotto_liste calcola il prodotto tramite somma_liste, senza
% richiamare rimuovi_zeri_in_testa ad ogni passo della ricorsione.
prodotto_liste([], _, []) :- !.
prodotto_liste(_, [], []) :- !.
prodotto_liste([Coeff_testa|Coeff_resto], Secondo_poli, Prodotto) :-
    moltiplica_per_scalare(Secondo_poli, Coeff_testa, Prodotto_testa),
    prodotto_liste(Coeff_resto, Secondo_poli, Prodotto_resto),
    somma_liste(Prodotto_testa, [0.0|Prodotto_resto], Prodotto).

% Il predicato ausiliario moltiplica_per_scalare moltiplica ogni coefficiente di una lista per uno scalare.
moltiplica_per_scalare([], _, []).
moltiplica_per_scalare([Y|Y_resto], Scalare, [Prod|Prod_resto]) :-
    Prod is Y * Scalare,
    moltiplica_per_scalare(Y_resto, Scalare, Prod_resto).

/* Il predicato divisione calcola quoziente e resto della divisione euclidea tra due polinomi. */
divisione(Dividendo, Divisore, Quoziente, Resto) :-
    rimuovi_zeri_in_testa(Divisore, Divisore_normalizzato),
    Divisore_normalizzato \= [], !,
    rimuovi_zeri_in_testa(Dividendo, Dividendo_normalizzato),
    divisione_ricorsiva(Dividendo_normalizzato, Divisore_normalizzato, [], Quoziente, Resto).

% Il predicato ausiliario divisione_ricorsiva esegue la divisione lunga accumulando il quoziente.
divisione_ricorsiva(Resto_corrente, Divisore_normalizzato, Quoziente_parziale, Quoziente, Resto) :-
    length(Resto_corrente, Lungh_resto),
    length(Divisore_normalizzato, Lungh_divisore),
    Lungh_resto < Lungh_divisore, !,
    rimuovi_zeri_in_testa(Quoziente_parziale, Quoziente),
    rimuovi_zeri_in_testa(Resto_corrente, Resto).
divisione_ricorsiva(Resto_corrente, Divisore_normalizzato, Quoziente_parziale, Quoziente, Resto) :-
    last(Resto_corrente, Coefficiente_direttore_dividendo),
    last(Divisore_normalizzato, Coefficiente_direttore_divisore),
    length(Resto_corrente, Lungh_resto),
    length(Divisore_normalizzato, Lungh_divisore),
    Coefficiente_termine is Coefficiente_direttore_dividendo / Coefficiente_direttore_divisore,
    Differenza_grado is Lungh_resto - Lungh_divisore,
    costruisci_monomio(Differenza_grado, Coefficiente_termine, Termine_quoziente),
    prodotto(Divisore_normalizzato, Termine_quoziente, Termine_da_sottrarre),
    differenza(Resto_corrente, Termine_da_sottrarre, Resto_aggiornato),
    somma(Quoziente_parziale, Termine_quoziente, Quoziente_aggiornato),
    rimuovi_zeri_in_testa(Resto_aggiornato, Resto_aggiornato_norm),
    divisione_ricorsiva(Resto_aggiornato_norm, Divisore_normalizzato, Quoziente_aggiornato, Quoziente, Resto).

% Il predicato ausiliario costruisci_monomio genera una lista densa di coefficienti,
% con zeri nei gradi inferiori, rappresentante un unico monomio.
costruisci_monomio(0, Coefficiente, [Coefficiente]) :- !.
costruisci_monomio(N, Coefficiente, [0.0|Resto]) :-
    N > 0, N_1 is N - 1,
    costruisci_monomio(N_1, Coefficiente, Resto).

/* Il predicato mcd calcola il massimo comun divisore di due polinomi tramite l'algoritmo
   di Euclide, restituendo il risultato reso monico. */
mcd(Primo_poli, Secondo_poli, Mcd) :-
    rimuovi_zeri_in_testa(Primo_poli, Primo_poli_norm),
    rimuovi_zeri_in_testa(Secondo_poli, Secondo_poli_norm),
    algoritmo_euclide(Primo_poli_norm, Secondo_poli_norm, Mcd).

% Il predicato ausiliario algoritmo_euclide applica ricorsivamente l'algoritmo di Euclide
% ai due polinomi, tramite rendi_monico, fino a ridurre il secondo polinomio al polinomio nullo.
algoritmo_euclide(Primo_poli, [], Mcd) :- !, rendi_monico(Primo_poli, Mcd).
algoritmo_euclide(Primo_poli, Secondo_poli, Mcd) :-
    divisione(Primo_poli, Secondo_poli, _, Resto),
    algoritmo_euclide(Secondo_poli, Resto, Mcd).

% Il predicato ausiliario rendi_monico divide tutti i coefficienti per il coefficiente
% direttore (l'ultimo della lista), tramite moltiplica_per_scalare.
rendi_monico([], []) :- !.
rendi_monico(Coefficienti, Monico) :-
    last(Coefficienti, Coeff_direttore),
    Inverso_coefficiente is 1.0 / Coeff_direttore,
    moltiplica_per_scalare(Coefficienti, Inverso_coefficiente, Monico).
