/* Programma Prolog per operazioni ed algoritmi su polinomi */

/* La costante tolleranza rappresenta la soglia al di sotto della quale
   un valore Double viene considerato pari a zero, al fine di compensare gli
   errori di arrotondamento tipici dell'aritmetica in virgola mobile */
tolleranza_numerica(0.000001).

/* Il predicato main coordina l'intero programma: acquisisce i due polinomi,
   li visualizza, calcola e stampa il grado di ciascuno, la loro somma,
   differenza e prodotto, il quoziente e il resto della divisione euclidea
   (segnalando l'eventuale impossibilità di dividere per il polinomio nullo)
   e il loro massimo comun divisore */

main :-
    acquisisci_polinomio('A', Primo_poli),
    acquisisci_polinomio('B', Secondo_poli),
    nl, write('Polinomio A:  '), mostra(Primo_poli), nl,
    write('Polinomio B:  '), mostra(Secondo_poli), nl,
    grado_polinomio(Primo_poli, Grado_a), write('Grado A:      '),
                                          write(Grado_a),
                                          nl,
    grado_polinomio(Secondo_poli, Grado_b), write('Grado B:      '),
                                            write(Grado_b), 
                                            nl,
    somma(Primo_poli, Secondo_poli, Somma), write('Somma:        '), 
                                            mostra(Somma),
                                            nl,
    differenza(Primo_poli, Secondo_poli, Differenza), write('Differenza:   '), 
                                                      mostra(Differenza),
                                                      nl,
    prodotto(Primo_poli, Secondo_poli, Prodotto), write('Prodotto:     '),
                                                  mostra(Prodotto),
                                                  nl,
    gestisci_divisione(Primo_poli, Secondo_poli),
    mcd(Primo_poli, Secondo_poli, Mcd), write('MCD:          '),
                                        mostra(Mcd),
                                        nl.

/* Il predicato acquisisci_polinomio acquisisce un polinomio di
   coefficienti Double da tastiera, restituendo la lista dei coefficienti
   in ordine crescente di grado:
   - il primo argomento è l'etichetta (nome) del polinomio da acquisire,
     usata nel messaggio mostrato all'utente
   - il secondo argomento è la lista dei coefficienti acquisita */

acquisisci_polinomio(Etichetta, Polinomio) :-
    write('Inserisci i coefficienti del polinomio '), write(Etichetta),
    write(' separati da spazi (ordine crescente): '), nl,
    read_line_to_string(user_input, Riga_input),
    normalize_space(string(Riga_normalizzata), Riga_input),
    Riga_normalizzata \= "", !,
    split_string(Riga_normalizzata, " \t", " \t", Lista_stringhe),
    elabora_input(Etichetta, Lista_stringhe, Polinomio).
acquisisci_polinomio(Etichetta, Polinomio) :-
    write('ERRORE: Devi inserire almeno un coefficiente esplicito!'), nl, nl,
    acquisisci_polinomio(Etichetta, Polinomio).

/* Il predicato elabora_input converte l'input testuale in coefficienti
   numerici tramite converti_input, ripetendo l'acquisizione in caso di
   formato non valido:
   - il primo argomento è l'etichetta del polinomio, usata per richiedere
     nuovamente l'input in caso di errore
   - il secondo argomento è la lista delle stringhe ottenute dalla
     suddivisione della riga letta
   - il terzo argomento è la lista dei coefficienti risultante */

elabora_input(_, Lista_stringhe, Polinomio) :-
    converti_input(Lista_stringhe, Coefficienti), !,
    rimuovi_zeri_in_testa(Coefficienti, Polinomio).
elabora_input(Etichetta, _, Polinomio) :-
    write('ERRORE: L\'input contiene caratteri non numerici o non validi.'),
         nl,
         nl,
    acquisisci_polinomio(Etichetta, Polinomio).

/* Il predicato converti_input converte le stringhe non vuote in coefficienti
   numerici, scartando le stringhe vuote generate dagli spazi multipli:
   - il primo argomento è la lista delle stringhe da convertire
   - il secondo argomento è la lista dei coefficienti numerici risultante */

converti_input(Lista_stringhe, Coefficienti) :-
    converti_input(Lista_stringhe, [], Coefficienti_invertiti),
    reverse(Coefficienti_invertiti, Coefficienti).

/* Il predicato ausiliario converti_input/3 converte ricorsivamente la lista
  di stringhe in coefficienti, accumulando il risultato in ordine inverso:
  - il primo argomento è la lista delle stringhe residue da convertire
  - il secondo argomento è l'accumulatore dei coefficienti convertiti finora
  - il terzo argomento è la lista dei coefficienti risultante */

converti_input([], Acc, Acc).
converti_input([S|Resto], Acc, Coefficienti) :-
    S == "", !,
    converti_input(Resto, Acc, Coefficienti).
converti_input([S|Resto], Acc, Coefficienti) :-
    catch(number_string(F, S), _, fail),
    converti_input(Resto, [F|Acc], Coefficienti).

/* Il predicato rimuovi_zeri_in_testa elimina gli zeri di testa
   (grado massimo) di un polinomio:
   - il primo argomento è la lista dei coefficienti in ordine crescente
     di grado
   - il secondo argomento è la lista dei coefficienti risultante, priva
     degli zeri di testa */

rimuovi_zeri_in_testa(Polinomio, Polinomio_norm) :-
    reverse(Polinomio, Invertito),
    rimuovi_zeri_da_lista_invertita(Invertito, Invertito_ripulito),
    reverse(Invertito_ripulito, Polinomio_norm).

/* Il predicato ausiliario rimuovi_zeri_da_lista_invertita elimina
   ricorsivamente i coefficienti di testa (lista invertita) il cui
   valore assoluto è sotto tolleranza:
   - il primo argomento è la lista dei coefficienti in ordine invertito
     (dal grado massimo al minimo)
   - il secondo argomento è la lista risultante, priva dei coefficienti
     di testa sotto tolleranza */

rimuovi_zeri_da_lista_invertita([X|Resto], Risultato) :-
    tolleranza_numerica(T), abs(X) < T, !,
    rimuovi_zeri_da_lista_invertita(Resto, Risultato).
rimuovi_zeri_da_lista_invertita(Lista, Lista).

/* Il predicato mostra stampa la rappresentazione algebrica di un polinomio,
   dal grado massimo al minimo. I predicati ausiliari formatta_segno/2 e
   formatta_monomio/2 sono utilizzati esclusivamente da questo predicato:
   - il suo unico argomento è la lista dei coefficienti del polinomio da
     stampare, in ordine crescente di grado */

mostra(Polinomio) :-
    rimuovi_zeri_in_testa(Polinomio, Poli_norm),
    costruisci_coppie_grado_coeff(Poli_norm, Coppie_invertite),
    formatta_termini(Coppie_invertite, 1).

/* Il predicato ausiliario costruisci_coppie_grado_coeff costruisce,
   tramite accumula_coppie, le coppie (grado, coefficiente) ordinate
   dal grado massimo al minimo:
   - il primo argomento è la lista dei coefficienti in ordine crescente
     di grado
   - il secondo argomento è la lista delle coppie (grado, coefficiente)
     risultante, ordinata dal grado massimo al minimo */

costruisci_coppie_grado_coeff(Polinomio, Coppie_invertite) :-
    accumula_coppie(Polinomio, 0, Coppie),
    reverse(Coppie, Coppie_invertite).

/* Il predicato ausiliario accumula_coppie associa ad ogni coefficiente
   il proprio grado, percorrendo il polinomio in ordine crescente a 
   partire dal grado 0:
   - il primo argomento è la lista dei coefficienti residui da associare 
     al grado
   - il secondo argomento è il grado corrente, incrementato a ogni passo
     della ricorsione
   - il terzo argomento è la lista delle coppie (grado, coefficiente)
     risultante */

accumula_coppie([], _, []).
accumula_coppie([Coeff|Resto], Grado, [(Grado, Coeff)|Coppie_resto]) :-
    Grado_succ is Grado + 1,
    accumula_coppie(Resto, Grado_succ, Coppie_resto).

/* Il predicato ausiliario formatta_termini stampa ricorsivamente i
   termini di un polinomio, dal grado massimo al minimo, saltando i
   coefficienti nulli:
   - il primo argomento è la lista delle coppie (grado, coefficiente)
     residue da stampare, ordinata dal grado massimo al minimo
   - il secondo argomento è un flag (1 oppure 0) che indica se il
     termine da stampare è il primo termine non nullo del polinomio */

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

/* Il predicato ausiliario formatta_segno stampa il segno da anteporre al
   termine corrente:
   - il primo argomento è il coefficiente del termine corrente
   - il secondo argomento è il flag (1 oppure 0) che indica se il termine
     è il primo termine non nullo del polinomio */

formatta_segno(Coefficiente, 1) :- Coefficiente < 0, !, write('-').
formatta_segno(_, 1) :- !.
formatta_segno(Coefficiente, 0) :- Coefficiente < 0, !, write(' - ').
formatta_segno(_, 0) :- write(' + ').

/* Il predicato ausiliario formatta_monomio stampa grado e coefficiente di
   un monomio, omettendo coefficienti unitari e potenze 0/1:
   - il primo argomento è il grado del monomio
   - il secondo argomento è il valore assoluto del coefficiente del monomio */

formatta_monomio(0, Coefficiente) :- !, scrivi_valore(Coefficiente).
formatta_monomio(1, Coefficiente) :-
    tolleranza_numerica(T), abs(Coefficiente - 1.0) < T, !, write('x').
formatta_monomio(1, Coefficiente) :- !, scrivi_valore(Coefficiente), write('x').
formatta_monomio(Grado, Coefficiente) :- tolleranza_numerica(T),
                                         abs(Coefficiente - 1.0) < T,
                                         !,
                                         write('x^'),
                                         write(Grado).
formatta_monomio(Grado, Coefficiente) :-
    scrivi_valore(Coefficiente), write('x^'), write(Grado).

/* Il predicato scrivi_valore restituisce la rappresentazione testuale di
   un coefficiente, come intero se la parte decimale è trascurabile,
   altrimenti arrotondato a 4 cifre decimali:
   - il suo unico argomento è il valore del coefficiente da rappresentare */

scrivi_valore(X) :-
    tolleranza_numerica(T),
    Diff is abs(X - round(X)),
    ( Diff < T
    -> R is round(X), write(R)
    ;  Val is round(X * 10000) / 10000, write(Val)
    ).

/* Il predicato grado_polinomio calcola il grado di un polinomio come la
   lunghezza del polinomio normalizzato meno uno, oppure 0 se il polinomio
   è nullo:
   - il primo argomento è la lista dei coefficienti in ordine crescente di grado
   - il secondo argomento è il grado calcolato */

grado_polinomio(Polinomio, Grado) :-
    rimuovi_zeri_in_testa(Polinomio, Poli_norm),
    length(Poli_norm, Lunghezza),
    Lunghezza > 0, !,
    Grado is Lunghezza - 1.
grado_polinomio(_, 0).

/* Il predicato somma calcola la somma di due polinomi in tempo lineare O(n):
   - il primo argomento è la lista dei coefficienti del primo polinomio
   - il secondo argomento è la lista dei coefficienti del secondo polinomio
   - il terzo argomento è la lista dei coefficienti del polinomio somma,
     non normalizzata */

somma([], P2, P2) :- !.
somma(P1, [], P1) :- !.
somma([C1|Resto1], [C2|Resto2], [Somma|Somme_resto]) :-
    Somma is C1 + C2,
    somma(Resto1, Resto2, Somme_resto).

/* Il predicato differenza calcola la differenza tra due polinomi in tempo
   lineare O(n):
   - il primo argomento è la lista dei coefficienti del primo polinomio
   - il secondo argomento è la lista dei coefficienti del secondo polinomio
   - il terzo argomento è la lista dei coefficienti del polinomio
     differenza, non normalizzata */

differenza([], Secondo_poli, Risultato) :- 
            !, moltiplica_per_scalare(Secondo_poli, -1, Risultato).
differenza(Primo_poli, [], Primo_poli) :- !.
differenza([C1|Resto1], [C2|Resto2], [Diff|Diff_resto]) :-
    Diff is C1 - C2,
    differenza(Resto1, Resto2, Diff_resto).

/* Il predicato prodotto calcola il prodotto di due polinomi:
   - il primo argomento è la lista dei coefficienti del primo polinomio
   - il secondo argomento è la lista dei coefficienti del secondo polinomio
   - il terzo argomento è la lista dei coefficienti del polinomio
     prodotto, non normalizzata */

prodotto([], _, []) :- !.
prodotto(_, [], []) :- !.
prodotto([Coeff_testa|Coeff_resto], Secondo_poli, Prodotto) :-
    moltiplica_per_scalare(Secondo_poli, Coeff_testa, Prodotto_testa),
    prodotto(Coeff_resto, Secondo_poli, Prodotto_resto),
    somma(Prodotto_testa, [0.0|Prodotto_resto], Prodotto).

/* Il predicato ausiliario moltiplica_per_scalare moltiplica ogni
   coefficiente di una lista per uno scalare:
   - il primo argomento è la lista dei coefficienti da moltiplicare
   - il secondo argomento è lo scalare per cui moltiplicare ogni coefficiente
   - il terzo argomento è la lista dei coefficienti risultante */

moltiplica_per_scalare([], _, []).
moltiplica_per_scalare([Y|Y_resto], Scalare, [Prod|Prod_resto]) :-
    Prod is Y * Scalare,
    moltiplica_per_scalare(Y_resto, Scalare, Prod_resto).

/* Il predicato gestisci_divisione coordina la divisione euclidea tra due
   polinomi e la stampa del quoziente e del resto, segnalando l'eventuale
   impossibilità di dividere per il polinomio nullo:
   - il primo argomento è la lista dei coefficienti del polinomio dividendo
   - il secondo argomento è la lista dei coefficienti del polinomio divisore */

gestisci_divisione(A, B) :-
    divisione(A, B, Quoziente, Resto), !,
    write('Quoziente:    '), mostra(Quoziente), nl,
    write('Resto:        '), mostra(Resto), nl.
gestisci_divisione(_, _) :-
    write('Errore:       impossibile dividere per il polinomio nullo.'), nl.

/* Il predicato divisione calcola quoziente e resto della divisione
   euclidea tra due polinomi:
   - il primo argomento è la lista dei coefficienti del polinomio dividendo
   - il secondo argomento è la lista dei coefficienti del polinomio divisore
   - il terzo argomento è la lista dei coefficienti del polinomio quoziente
   - il quarto argomento è la lista dei coefficienti del polinomio resto */

divisione(Dividendo, Divisore, Quoziente, Resto) :-
    rimuovi_zeri_in_testa(Divisore, Divisore_normalizzato),
    Divisore_normalizzato \= [], !,
    rimuovi_zeri_in_testa(Dividendo, Dividendo_normalizzato),
    divisione_ricorsiva(Dividendo_normalizzato,
                        Divisore_normalizzato,
                        [],
                        Quoziente,
                        Resto).

/* Il predicato ausiliario divisione_ricorsiva esegue la divisione lunga
   accumulando il quoziente:
   - il primo argomento è la lista dei coefficienti del dividendo corrente
   - il secondo argomento è la lista dei coefficienti del 
     divisore, normalizzata
   - il terzo argomento è la lista dei coefficienti del quoziente parziale 
     accumulato finora
   - il quarto argomento è la lista dei coefficienti del quoziente finale
     risultante
   - il quinto argomento è la lista dei coefficienti del resto
     finale risultante */

divisione_ricorsiva(Resto_corrente,
                    Divisore_normalizzato,
                    Quoziente_parziale,
                    Quoziente,
                    Resto) :-
    length(Resto_corrente, Lungh_resto),
    length(Divisore_normalizzato, Lungh_divisore),
    Lungh_resto < Lungh_divisore, !,
    rimuovi_zeri_in_testa(Quoziente_parziale, Quoziente),
    rimuovi_zeri_in_testa(Resto_corrente, Resto).
divisione_ricorsiva(Resto_corrente,
                    Divisore_normalizzato,
                    Quoziente_parziale,
                    Quoziente,
                    Resto) :-
    last(Resto_corrente, Coefficiente_direttore_dividendo),
    last(Divisore_normalizzato, Coefficiente_direttore_divisore),
    length(Resto_corrente, Lungh_resto),
    length(Divisore_normalizzato, Lungh_divisore),
    Coefficiente_termine is Coefficiente_direttore_dividendo /
                            Coefficiente_direttore_divisore,
    Differenza_grado is Lungh_resto - Lungh_divisore,
    costruisci_monomio(Differenza_grado,
                       Coefficiente_termine,
                       Termine_quoziente),
    prodotto(Divisore_normalizzato, Termine_quoziente, Termine_da_sottrarre),
    differenza(Resto_corrente, Termine_da_sottrarre, Resto_aggiornato),
    somma(Quoziente_parziale, Termine_quoziente, Quoziente_aggiornato),
    rimuovi_zeri_in_testa(Resto_aggiornato, Resto_aggiornato_norm),
    divisione_ricorsiva(Resto_aggiornato_norm,
                        Divisore_normalizzato,
                        Quoziente_aggiornato,
                        Quoziente,
                        Resto).

/* Il predicato ausiliario costruisci_monomio genera una lista densa
   di coefficienti, con zeri nei gradi inferiori, rappresentante un
   unico monomio:
   - il primo argomento è il grado del monomio da costruire
   - il secondo argomento è il coefficiente del monomio
   - il terzo argomento è la lista dei coefficienti risultante,
     rappresentante il monomio */

costruisci_monomio(0, Coefficiente, [Coefficiente]) :- !.
costruisci_monomio(N, Coefficiente, [0.0|Resto]) :-
    N > 0, N_1 is N - 1,
    costruisci_monomio(N_1, Coefficiente, Resto).

/* Il predicato mcd calcola il massimo comun divisore di due polinomi
   tramite l'algoritmo di Euclide, restituendo il risultato reso monico:
   - il primo argomento è la lista dei coefficienti del primo polinomio
   - il secondo argomento è la lista dei coefficienti del secondo polinomio
   - il terzo argomento è la lista dei coefficienti del massimo comun
     divisore risultante */

mcd(Primo_poli, Secondo_poli, Mcd) :-
    rimuovi_zeri_in_testa(Primo_poli, Primo_poli_norm),
    rimuovi_zeri_in_testa(Secondo_poli, Secondo_poli_norm),
    algoritmo_euclide(Primo_poli_norm, Secondo_poli_norm, Mcd).

/* Il predicato ausiliario algoritmo_euclide applica ricorsivamente
   l'algoritmo di Euclide ai due polinomi, tramite rendi_monico, fino a
   ridurre il secondo polinomio al polinomio nullo:
   - il primo argomento è la lista dei coefficienti del primo polinomio
   - il secondo argomento è la lista dei coefficienti del secondo polinomio
   - il terzo argomento è la lista dei coefficienti del massimo comun
     divisore risultante */

algoritmo_euclide(Primo_poli, [], Mcd) :- !, rendi_monico(Primo_poli, Mcd).
algoritmo_euclide(Primo_poli, Secondo_poli, Mcd) :-
    divisione(Primo_poli, Secondo_poli, _, Resto),
    algoritmo_euclide(Secondo_poli, Resto, Mcd).

/* Il predicato ausiliario rendi_monico divide tutti i coefficienti
   per il coefficiente direttore (l'ultimo della lista), tramite
   moltiplica_per_scalare:
   - il primo argomento è la lista dei coefficienti da rendere monica
   - il secondo argomento è la lista dei coefficienti resa monica risultante */

rendi_monico([], []) :- !.
rendi_monico(Coefficienti, Monico) :-
    last(Coefficienti, Coeff_direttore),
    Inverso_coefficiente is 1.0 / Coeff_direttore,
    moltiplica_per_scalare(Coefficienti, Inverso_coefficiente, Monico).