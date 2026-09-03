/* Programma Prolog per operazioni ed algoritmi su polinomi. */

/* La costante tolleranza rappresenta la soglia al di sotto della
   quale un valore reale viene considerato pari a zero, al fine di
   compensare gli errori di arrotondamento tipici dell'aritmetica in
   virgola mobile */

tolleranza_numerica(0.000001).

/* Il predicato main coordina l'intero programma: acquisisce i due
   polinomi, li visualizza, calcola e stampa il grado di ciascuno, la
   loro somma, differenza e prodotto, il quoziente e il resto della
   divisione euclidea (segnalando l'eventuale impossibilità di
   dividere per il polinomio nullo) e il loro massimo comun divisore */

main :-
    acquisisci_polinomio('A', Primo_poli),
    acquisisci_polinomio('B', Secondo_poli),
    nl, nl,
    write('Polinomio A:    '), mostra(Primo_poli), nl,
    write('Polinomio B:    '), mostra(Secondo_poli), nl,
    grado_polinomio(Primo_poli, Grado_a),
    write('Grado A:        '), write(Grado_a), nl,
    grado_polinomio(Secondo_poli, Grado_b),
    write('Grado B:        '), write(Grado_b), nl,
    somma(Primo_poli, Secondo_poli, Somma),
    write('Somma:          '), mostra(Somma), nl,
    differenza(Primo_poli, Secondo_poli, Differenza),
    write('Differenza:     '), mostra(Differenza), nl,
    prodotto(Primo_poli, Secondo_poli, Prodotto),
    write('Prodotto:       '), mostra(Prodotto), nl,
    gestisci_divisione(Primo_poli, Secondo_poli),
    mcd(Primo_poli, Secondo_poli, Mcd),
    write('MCD:            '), mostra(Mcd), nl.

/* Il predicato acquisisci_polinomio acquisisce un polinomio a
   coefficienti reali da tastiera, restituendo la lista dei
   coefficienti in ordine crescente di grado:
   - il primo argomento è l'etichetta (nome) del polinomio da
     acquisire, usata nel messaggio mostrato all'utente
   - il secondo argomento è la lista dei coefficienti acquisita */

acquisisci_polinomio(Etichetta, Polinomio) :-
    write('Inserisci i coefficienti del polinomio '), write(Etichetta),
    write(' separati da spazi (ordine crescente di grado): '),
    leggi_riga(Caratteri),
    interpreta_riga(Etichetta, Caratteri, Polinomio).

/* Il predicato leggi_riga legge da tastiera una riga di testo,
   carattere per carattere, fino al primo fine riga oppure alla fine
   dell'input:
   - il suo unico argomento è la lista dei caratteri letti, oppure
     l'atomo fine_input se l'input è terminato */

leggi_riga(Riga) :-
    get_char(Carattere),
    apri_riga(Carattere, Riga).

/* Il predicato ausiliario apri_riga distingue la fine dell'input dal
   primo carattere di una riga effettiva:
   - il primo argomento è il primo carattere letto
   - il secondo argomento è la riga risultante, ossia la lista dei
     suoi caratteri oppure l'atomo fine_input */

apri_riga(end_of_file, fine_input) :- !.
apri_riga(Carattere, Riga) :-
    accumula_caratteri(Carattere, Riga).

/* Il predicato ausiliario accumula_caratteri costruisce
   ricorsivamente la lista dei caratteri della riga corrente,
   ignorando i ritorni carrello dei file di testo in formato Windows:
   - il primo argomento è il carattere corrente
   - il secondo argomento è la lista dei caratteri risultante */

accumula_caratteri(end_of_file, []) :- !.
accumula_caratteri('\n', []) :- !.
accumula_caratteri('\r', Caratteri) :- !,
    get_char(Successivo),
    accumula_caratteri(Successivo, Caratteri).
accumula_caratteri(Carattere, [Carattere|Resto]) :-
    get_char(Successivo),
    accumula_caratteri(Successivo, Resto).

/* Il predicato ausiliario interpreta_riga termina il programma se
   l'input è esaurito, altrimenti suddivide la riga letta nei token
   che la compongono:
   - il primo argomento è l'etichetta del polinomio, usata per
     richiedere nuovamente l'input in caso di errore
   - il secondo argomento è la riga letta, ossia la lista dei suoi
     caratteri oppure l'atomo fine_input
   - il terzo argomento è la lista dei coefficienti risultante */

interpreta_riga(_, fine_input, _) :- !,
    nl, write('Input terminato (EOF). Uscita dal programma.'), nl,
    halt.
interpreta_riga(Etichetta, Caratteri, Polinomio) :-
    dividi_in_token(Caratteri, Token),
    prosegui_acquisizione(Etichetta, Token, Polinomio).

/* Il predicato dividi_in_token spezza una lista di caratteri sugli
   spazi e sulle tabulazioni, restituendo la lista dei token che la
   compongono (ciascuno a sua volta una lista di caratteri) e
   scartando i token vuoti generati da separatori consecutivi:
   - il primo argomento è la lista dei caratteri da suddividere
   - il secondo argomento è la lista dei token risultante */

dividi_in_token(Caratteri, Token) :-
    dividi_in_token(Caratteri, [], [], Token).

/* Il predicato ausiliario dividi_in_token/4 scandisce ricorsivamente
   la lista dei caratteri, accumulando il token corrente e
   chiudendolo ogni volta che incontra uno spazio o una tabulazione:
   - il primo argomento è la lista dei caratteri residui da scandire
   - il secondo argomento è l'accumulatore (invertito) del token
     corrente
   - il terzo argomento è l'accumulatore (invertito) dei token già
     chiusi
   - il quarto argomento è la lista dei token risultante, in ordine */

dividi_in_token([], Acc_corrente, Acc_token, Token) :-
    aggiungi_token(Acc_corrente, Acc_token, Token_invertiti),
    reverse(Token_invertiti, Token).
dividi_in_token([' '|Resto], Acc_corrente, Acc_token, Token) :- !,
    aggiungi_token(Acc_corrente, Acc_token, Acc_aggiornato),
    dividi_in_token(Resto, [], Acc_aggiornato, Token).
dividi_in_token(['\t'|Resto], Acc_corrente, Acc_token, Token) :- !,
    aggiungi_token(Acc_corrente, Acc_token, Acc_aggiornato),
    dividi_in_token(Resto, [], Acc_aggiornato, Token).
dividi_in_token([Carattere|Resto], Acc_corrente, Acc_token, Token) :-
    dividi_in_token(Resto, [Carattere|Acc_corrente], Acc_token, Token).

/* Il predicato ausiliario aggiungi_token chiude il token corrente e
   lo antepone alla lista dei token già accumulati, ignorando i token
   vuoti generati da separatori consecutivi:
   - il primo argomento è il token corrente (invertito),
     eventualmente vuoto
   - il secondo argomento è la lista (invertita) dei token già chiusi
   - il terzo argomento è la lista (invertita) risultante */

aggiungi_token([], Acc_token, Acc_token) :- !.
aggiungi_token(Acc_corrente, Acc_token, [Token|Acc_token]) :-
    reverse(Acc_corrente, Token).

/* Il predicato ausiliario prosegui_acquisizione ripete l'acquisizione
   se la riga digitata non contiene alcun token, altrimenti avvia la
   conversione dei token in coefficienti:
   - il primo argomento è l'etichetta del polinomio, usata per
     richiedere nuovamente l'input in caso di errore
   - il secondo argomento è la lista dei token estratti dalla riga
   - il terzo argomento è la lista dei coefficienti risultante */

prosegui_acquisizione(Etichetta, [], Polinomio) :- !,
    write('Devi inserire almeno un coefficiente esplicito!'), nl,
    acquisisci_polinomio(Etichetta, Polinomio).
prosegui_acquisizione(Etichetta, Token, Polinomio) :-
    elabora_input(Etichetta, Token, Polinomio).

/* Il predicato elabora_input converte i token in coefficienti
   numerici tramite converti_input, ripetendo l'acquisizione in caso
   di formato non valido:
   - il primo argomento è l'etichetta del polinomio, usata per
     richiedere nuovamente l'input in caso di errore
   - il secondo argomento è la lista dei token da convertire
   - il terzo argomento è la lista dei coefficienti risultante */

elabora_input(_, Token, Polinomio) :-
    converti_input(Token, Coefficienti), !,
    rimuovi_zeri_grado_massimo(Coefficienti, Polinomio).
elabora_input(Etichetta, _, Polinomio) :-
    write('Formato non valido! Riprova.'), nl,
    acquisisci_polinomio(Etichetta, Polinomio).

/* Il predicato converti_input converte i token in coefficienti
   numerici, fallendo se anche un solo token non rappresenta un
   numero:
   - il primo argomento è la lista dei token da convertire
   - il secondo argomento è la lista dei coefficienti risultante */

converti_input(Token, Coefficienti) :-
    converti_input(Token, [], Coefficienti_invertiti),
    reverse(Coefficienti_invertiti, Coefficienti).

/* Il predicato ausiliario converti_input/3 converte ricorsivamente
   la lista dei token in coefficienti, accumulando il risultato in
   ordine inverso:
   - il primo argomento è la lista dei token residui da convertire
   - il secondo argomento è l'accumulatore dei coefficienti
     convertiti finora
   - il terzo argomento è la lista dei coefficienti risultante */

converti_input([], Acc, Acc).
converti_input([Token|Resto], Acc, Coefficienti) :-
    aggiungi_punto_decimale(Token, Token_reale),
    catch(number_chars(Numero, Token_reale), _, fail),
    Coefficiente is float(Numero),
    converti_input(Resto, [Coefficiente|Acc], Coefficienti).

/* Il predicato ausiliario aggiungi_punto_decimale porta un token
   numerico nella forma sintattica di un numero reale, inserendo la
   parte decimale nulla prima dell'eventuale esponente oppure, in sua
   assenza, in coda al token; un token che contiene già il punto
   decimale viene lasciato invariato:
   - il primo argomento è la lista dei caratteri del token originale
   - il secondo argomento è la lista dei caratteri risultante */

aggiungi_punto_decimale(Token, Token) :-
    member('.', Token), !.
aggiungi_punto_decimale(Token, Token_reale) :-
    append(Mantissa, [Carattere|Cifre], Token),
    esponente(Carattere), !,
    append(Mantissa, ['.', '0', Carattere|Cifre], Token_reale).
aggiungi_punto_decimale(Token, Token_reale) :-
    append(Token, ['.', '0'], Token_reale).

/* Il predicato ausiliario esponente riconosce i caratteri che
   introducono l'esponente nella notazione scientifica:
   - il suo unico argomento è il carattere da riconoscere */

esponente(e).
esponente('E').

/* Il predicato rimuovi_zeri_grado_massimo elimina i coefficienti
   nulli di grado massimo di un polinomio, ossia quelli che si
   trovano in coda alla lista:
   - il primo argomento è la lista dei coefficienti in ordine
     crescente di grado
   - il secondo argomento è la lista dei coefficienti risultante */

rimuovi_zeri_grado_massimo(Polinomio, Polinomio_norm) :-
    reverse(Polinomio, Invertito),
    rimuovi_zeri_da_lista_invertita(Invertito, Invertito_ripulito),
    reverse(Invertito_ripulito, Polinomio_norm).

/* Il predicato ausiliario rimuovi_zeri_da_lista_invertita elimina
   ricorsivamente i coefficienti di testa (lista invertita) il cui
   valore assoluto è sotto tolleranza:
   - il primo argomento è la lista dei coefficienti in ordine
     invertito (dal grado massimo al minimo)
   - il secondo argomento è la lista risultante, priva dei
     coefficienti di testa sotto tolleranza */

rimuovi_zeri_da_lista_invertita([X|Resto], Risultato) :-
    tolleranza_numerica(T), abs(X) < T, !,
    rimuovi_zeri_da_lista_invertita(Resto, Risultato).
rimuovi_zeri_da_lista_invertita(Lista, Lista).

/* Il predicato mostra stampa la rappresentazione algebrica di un
   polinomio, dal grado massimo al minimo:
   - il suo unico argomento è la lista dei coefficienti del polinomio
     da stampare, in ordine crescente di grado */

mostra(Polinomio) :-
    rimuovi_zeri_grado_massimo(Polinomio, Poli_norm),
    costruisci_coppie_grado_coeff(Poli_norm, Coppie_invertite),
    formatta_termini(Coppie_invertite, primo).

/* Il predicato ausiliario costruisci_coppie_grado_coeff costruisce,
   tramite accumula_coppie, le coppie (grado, coefficiente) ordinate
   dal grado massimo al minimo:
   - il primo argomento è la lista dei coefficienti in ordine
     crescente di grado
   - il secondo argomento è la lista delle coppie risultante */

costruisci_coppie_grado_coeff(Polinomio, Coppie_invertite) :-
    accumula_coppie(Polinomio, 0, Coppie),
    reverse(Coppie, Coppie_invertite).

/* Il predicato ausiliario accumula_coppie associa ad ogni
   coefficiente il proprio grado, percorrendo il polinomio in ordine
   crescente a partire dal grado 0:
   - il primo argomento è la lista dei coefficienti residui da
     associare al grado
   - il secondo argomento è il grado corrente, incrementato a ogni
     passo della ricorsione
   - il terzo argomento è la lista delle coppie risultante */

accumula_coppie([], _, []).
accumula_coppie([Coeff|Resto], Grado, [(Grado, Coeff)|Coppie_resto]) :-
    Grado_succ is Grado + 1,
    accumula_coppie(Resto, Grado_succ, Coppie_resto).

/* Il predicato ausiliario formatta_termini stampa ricorsivamente i
   termini di un polinomio, dal grado massimo al minimo, saltando i
   coefficienti nulli:
   - il primo argomento è la lista delle coppie (grado,
     coefficiente) residue da stampare, ordinata dal grado massimo al
     minimo
   - il secondo argomento è l'atomo primo se il termine da stampare è
     il primo termine non nullo del polinomio, l'atomo successivo
     altrimenti */

formatta_termini([], primo) :- !, write('0').
formatta_termini([], successivo) :- !.
formatta_termini([(_, Coefficiente)|Resto], Posizione) :-
    tolleranza_numerica(T), abs(Coefficiente) < T, !,
    formatta_termini(Resto, Posizione).
formatta_termini([(Grado, Coefficiente)|Resto], Posizione) :-
    formatta_segno(Coefficiente, Posizione),
    Valore_assoluto is abs(Coefficiente),
    formatta_monomio(Grado, Valore_assoluto),
    formatta_termini(Resto, successivo).

/* Il predicato ausiliario formatta_segno stampa il segno da
   anteporre al termine corrente:
   - il primo argomento è il coefficiente del termine corrente
   - il secondo argomento è l'atomo primo se il termine è il primo
     termine non nullo del polinomio, l'atomo successivo altrimenti */

formatta_segno(Coefficiente, primo) :- Coefficiente < 0, !, write('-').
formatta_segno(_, primo) :- !.
formatta_segno(Coefficiente, successivo) :-
    Coefficiente < 0, !, write(' - ').
formatta_segno(_, successivo) :- write(' + ').

/* Il predicato ausiliario formatta_monomio stampa grado e
   coefficiente di un monomio, omettendo i coefficienti unitari e le
   potenze 0 e 1:
   - il primo argomento è il grado del monomio
   - il secondo argomento è il valore assoluto del coefficiente del
     monomio */

formatta_monomio(0, Coefficiente) :- !, scrivi_valore(Coefficiente).
formatta_monomio(1, Coefficiente) :-
    tolleranza_numerica(T), abs(Coefficiente - 1.0) < T, !, write('x').
formatta_monomio(1, Coefficiente) :- !,
    scrivi_valore(Coefficiente), write('x').
formatta_monomio(Grado, Coefficiente) :-
    tolleranza_numerica(T), abs(Coefficiente - 1.0) < T, !,
    write('x^'), write(Grado).
formatta_monomio(Grado, Coefficiente) :-
    scrivi_valore(Coefficiente), write('x^'), write(Grado).

/* Il predicato ausiliario scrivi_valore stampa un coefficiente non
   negativo come intero se la sua parte decimale è trascurabile,
   altrimenti arrotondato a quattro cifre decimali e privato degli
   zeri finali; i valori di modulo troppo grande per essere
   arrotondati a intero vengono stampati nella forma nativa:
   - il suo unico argomento è il valore del coefficiente da stampare */

scrivi_valore(Coefficiente) :-
    abs(Coefficiente) > 1.0e14, !, write(Coefficiente).
scrivi_valore(Coefficiente) :-
    tolleranza_numerica(T),
    Arrotondato is round(Coefficiente),
    abs(Coefficiente - Arrotondato) < T, !,
    write(Arrotondato).
scrivi_valore(Coefficiente) :-
    Parte_intera is truncate(Coefficiente),
    Frazione is Coefficiente - Parte_intera,
    Parte_frazionaria is round(Frazione * 10000),
    scrivi_parti(Parte_intera, Parte_frazionaria).

/* Il predicato ausiliario scrivi_parti stampa la parte intera e, se
   significativa, la parte frazionaria di un coefficiente, gestendo
   il riporto prodotto dall'arrotondamento alla quarta cifra
   decimale:
   - il primo argomento è la parte intera del coefficiente
   - il secondo argomento è la parte frazionaria arrotondata,
     espressa come intero su quattro cifre */

scrivi_parti(Parte_intera, 10000) :- !,
    Parte_intera_succ is Parte_intera + 1,
    write(Parte_intera_succ).
scrivi_parti(Parte_intera, 0) :- !,
    write(Parte_intera).
scrivi_parti(Parte_intera, Parte_frazionaria) :-
    write(Parte_intera), write('.'),
    scrivi_cifre_frazionarie(Parte_frazionaria, 1000).

/* Il predicato ausiliario scrivi_cifre_frazionarie stampa le cifre
   della parte frazionaria, dalla più significativa alla meno
   significativa, arrestandosi quando le cifre residue sono tutte
   nulle:
   - il primo argomento è la parte frazionaria residua, espressa come
     intero su quattro cifre
   - il secondo argomento è il peso della cifra da stampare */

scrivi_cifre_frazionarie(0, _) :- !.
scrivi_cifre_frazionarie(Parte_frazionaria, Peso) :-
    Cifra is Parte_frazionaria // Peso,
    Resto is Parte_frazionaria mod Peso,
    Peso_successivo is Peso // 10,
    write(Cifra),
    scrivi_cifre_frazionarie(Resto, Peso_successivo).

/* Il predicato grado_polinomio calcola il grado di un polinomio come
   la lunghezza del polinomio normalizzato meno uno, oppure 0 se il
   polinomio è nullo:
   - il primo argomento è la lista dei coefficienti in ordine
     crescente di grado
   - il secondo argomento è il grado calcolato */

grado_polinomio(Polinomio, Grado) :-
    rimuovi_zeri_grado_massimo(Polinomio, Poli_norm),
    length(Poli_norm, Lunghezza),
    Lunghezza > 0, !,
    Grado is Lunghezza - 1.
grado_polinomio(_, 0).

/* Il predicato somma calcola la somma di due polinomi in tempo
   lineare O(n):
   - il primo argomento è la lista dei coefficienti del primo
     polinomio
   - il secondo argomento è la lista dei coefficienti del secondo
     polinomio
   - il terzo argomento è la lista dei coefficienti del polinomio
     somma, non normalizzata */

somma([], Secondo_poli, Secondo_poli) :- !.
somma(Primo_poli, [], Primo_poli) :- !.
somma([C1|Resto1], [C2|Resto2], [Somma|Somme_resto]) :-
    Somma is C1 + C2,
    somma(Resto1, Resto2, Somme_resto).

/* Il predicato differenza calcola la differenza tra due polinomi in
   tempo lineare O(n):
   - il primo argomento è la lista dei coefficienti del primo
     polinomio
   - il secondo argomento è la lista dei coefficienti del secondo
     polinomio
   - il terzo argomento è la lista dei coefficienti del polinomio
     differenza, non normalizzata */

differenza([], Secondo_poli, Risultato) :- !,
    moltiplica_per_scalare(Secondo_poli, -1, Risultato).
differenza(Primo_poli, [], Primo_poli) :- !.
differenza([C1|Resto1], [C2|Resto2], [Diff|Diff_resto]) :-
    Diff is C1 - C2,
    differenza(Resto1, Resto2, Diff_resto).

/* Il predicato ausiliario moltiplica_per_scalare moltiplica ogni
   coefficiente di una lista per uno scalare:
   - il primo argomento è la lista dei coefficienti da moltiplicare
   - il secondo argomento è lo scalare per cui moltiplicare ogni
     coefficiente
   - il terzo argomento è la lista dei coefficienti risultante */

moltiplica_per_scalare([], _, []).
moltiplica_per_scalare([Y|Y_resto], Scalare, [Prod|Prod_resto]) :-
    Prod is Y * Scalare,
    moltiplica_per_scalare(Y_resto, Scalare, Prod_resto).

/* Il predicato prodotto calcola il prodotto di due polinomi:
   - il primo argomento è la lista dei coefficienti del primo
     polinomio
   - il secondo argomento è la lista dei coefficienti del secondo
     polinomio
   - il terzo argomento è la lista dei coefficienti del polinomio
     prodotto, non normalizzata */

prodotto([], _, []) :- !.
prodotto(_, [], []) :- !.
prodotto([Coeff_testa|Coeff_resto], Secondo_poli, Prodotto) :-
    moltiplica_per_scalare(Secondo_poli, Coeff_testa, Prodotto_testa),
    prodotto(Coeff_resto, Secondo_poli, Prodotto_resto),
    somma(Prodotto_testa, [0.0|Prodotto_resto], Prodotto).

/* Il predicato gestisci_divisione coordina la divisione euclidea tra
   due polinomi e la stampa del quoziente e del resto, segnalando
   l'impossibilità di dividere per il polinomio nullo, caso nel quale
   il predicato divisione fallisce:
   - il primo argomento è la lista dei coefficienti del polinomio
     dividendo
   - il secondo argomento è la lista dei coefficienti del polinomio
     divisore */

gestisci_divisione(Dividendo, Divisore) :-
    divisione(Dividendo, Divisore, Quoziente, Resto), !,
    write('Quoziente:      '), mostra(Quoziente), nl,
    write('Resto:          '), mostra(Resto), nl.
gestisci_divisione(_, _) :-
    write('Errore:         impossibile dividere per '),
    write('il polinomio nullo.'), nl.

/* Il predicato divisione calcola quoziente e resto della divisione
   euclidea tra due polinomi, fallendo se il divisore è il polinomio
   nullo:
   - il primo argomento è la lista dei coefficienti del polinomio
     dividendo
   - il secondo argomento è la lista dei coefficienti del polinomio
     divisore
   - il terzo argomento è la lista dei coefficienti del polinomio
     quoziente
   - il quarto argomento è la lista dei coefficienti del polinomio
     resto */

divisione(Dividendo, Divisore, Quoziente, Resto) :-
    rimuovi_zeri_grado_massimo(Divisore, Divisore_normalizzato),
    Divisore_normalizzato \== [], !,
    rimuovi_zeri_grado_massimo(Dividendo, Dividendo_normalizzato),
    divisione_ricorsiva(Dividendo_normalizzato,
                        Divisore_normalizzato,
                        [],
                        Quoziente,
                        Resto).

/* Il predicato ausiliario divisione_ricorsiva esegue la divisione
   lunga accumulando il quoziente:
   - il primo argomento è la lista dei coefficienti del dividendo
     corrente, normalizzata
   - il secondo argomento è la lista dei coefficienti del divisore,
     normalizzata
   - il terzo argomento è la lista dei coefficienti del quoziente
     parziale accumulato finora
   - il quarto argomento è la lista dei coefficienti del quoziente
     finale risultante
   - il quinto argomento è la lista dei coefficienti del resto finale
     risultante */

divisione_ricorsiva(Resto_corrente,
                    Divisore_normalizzato,
                    Quoziente_parziale,
                    Quoziente,
                    Resto) :-
    length(Resto_corrente, Lungh_resto),
    length(Divisore_normalizzato, Lungh_divisore),
    Lungh_resto < Lungh_divisore,
    !,
    rimuovi_zeri_grado_massimo(Quoziente_parziale, Quoziente),
    rimuovi_zeri_grado_massimo(Resto_corrente, Resto).
divisione_ricorsiva(Resto_corrente,
                    Divisore_normalizzato,
                    Quoziente_parziale,
                    Quoziente,
                    Resto) :-
    length(Resto_corrente, Lungh_resto),
    length(Divisore_normalizzato, Lungh_divisore),
    Lungh_resto >= Lungh_divisore,
    !,
    last(Resto_corrente, Direttore_dividendo),
    last(Divisore_normalizzato, Direttore_divisore),
    Coefficiente_termine is float(Direttore_dividendo) /
                            float(Direttore_divisore),
    Differenza_grado is Lungh_resto - Lungh_divisore,
    costruisci_monomio(Differenza_grado,
                       Coefficiente_termine,
                       Termine_quoziente),
    prodotto(Divisore_normalizzato,
             Termine_quoziente,
             Termine_da_sottrarre),
    differenza(Resto_corrente, Termine_da_sottrarre, Resto_aggiornato),
    somma(Quoziente_parziale, Termine_quoziente, Quoziente_aggiornato),
    rimuovi_zeri_grado_massimo(Resto_aggiornato, Resto_aggiornato_norm),
    divisione_ricorsiva(Resto_aggiornato_norm,
                        Divisore_normalizzato,
                        Quoziente_aggiornato,
                        Quoziente,
                        Resto).

/* Il predicato ausiliario costruisci_monomio genera una lista di
   coefficienti, con zeri nei gradi inferiori, rappresentante un
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
   tramite l'algoritmo di Euclide, restituendo il risultato reso
   monico:
   - il primo argomento è la lista dei coefficienti del primo
     polinomio
   - il secondo argomento è la lista dei coefficienti del secondo
     polinomio
   - il terzo argomento è la lista dei coefficienti del massimo comun
     divisore risultante */

mcd(Primo_poli, Secondo_poli, Mcd) :-
    rimuovi_zeri_grado_massimo(Primo_poli, Primo_poli_norm),
    rimuovi_zeri_grado_massimo(Secondo_poli, Secondo_poli_norm),
    algoritmo_euclide(Primo_poli_norm, Secondo_poli_norm, Mcd).

/* Il predicato ausiliario algoritmo_euclide applica ricorsivamente
   l'algoritmo di Euclide ai due polinomi, tramite rendi_monico, fino
   a ridurre il secondo polinomio al polinomio nullo:
   - il primo argomento è la lista dei coefficienti del primo
     polinomio, normalizzata
   - il secondo argomento è la lista dei coefficienti del secondo
     polinomio, normalizzata
   - il terzo argomento è la lista dei coefficienti del massimo comun
     divisore risultante */

algoritmo_euclide(Primo_poli, [], Mcd) :- !,
    rendi_monico(Primo_poli, Mcd).
algoritmo_euclide(Primo_poli, Secondo_poli, Mcd) :-
    divisione(Primo_poli, Secondo_poli, _, Resto),
    rimuovi_zeri_grado_massimo(Resto, Resto_norm),
    algoritmo_euclide(Secondo_poli, Resto_norm, Mcd).

/* Il predicato ausiliario rendi_monico divide tutti i coefficienti
   di un polinomio per il suo coefficiente direttore (l'ultimo della
   lista), tramite moltiplica_per_scalare:
   - il primo argomento è la lista dei coefficienti da rendere monica
   - il secondo argomento è la lista dei coefficienti resa monica
     risultante */

rendi_monico(Coefficienti, Monico) :-
    rimuovi_zeri_grado_massimo(Coefficienti, Normalizzati),
    rendi_monico_normalizzato(Normalizzati, Monico).

/* Il predicato ausiliario rendi_monico_normalizzato distingue il
   caso del polinomio nullo, per il quale il risultato è il polinomio
   nullo stesso, dal caso generale:
   - il primo argomento è la lista dei coefficienti da rendere
     monica, già normalizzata
   - il secondo argomento è la lista dei coefficienti resa monica
     risultante */

rendi_monico_normalizzato([], []) :- !.
rendi_monico_normalizzato(Coefficienti, Monico) :-
    last(Coefficienti, Coeff_direttore),
    Inverso_coefficiente is 1.0 / Coeff_direttore,
    moltiplica_per_scalare(Coefficienti, Inverso_coefficiente, Monico).
