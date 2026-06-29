/* Programma Prolog per operazioni ed algoritmi avanzati su polinomi. */

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

tolleranzaNumerica(1e-6).

/* Il predicato rimuoviZeriInTesta normalizza un polinomio eliminando i coefficienti nulli
   a partire dal grado massimo verso il basso:
   - il suo unico argomento è la lista dei coefficienti del polinomio in ordine crescente di grado. */
rimuoviZeriInTesta(Polinomio, PolinomioNorm) :-
    reverse(Polinomio, Invertito),
    rimuoviZeriDaListaInvertita(Invertito, InvertitoRipulito),
    reverse(InvertitoRipulito, PolinomioNorm).

/* Il predicato rimuoviZeriDaListaInvertita elimina ricorsivamente i valori vicini allo zero
   dall'inizio della lista invertita dei coefficienti:
   - il primo argomento è la lista da cui eliminare gli zeri;
   - il secondo argomento è la lista risultante priva degli zeri iniziali.

   Caso base: la testa della lista è prossima a zero; il taglio blocca il backtracking e
   si procede ricorsivamente sul resto.
   Caso generale: la testa non è prossima a zero; la lista viene restituita invariata per
   unificazione con il secondo argomento. */
rimuoviZeriDaListaInvertita([X|Resto], Risultato) :-
    tolleranzaNumerica(T), abs(X) < T, !,
    rimuoviZeriDaListaInvertita(Resto, Risultato).
rimuoviZeriDaListaInvertita(Lista, Lista).

/* Il predicato gradoPolinomio calcola il grado di un polinomio:
   - il primo argomento è la lista dei coefficienti del polinomio in ordine crescente di grado;
   - il secondo argomento è il grado del polinomio.

   Caso base con lista vuota: il grado è zero.
   Caso generale: si normalizza il polinomio, si calcola la lunghezza della lista risultante
   e il grado viene unificato con tale lunghezza decrementata di uno. */
gradoPolinomio([], 0) :- !.
gradoPolinomio(Polinomio, Grado) :-
    rimuoviZeriInTesta(Polinomio, PoliNorm),
    length(PoliNorm, Lunghezza),
    Lunghezza > 0, !,
    Grado is Lunghezza - 1.
gradoPolinomio(_, 0).

/* Il predicato scriviValore stampa un valore numerico come intero se prossimo a un intero,
   altrimenti arrotondato a quattro cifre decimali:
   - il suo unico argomento è il valore numerico da stampare. */
scriviValore(X) :-
    tolleranzaNumerica(T),
    Diff is abs(X - round(X)),
    scriviValoreConPrecisione(Diff, T, X).

scriviValoreConPrecisione(Diff, T, X) :- Diff < T, !, R is round(X), write(R).
scriviValoreConPrecisione(_, _, X) :- Val is round(X * 10000) / 10000, write(Val).

/* Il predicato mostraPolinomio stampa la rappresentazione algebrica canonica di un polinomio,
   dal termine di grado massimo a quello di grado minimo:
   - il suo unico argomento è la lista dei coefficienti del polinomio in ordine crescente di grado. */
mostraPolinomio(Polinomio) :-
    rimuoviZeriInTesta(Polinomio, PoliNorm),
    costruisciCoppieGradoCoeff(PoliNorm, CoppieInvertite),
    formattaTermini(CoppieInvertite, 1).

/* Il predicato formattaTermini elabora ricorsivamente le coppie (grado, coefficiente) di un polinomio
   stampandone la rappresentazione algebrica:
   - il primo argomento è la lista delle coppie (grado, coefficiente) in ordine decrescente di grado;
   - il secondo argomento indica se il termine da elaborare è il primo della rappresentazione.

   Caso base con lista vuota e primo termine: viene stampata la stringa "0".
   Caso base con lista vuota: la stampa termina.
   Caso ricorsivo con coefficiente nullo: il termine viene saltato e si procede sul resto.
   Caso ricorsivo generale: viene stampato il segno tramite formattaSegno e il monomio
   tramite formattaMonomio, poi si procede ricorsivamente sul resto della lista. */
formattaTermini([], 1) :- !, write('0').
formattaTermini([], 0) :- !.
formattaTermini([(_, Coefficiente)|Resto], EPrimoTermine) :-
    tolleranzaNumerica(T), abs(Coefficiente) < T, !,
    formattaTermini(Resto, EPrimoTermine).
formattaTermini([(Grado, Coefficiente)|Resto], EPrimoTermine) :-
    formattaSegno(Coefficiente, EPrimoTermine),
    ValoreAssoluto is abs(Coefficiente),
    formattaMonomio(Grado, ValoreAssoluto),
    formattaTermini(Resto, 0).

/* Il predicato costruisciCoppieGradoCoeff costruisce la lista delle coppie (grado, coefficiente)
   associate ai termini di un polinomio, ordinata dal grado massimo al grado minimo:
   - il primo argomento è la lista dei coefficienti del polinomio in ordine crescente di grado;
   - il secondo argomento è la lista delle coppie (grado, coefficiente) in ordine decrescente di grado. */
costruisciCoppieGradoCoeff(Polinomio, CoppieInvertite) :-
    accumulaCoppie(Polinomio, 0, Coppie),
    reverse(Coppie, CoppieInvertite).

/* Il predicato accumulaCoppie costruisce ricorsivamente la lista delle coppie (grado, coefficiente):
   - il primo argomento è la lista dei coefficienti del polinomio in ordine crescente di grado;
   - il secondo argomento è il grado corrente;
   - il terzo argomento è la lista delle coppie (grado, coefficiente) risultante.

   Caso base con lista vuota: viene restituita la lista vuota.
   Caso generale: si costruisce la coppia con il grado corrente e la testa della lista,
   si incrementa il grado e si procede ricorsivamente sulla coda. */
accumulaCoppie([], _, []).
accumulaCoppie([Coeff|Resto], Grado, [(Grado, Coeff)|CoppieResto]) :-
    GradoSucc is Grado + 1,
    accumulaCoppie(Resto, GradoSucc, CoppieResto).

/* Il predicato formattaSegno stampa il segno da anteporre a un termine del polinomio:
   - il primo argomento è il valore del coefficiente del termine;
   - il secondo argomento indica se il termine è il primo della rappresentazione.

   Caso con primo termine negativo: viene stampato "-".
   Caso con primo termine positivo: non viene stampato nulla.
   Caso con termine successivo negativo: viene stampato " - ".
   Caso con termine successivo positivo: viene stampato " + ". */
formattaSegno(Coefficiente, 1) :- Coefficiente < 0, !, write('-').
formattaSegno(_, 1) :- !.
formattaSegno(Coefficiente, 0) :- Coefficiente < 0, !, write(' - ').
formattaSegno(_, 0) :- write(' + ').

/* Il predicato formattaMonomio stampa la rappresentazione testuale di un monomio,
   omettendo i coefficienti unitari e le potenze di esponente zero o uno:
   - il primo argomento è il grado del monomio;
   - il secondo argomento è il valore assoluto del coefficiente del monomio.

   Caso base con grado zero: viene stampato il solo valore del coefficiente.
   Caso con grado uno e coefficiente unitario: viene stampato "x".
   Caso con grado uno: viene stampato il coefficiente seguito da "x".
   Caso con grado superiore e coefficiente unitario: viene stampato "x^grado".
   Caso generale: viene stampato il coefficiente seguito da "x^grado". */
formattaMonomio(0, Coefficiente) :- !, scriviValore(Coefficiente).
formattaMonomio(1, Coefficiente) :-
    tolleranzaNumerica(T), abs(Coefficiente - 1.0) < T, !, write('x').
formattaMonomio(1, Coefficiente) :- !, scriviValore(Coefficiente), write('x').
formattaMonomio(Grado, Coefficiente) :-
    tolleranzaNumerica(T), abs(Coefficiente - 1.0) < T, !, write('x^'), write(Grado).
formattaMonomio(Grado, Coefficiente) :-
    scriviValore(Coefficiente), write('x^'), write(Grado).

/* Il predicato sommaPolinomi calcola la somma di due polinomi:
   - il primo argomento è il primo dei due polinomi;
   - il secondo argomento è il secondo dei due polinomi;
   - il terzo argomento è il polinomio somma.

   Caso base con primo polinomio vuoto: il secondo polinomio normalizzato viene unificato col risultato.
   Caso base con secondo polinomio vuoto: il primo polinomio normalizzato viene unificato col risultato.
   Caso generale: le teste vengono sommate e si procede ricorsivamente sulle code. */
sommaPolinomi([], SecondoPoli, Risultato) :- !, rimuoviZeriInTesta(SecondoPoli, Risultato).
sommaPolinomi(PrimoPoli, [], Risultato) :- !, rimuoviZeriInTesta(PrimoPoli, Risultato).
sommaPolinomi([C1|Resto1], [C2|Resto2], [Somma|SommeResto]) :-
    Somma is C1 + C2,
    sommaPolinomi(Resto1, Resto2, SommeResto).

/* Il predicato sottraiPolinomi calcola la differenza tra due polinomi:
   - il primo argomento è il primo dei due polinomi;
   - il secondo argomento è il secondo dei due polinomi;
   - il terzo argomento è il polinomio differenza.

   Caso base con primo polinomio vuoto: i coefficienti del secondo vengono negati tramite
   negaCoefficienti e unificati col risultato.
   Caso base con secondo polinomio vuoto: il primo polinomio normalizzato viene unificato col risultato.
   Caso generale: le teste vengono sottratte e si procede ricorsivamente sulle code. */
sottraiPolinomi([], SecondoPoli, Risultato) :- !, negaCoefficienti(SecondoPoli, Risultato).
sottraiPolinomi(PrimoPoli, [], Risultato) :- !, rimuoviZeriInTesta(PrimoPoli, Risultato).
sottraiPolinomi([C1|Resto1], [C2|Resto2], [Diff|DiffResto]) :-
    Diff is C1 - C2,
    sottraiPolinomi(Resto1, Resto2, DiffResto).

/* Il predicato negaCoefficienti calcola l'opposto di ogni coefficiente di un polinomio:
   - il primo argomento è la lista dei coefficienti del polinomio;
   - il secondo argomento è la lista dei coefficienti opposti.

   Caso base con lista vuota: viene restituita la lista vuota.
   Caso generale: la testa viene negata e si procede ricorsivamente sulla coda. */
negaCoefficienti([], []).
negaCoefficienti([X|Resto], [NegX|RestoNeg]) :- NegX is -X, negaCoefficienti(Resto, RestoNeg).

/* Il predicato moltiplPolinomi calcola il prodotto di due polinomi per distribuzione:
   - il primo argomento è il primo dei due polinomi;
   - il secondo argomento è il secondo dei due polinomi;
   - il terzo argomento è il polinomio prodotto.

   Caso base con primo polinomio vuoto: il risultato è la lista vuota.
   Caso base con secondo polinomio vuoto: il risultato è la lista vuota.
   Caso generale: si moltiplica la testa del primo polinomio per il secondo tramite
   moltiplicaPerScalare, poi si somma il risultato al prodotto della coda del primo
   per il secondo, scalato di un grado tramite la prepend di uno zero. */
moltiplPolinomi([], _, []) :- !.
moltiplPolinomi(_, [], []) :- !.
moltiplPolinomi([CoeffTesta|CoeffResto], SecondoPoli, Prodotto) :-
    moltiplicaPerScalare(SecondoPoli, CoeffTesta, ProdottoTesta),
    moltiplPolinomi(CoeffResto, SecondoPoli, ProdottoResto),
    sommaPolinomi(ProdottoTesta, [0.0|ProdottoResto], Prodotto).

/* Il predicato moltiplicaPerScalare moltiplica ogni coefficiente di un polinomio per uno scalare:
   - il primo argomento è la lista dei coefficienti del polinomio;
   - il secondo argomento è il valore dello scalare;
   - il terzo argomento è la lista dei coefficienti risultanti.

   Caso base con lista vuota: viene restituita la lista vuota.
   Caso generale: la testa viene moltiplicata per lo scalare e si procede ricorsivamente sulla coda. */
moltiplicaPerScalare([], _, []).
moltiplicaPerScalare([Y|YResto], Scalare, [Prod|ProdResto]) :-
    Prod is Y * Scalare,
    moltiplicaPerScalare(YResto, Scalare, ProdResto).

/* Il predicato divisioneConResto calcola il quoziente e il resto della divisione euclidea tra due polinomi:
   - il primo argomento è il polinomio dividendo;
   - il secondo argomento è il polinomio divisore;
   - il terzo argomento è il polinomio quoziente;
   - il quarto argomento è il polinomio resto. */
divisioneConResto(Dividendo, Divisore, Quoziente, Resto) :-
    rimuoviZeriInTesta(Dividendo, DividendoNorm),
    rimuoviZeriInTesta(Divisore, DivisoreNorm),
    passoDiv(DividendoNorm, DivisoreNorm, [], Quoziente, Resto).

/* Il predicato passoDiv esegue ricorsivamente la divisione lunga tra polinomi, accumulando il quoziente:
   - il primo argomento è il polinomio dividendo corrente;
   - il secondo argomento è il polinomio divisore;
   - il terzo argomento è il quoziente parziale accumulato fino al passo corrente;
   - il quarto argomento è il polinomio quoziente;
   - il quinto argomento è il polinomio resto.

   Caso base: il grado del dividendo corrente è inferiore a quello del divisore; il taglio
   blocca il backtracking e il quoziente parziale e il dividendo corrente vengono unificati
   rispettivamente con il quoziente e il resto.
   Caso generale: si calcola il termine del quoziente dividendo i coefficienti direttori,
   si sottrae dal dividendo corrente il prodotto del divisore per tale termine e si procede
   ricorsivamente con il dividendo ridotto e il quoziente aggiornato. */
passoDiv(DividendoCorrente, Divisore, QuozienteParziale, Quoziente, Resto) :-
    length(DividendoCorrente, LunghDividendo),
    length(Divisore, LunghDivisore),
    LunghDividendo < LunghDivisore, !,
    rimuoviZeriInTesta(QuozienteParziale, Quoziente),
    rimuoviZeriInTesta(DividendoCorrente, Resto).

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

/* Il predicato costruisciMonomio costruisce un monomio come lista densa di coefficienti,
   con zeri nelle posizioni di grado inferiore:
   - il primo argomento è il grado del monomio;
   - il secondo argomento è il coefficiente del monomio;
   - il terzo argomento è la lista densa risultante.

   Caso base con grado zero: viene restituita la lista contenente il solo coefficiente.
   Caso generale: viene preposta una posizione nulla e si procede ricorsivamente
   decrementando il grado. */
costruisciMonomio(0, Coefficiente, [Coefficiente]) :- !.
costruisciMonomio(N, Coefficiente, [0.0|Resto]) :-
    N > 0, N1 is N - 1,
    costruisciMonomio(N1, Coefficiente, Resto).

/* Il predicato calcolaMCD calcola il massimo comun divisore di due polinomi:
   - il primo argomento è il primo dei due polinomi;
   - il secondo argomento è il secondo dei due polinomi;
   - il terzo argomento è il massimo comun divisore. */
calcolaMCD(PoliA, PoliB, MCD) :-
    rimuoviZeriInTesta(PoliA, PoliANorm),
    rimuoviZeriInTesta(PoliB, PoliBNorm),
    algoritmoEuclide(PoliANorm, PoliBNorm, MCD).

/* Il predicato algoritmoEuclide calcola il massimo comun divisore di due polinomi
   tramite l'algoritmo di Euclide, restituendo il risultato reso monico:
   - il primo argomento è il primo dei due polinomi;
   - il secondo argomento è il secondo dei due polinomi;
   - il terzo argomento è il massimo comun divisore.

   Caso base: il secondo polinomio è vuoto; il taglio blocca il backtracking e il primo
   polinomio viene reso monico e unificato col risultato.
   Caso generale: si sostituisce la coppia (A, B) con (B, resto della divisione di A per B)
   e si procede ricorsivamente. */
algoritmoEuclide(PoliA, [], MCD) :- !, rendiMonico(PoliA, MCD).
algoritmoEuclide(PoliA, PoliB, MCD) :-
    divisioneConResto(PoliA, PoliB, _, Resto),
    algoritmoEuclide(PoliB, Resto, MCD).

/* Il predicato rendiMonico divide tutti i coefficienti di un polinomio per il suo coefficiente direttore:
   - il primo argomento è la lista dei coefficienti del polinomio in ordine crescente di grado;
   - il secondo argomento è la lista dei coefficienti del polinomio monico risultante.

   Caso base con lista vuota: viene restituita la lista vuota.
   Caso generale: ogni coefficiente viene diviso per l'ultimo elemento della lista,
   ovvero il coefficiente direttore. */
rendiMonico([], []) :- !.
rendiMonico(Coefficienti, Monico) :-
    last(Coefficienti, CoeffDirettore),
    InversoCoefficiente is 1.0 / CoeffDirettore,
    moltiplicaPerScalare(Coefficienti, InversoCoefficiente, Monico).

/* Il predicato leggiPolinomio acquisisce un polinomio leggendo i suoi coefficienti da tastiera
   separati da spazi, in ordine crescente di grado:
   - il primo argomento è una stringa che specifica di quale polinomio si tratta;
   - il secondo argomento è la lista dei coefficienti del polinomio acquisito. */
leggiPolinomio(Etichetta, Polinomio) :-
    write('Inserisci i coefficienti del polinomio '), write(Etichetta),
    write(' separati da spazi (ordine crescente): '), nl,
    read_line_to_string(user_input, RigaInput),
    split_string(RigaInput, " ", " ", ListaStringhe),
    elaboraInput(Etichetta, ListaStringhe, Polinomio).

/* Il predicato elaboraInput verifica la validità dell'input acquisito e lo converte
   in una lista di coefficienti numerici, richiedendo un nuovo inserimento in caso di errore:
   - il primo argomento è una stringa che specifica di quale polinomio si tratta;
   - il secondo argomento è la lista di stringhe letta da tastiera;
   - il terzo argomento è la lista dei coefficienti del polinomio risultante.

   Caso base: tutte le stringhe sono valide; si procede alla conversione tramite
   convertiStringheInFloat e alla normalizzazione del risultato.
   Caso generale: almeno una stringa non è valida; viene segnalato l'errore e l'acquisizione
   viene ripetuta tramite leggiPolinomio. */
elaboraInput(_, ListaStringhe, Polinomio) :-
    stringheValide(ListaStringhe), !,
    convertiStringheInFloat(ListaStringhe, Coefficienti),
    rimuoviZeriInTesta(Coefficienti, Polinomio).

elaboraInput(Etichetta, _, Polinomio) :-
    write('*** ERRORE: L\'input contiene caratteri non numerici o non validi. Riprova. ***'), nl, nl,
    leggiPolinomio(Etichetta, Polinomio).

/* Il predicato stringheValide verifica che ogni elemento di una lista di stringhe
   rappresenti un valore numerico valido:
   - il suo unico argomento è la lista di stringhe da verificare.

   Caso base con lista vuota: la verifica ha esito positivo.
   Caso ricorsivo con stringa vuota: l'elemento viene saltato e si procede sul resto.
   Caso ricorsivo generale: si tenta la conversione numerica tramite number_string;
   se ha successo si procede ricorsivamente, altrimenti il predicato fallisce. */
stringheValide([]).
stringheValide([S|Resto]) :-
    S == "", !,
    stringheValide(Resto).
stringheValide([S|Resto]) :-
    catch(number_string(_, S), _, fail),
    stringheValide(Resto).

/* Il predicato convertiStringheInFloat converte una lista di stringhe numeriche
   in una lista di valori in virgola mobile:
   - il primo argomento è la lista di stringhe da convertire;
   - il secondo argomento è la lista dei valori numerici risultanti.

   Caso base con lista vuota: viene restituita la lista vuota.
   Caso ricorsivo con stringa non vuota: la testa viene convertita tramite number_string
   e si procede ricorsivamente sulla coda.
   Caso ricorsivo con stringa vuota: l'elemento viene saltato e si procede sulla coda. */
convertiStringheInFloat([], []).
convertiStringheInFloat([S|Resto], [F|FloatResto]) :-
    S \= "", !,
    number_string(F, S),
    convertiStringheInFloat(Resto, FloatResto).
convertiStringheInFloat([_|Resto], FloatResto) :-
    convertiStringheInFloat(Resto, FloatResto).
