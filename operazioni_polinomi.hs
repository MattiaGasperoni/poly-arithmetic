{- Programma Haskell per operazioni ed algoritmi su polinomi. -}

module Main where

-- necessario per disabilitare il buffering dell'output
import System.IO (hSetBuffering, stdout, BufferMode (..))
-- necessario per il parsing sicuro dei coefficienti da tastiera
import Text.Read (readMaybe)
-- necessario per riconoscere le cifre nei token digitati dall'utente
import Data.Char (isDigit)
-- necessario per rimuovere i coefficienti nulli di grado massimo
import Data.List (dropWhileEnd)
{- necessario per intercettare l'EOF da tastiera in modo pulito
   (invece di lasciar propagare l'eccezione non gestita di getLine) -}
import System.IO.Error (isEOFError, catchIOError)
-- necessario per terminare pulitamente il programma in caso di EOF
import System.Exit (exitSuccess)

{- La costante tolleranza rappresenta la soglia al di sotto della
   quale un valore Double viene considerato pari a zero, al fine di
   compensare gli errori di arrotondamento tipici dell'aritmetica in
   virgola mobile -}

tolleranza :: Double
tolleranza = 0.000001

{- L'azione main coordina l'intero programma: acquisisce i due
   polinomi, li visualizza, calcola e stampa il grado di ciascuno, la
   loro somma, differenza e prodotto, il quoziente e il resto della
   divisione euclidea (segnalando l'eventuale impossibilità di
   dividere per il polinomio nullo) e il loro massimo comune divisore -}

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    polinomio_a <- acquisisci_polinomio "A"
    polinomio_b <- acquisisci_polinomio "B"
    putStrLn "\n"
    putStrLn $ "Polinomio A:    " ++ mostra polinomio_a
    putStrLn $ "Polinomio B:    " ++ mostra polinomio_b
    putStrLn $ "Grado A:        " ++ show (grado_polinomio polinomio_a)
    putStrLn $ "Grado B:        " ++ show (grado_polinomio polinomio_b)
    putStrLn $ "Somma:          " ++
               mostra (somma polinomio_a polinomio_b)
    putStrLn $ "Differenza:     " ++
               mostra (differenza polinomio_a polinomio_b)
    putStrLn $ "Prodotto:       " ++
               mostra (prodotto polinomio_a polinomio_b)
    case divisione polinomio_a polinomio_b of
      Nothing -> putStrLn ("Errore:         impossibile dividere " ++
                           "per il polinomio nullo.")
      Just (quoziente, resto) -> do
        putStrLn $ "Quoziente:      " ++ mostra quoziente
        putStrLn $ "Resto:          " ++ mostra resto
    putStrLn $ "MCD:            " ++
               mostra (mcd polinomio_a polinomio_b)

{- L'azione parametrica di input/output acquisisci_polinomio
   acquisisce un polinomio di coefficienti Double da tastiera,
   restituendo la lista dei coefficienti in ordine crescente di grado:
   - il suo unico argomento è l'etichetta (nome) del polinomio da
     acquisire, usata nel messaggio mostrato all'utente
   Se l'utente chiude lo standard input (EOF) invece di digitare una
   riga, il programma termina con un messaggio invece di sollevare
   un'eccezione non gestita -}

acquisisci_polinomio :: String -> IO [Double]
acquisisci_polinomio etichetta = do
    putStr $ "Inserisci i coefficienti del polinomio " ++
             etichetta ++
             " separati da spazi (ordine crescente di grado): "
    riga_letta <- leggi_riga_sicura
    case riga_letta of
      Nothing -> do
        putStrLn "\nInput terminato (EOF). Uscita dal programma."
        exitSuccess
      Just riga -> case words riga of
        [] -> putStrLn "Devi inserire almeno un coefficiente esplicito!"
              >> acquisisci_polinomio etichetta
        token_letti -> case mapM leggi_coefficiente token_letti of
          Just coeff -> return (normalizza coeff)
          Nothing    -> putStrLn "Formato non valido! Riprova." >>
                        acquisisci_polinomio etichetta

{- L'azione leggi_riga_sicura legge una riga da tastiera restituendo
   Nothing se lo standard input è terminato (EOF), invece di lasciar
   propagare l'eccezione di getLine -}

leggi_riga_sicura :: IO (Maybe String)
leggi_riga_sicura =
    (Just <$> getLine) `catchIOError` gestisci_errore_lettura
  where
    {- L'azione ausiliaria gestisci_errore_lettura restituisce Nothing
       se l'errore intercettato segnala la fine dell'input, altrimenti
       propaga l'errore:
       - il suo unico argomento è l'errore di input/output
         intercettato -}

    gestisci_errore_lettura :: IOError -> IO (Maybe String)
    gestisci_errore_lettura errore
      | isEOFError errore = return Nothing
      | otherwise         = ioError errore

{- La funzione leggi_coefficiente converte un token digitato
   dall'utente nel corrispondente coefficiente, restituendo Nothing se
   il token non rappresenta un numero reale finito in notazione
   decimale o scientifica:
   - il suo unico argomento è il token da convertire
   Il controllo preliminare sui caratteri ammessi esclude le notazioni
   esadecimale e ottale di Haskell (0x10, 0o10), che altrimenti
   sarebbero accettate da readMaybe ma non dal programma Prolog -}

leggi_coefficiente :: String -> Maybe Double
leggi_coefficiente token
  | all carattere_ammesso token = verifica_coefficiente (readMaybe token)
  | otherwise                   = Nothing
  where
    {- La funzione ausiliaria carattere_ammesso riconosce i caratteri
       che possono comparire in un coefficiente reale:
       - il suo unico argomento è il carattere da riconoscere -}

    carattere_ammesso :: Char -> Bool
    carattere_ammesso carattere = isDigit carattere ||
                                  elem carattere ".+-eE"

{- La funzione verifica_coefficiente scarta i valori non finiti
   prodotti dall'overflow della conversione (ad esempio 1e400), che
   renderebbero non terminante il calcolo della divisione e del
   massimo comune divisore:
   - il suo unico argomento è l'esito della conversione del token -}

verifica_coefficiente :: Maybe Double -> Maybe Double
verifica_coefficiente Nothing = Nothing
verifica_coefficiente (Just coefficiente)
  | isNaN coefficiente || isInfinite coefficiente = Nothing
  | otherwise                                     = Just coefficiente

{- La funzione normalizza elimina i coefficienti nulli di grado
   massimo di un polinomio, ossia quelli che si trovano in coda alla
   lista:
   - il suo unico argomento è la lista dei coefficienti in ordine
     crescente di grado -}

normalizza :: [Double] -> [Double]
normalizza = dropWhileEnd (\c -> abs c < tolleranza)

{- La funzione mostra restituisce la rappresentazione algebrica di un
   polinomio, dal grado massimo al minimo:
   - il suo unico argomento è la lista dei coefficienti in ordine
     crescente di grado
   Le funzioni ausiliarie termini, segno e monomio dipendono dalla
   struttura del polinomio da stampare e sono usate solo qui, da cui
   il where; le funzioni di formattazione del singolo coefficiente
   sono invece indipendenti dal polinomio e definite a livello
   globale -}

mostra :: [Double] -> String
mostra polinomio =
    termini (reverse (zip [0 ..] (normalizza polinomio))) True
  where
    {- La funzione ausiliaria termini concatena ricorsivamente i
       termini del polinomio:
       - il primo argomento è la lista delle coppie (grado,
         coefficiente) residue da stampare, ordinata dal grado
         massimo al minimo
       - il secondo argomento è il flag che indica se il termine da
         stampare è il primo termine non nullo del polinomio -}

    termini :: [(Int, Double)] -> Bool -> String
    termini [] True  = "0"
    termini [] False = ""
    termini ((grado, c) : resto) primo
      | abs c < tolleranza = termini resto primo
      | otherwise          = segno c primo ++
                             monomio grado (abs c) ++
                             termini resto False

    {- La funzione ausiliaria segno restituisce il segno da anteporre
       al termine corrente:
       - il primo argomento è il coefficiente del termine corrente
       - il secondo argomento è il flag che indica se il termine è il
         primo termine non nullo del polinomio -}

    segno :: Double -> Bool -> String
    segno c True  | c < 0     = "-"
                  | otherwise = ""
    segno c False | c < 0     = " - "
                  | otherwise = " + "

    {- La funzione ausiliaria monomio formatta grado e coefficiente,
       omettendo i coefficienti unitari e le potenze 0 e 1:
       - il primo argomento è il grado del monomio
       - il secondo argomento è il valore assoluto del coefficiente
         del monomio -}

    monomio :: Int -> Double -> String
    monomio 0 c = formatta_coefficiente c
    monomio 1 c | abs (c - 1) < tolleranza = "x"
                | otherwise = formatta_coefficiente c ++ "x"
    monomio g c | abs (c - 1) < tolleranza = "x^" ++ show g
                | otherwise = formatta_coefficiente c ++
                              "x^" ++ show g

{- La funzione formatta_coefficiente restituisce la rappresentazione
   testuale di un coefficiente non negativo, come intero se la parte
   decimale è trascurabile, altrimenti arrotondato a quattro cifre
   decimali:
   - il suo unico argomento è il valore del coefficiente
   Per valori il cui modulo supera 1e14 si evita la conversione a Int
   (che risulterebbe inesatta vicino ai limiti di Int) e si delega la
   stampa a show sul Double originale -}

formatta_coefficiente :: Double -> String
formatta_coefficiente x
  | abs x > 1.0e14 = show x
  | abs (x - fromIntegral (round x :: Int)) < tolleranza =
      show (round x :: Int)
  | otherwise = componi_parti parte_intera parte_frazionaria
  where
    parte_intera      = truncate x :: Int
    frazione          = x - fromIntegral parte_intera
    parte_frazionaria = round (frazione * 10000) :: Int

{- La funzione componi_parti concatena la parte intera e, se
   significativa, la parte frazionaria di un coefficiente, gestendo
   il riporto prodotto dall'arrotondamento alla quarta cifra
   decimale:
   - il primo argomento è la parte intera del coefficiente
   - il secondo argomento è la parte frazionaria arrotondata,
     espressa come intero su quattro cifre -}

componi_parti :: Int -> Int -> String
componi_parti parte_intera 10000 = show (parte_intera + 1)
componi_parti parte_intera 0     = show parte_intera
componi_parti parte_intera parte_frazionaria =
    show parte_intera ++ "." ++
    cifre_frazionarie parte_frazionaria 1000

{- La funzione cifre_frazionarie concatena le cifre della parte
   frazionaria, dalla più significativa alla meno significativa,
   arrestandosi quando le cifre residue sono tutte nulle:
   - il primo argomento è la parte frazionaria residua, espressa come
     intero su quattro cifre
   - il secondo argomento è il peso della cifra da concatenare -}

cifre_frazionarie :: Int -> Int -> String
cifre_frazionarie 0 _ = ""
cifre_frazionarie parte_frazionaria peso =
    show (parte_frazionaria `div` peso) ++
    cifre_frazionarie (parte_frazionaria `mod` peso) (peso `div` 10)

{- La funzione grado_polinomio calcola il grado di un polinomio come
   la lunghezza del polinomio normalizzato meno uno, oppure 0 se il
   polinomio è nullo:
   - il suo unico argomento è la lista dei coefficienti in ordine
     crescente di grado -}

grado_polinomio :: [Double] -> Int
grado_polinomio coeff = case normalizza coeff of
  []   -> 0
  norm -> length norm - 1

{- La funzione somma calcola la somma di due polinomi in tempo
   lineare O(n):
   - il primo argomento è la lista dei coefficienti del primo
     polinomio
   - il secondo argomento è la lista dei coefficienti del secondo
     polinomio
   il risultato è la lista dei coefficienti del polinomio somma, non
   normalizzata -}

somma :: [Double] -> [Double] -> [Double]
somma [] coeff_b = coeff_b
somma coeff_a [] = coeff_a
somma (testa_a : resto_a) (testa_b : resto_b) =
    (testa_a + testa_b) : somma resto_a resto_b

{- La funzione differenza calcola la differenza tra due polinomi in
   tempo lineare O(n):
   - il primo argomento è la lista dei coefficienti del primo
     polinomio
   - il secondo argomento è la lista dei coefficienti del secondo
     polinomio
   il risultato è la lista dei coefficienti del polinomio differenza,
   non normalizzata -}

differenza :: [Double] -> [Double] -> [Double]
differenza [] coeff_b = map negate coeff_b
differenza coeff_a [] = coeff_a
differenza (testa_a : resto_a) (testa_b : resto_b) =
    (testa_a - testa_b) : differenza resto_a resto_b

{- La funzione prodotto calcola il prodotto di due polinomi:
   - il primo argomento è la lista dei coefficienti del primo
     polinomio
   - il secondo argomento è la lista dei coefficienti del secondo
     polinomio
   il risultato è la lista dei coefficienti del polinomio prodotto,
   non normalizzata -}

prodotto :: [Double] -> [Double] -> [Double]
prodotto [] _ = []
prodotto _ [] = []
prodotto (testa_a : resto_a) polinomio_b =
    somma (map (* testa_a) polinomio_b)
          (0 : prodotto resto_a polinomio_b)

{- La funzione divisione calcola quoziente e resto della divisione
   euclidea tra due polinomi:
   - il primo argomento è la lista dei coefficienti del polinomio
     dividendo
   - il secondo argomento è la lista dei coefficienti del polinomio
     divisore
   il risultato, se il divisore non è il polinomio nullo, è la coppia
   formata dalla lista dei coefficienti del polinomio quoziente e
   dalla lista dei coefficienti del polinomio resto -}

divisione :: [Double] -> [Double] -> Maybe ([Double], [Double])
divisione dividendo divisore
  | null (normalizza divisore) = Nothing
  | otherwise = Just (divisione_ricorsiva (normalizza dividendo)
                                          (normalizza divisore)
                                          [])
  where
    {- La funzione ausiliaria divisione_ricorsiva esegue la divisione
       lunga accumulando il quoziente:
       - il primo argomento è la lista dei coefficienti del dividendo
         corrente, normalizzata
       - il secondo argomento è la lista dei coefficienti del
         divisore, normalizzata
       - il terzo argomento è la lista dei coefficienti del quoziente
         parziale accumulato finora -}

    divisione_ricorsiva :: [Double] -> [Double] -> [Double] ->
                           ([Double], [Double])
    divisione_ricorsiva dividendo_corrente divisore_norm quoziente
      | length dividendo_corrente < length divisore_norm =
          (normalizza quoziente, normalizza dividendo_corrente)
      | otherwise = divisione_ricorsiva dividendo_aggiornato
                                        divisore_norm
                                        quoziente_aggiornato
      where
        coefficiente_termine = last dividendo_corrente /
                               last divisore_norm
        termine_quoziente    = replicate (length dividendo_corrente -
                                          length divisore_norm) 0 ++
                               [coefficiente_termine]
        dividendo_aggiornato = normalizza
                                 (differenza dividendo_corrente
                                    (prodotto divisore_norm
                                              termine_quoziente))
        quoziente_aggiornato = somma quoziente termine_quoziente

{- La funzione mcd calcola il massimo comune divisore di due polinomi
   tramite l'algoritmo di Euclide, restituendo il risultato reso
   monico:
   - il primo argomento è la lista dei coefficienti del primo
     polinomio
   - il secondo argomento è la lista dei coefficienti del secondo
     polinomio
   il risultato è la lista dei coefficienti del polinomio massimo
   comun divisore -}

mcd :: [Double] -> [Double] -> [Double]
mcd polinomio_a polinomio_b = euclide (normalizza polinomio_a)
                                      (normalizza polinomio_b)
  where
    {- La funzione ausiliaria euclide applica ricorsivamente
       l'algoritmo di Euclide ai due polinomi, tramite monico, fino a
       ridurre il secondo polinomio al polinomio nullo:
       - il primo argomento è la lista dei coefficienti del primo
         polinomio, normalizzata
       - il secondo argomento è la lista dei coefficienti del secondo
         polinomio, normalizzata
       Caso limite: se entrambi i polinomi sono nulli, il risultato è
       il polinomio nullo -}

    euclide :: [Double] -> [Double] -> [Double]
    euclide primo []      = monico primo
    euclide primo secondo = euclide secondo
                              (normalizza (resto_divisione primo
                                                           secondo))

    {- La funzione ausiliaria monico divide tutti i coefficienti di
       un polinomio per il suo coefficiente direttore (l'ultimo della
       lista):
       - il suo unico argomento è la lista dei coefficienti da
         rendere monica -}

    monico :: [Double] -> [Double]
    monico []    = []
    monico coeff = map (/ last coeff) coeff

    {- La funzione ausiliaria resto_divisione estrae il resto della
       divisione euclidea tra due polinomi:
       - il primo argomento è la lista dei coefficienti del dividendo
       - il secondo argomento è la lista dei coefficienti del divisore
       Il divisore non è mai il polinomio nullo quando la funzione
       viene invocata da euclide, per cui il secondo caso non si
       verifica mai ed è presente solo per esaustività -}

    resto_divisione :: [Double] -> [Double] -> [Double]
    resto_divisione primo secondo = case divisione primo secondo of
      Just (_, resto) -> resto
      Nothing         -> []