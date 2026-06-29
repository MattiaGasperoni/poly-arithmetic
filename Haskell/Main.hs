-- Programma Haskell per operazioni ed algoritmi avanzati su polinomi.
module Main where

import System.IO (hSetBuffering, stdout, BufferMode (..))
import Text.Read (readMaybe)
import Data.List (dropWhileEnd)

tolleranzaNumerica :: Double
tolleranzaNumerica = 1e-6

{- La funzione rimuoviZeriInTesta normalizza un polinomio eliminando i coefficienti nulli
   a partire dal grado massimo verso il basso:
   - il suo unico argomento è la lista dei coefficienti del polinomio in ordine crescente di grado. -}
rimuoviZeriInTesta :: [Double] -> [Double]
rimuoviZeriInTesta = dropWhileEnd (\coefficiente -> abs coefficiente < tolleranzaNumerica)

{- La funzione gradoPolinomio calcola il grado di un polinomio:
   - il suo unico argomento è la lista dei coefficienti del polinomio in ordine crescente di grado. -}
gradoPolinomio :: [Double] -> Int
gradoPolinomio polinomio = max 0 (length (rimuoviZeriInTesta polinomio) - 1)

{- La funzione formattaCoefficiente restituisce la rappresentazione testuale di un coefficiente:
   - il suo unico argomento è il valore del coefficiente da formattare.

   Caso base con parte decimale trascurabile: il valore viene arrotondato e restituito come intero.
   Caso generale: il valore viene arrotondato a quattro cifre decimali. -}
formattaCoefficiente :: Double -> String
formattaCoefficiente x
  | abs (x - fromIntegral (round x :: Int)) < tolleranzaNumerica = show (round x :: Int)
  | otherwise = show (fromIntegral (round (x * 10000)) / 10000)

{- La funzione mostraPolinomio restituisce la rappresentazione algebrica canonica di un polinomio,
   stampando i termini dal grado massimo al grado minimo.
   L'uso di reverse consente di elaborare i termini in ordine decrescente di grado:
   - il suo unico argomento è la lista dei coefficienti del polinomio in ordine crescente di grado. -}
mostraPolinomio :: [Double] -> String
mostraPolinomio polinomio = formattaTermini (reverse (zip [0..] (rimuoviZeriInTesta polinomio))) True

{- La funzione formattaTermini elabora ricorsivamente le coppie (grado, coefficiente) di un polinomio
   restituendone la rappresentazione testuale:
   - il primo argomento è la lista delle coppie (grado, coefficiente) in ordine decrescente di grado;
   - il secondo argomento indica se il termine da elaborare è il primo della rappresentazione.

   Caso base con lista vuota e primo termine: viene restituita la stringa "0".
   Caso base con lista vuota: viene restituita la stringa vuota.
   Casi generali: i termini con coefficiente nullo vengono saltati; gli altri vengono
   formattati con il segno appropriato tramite formattaSegno e il monomio tramite formattaMonomio,
   procedendo ricorsivamente sul resto della lista. -}
formattaTermini :: [(Int, Double)] -> Bool -> String
formattaTermini [] True  = "0"
formattaTermini [] False = ""
formattaTermini ((grado, coefficiente) : resto) ePrimoTermine
  | abs coefficiente < tolleranzaNumerica = formattaTermini resto ePrimoTermine
  | otherwise = formattaSegno coefficiente ePrimoTermine
             ++ formattaMonomio grado (abs coefficiente)
             ++ formattaTermini resto False

{- La funzione formattaSegno restituisce la stringa del segno da anteporre a un termine:
   - il primo argomento è il valore del coefficiente del termine;
   - il secondo argomento indica se il termine è il primo della rappresentazione.

   Caso con primo termine negativo: viene restituito "-".
   Caso con primo termine positivo: viene restituita la stringa vuota.
   Caso con termine successivo negativo: viene restituita " - ".
   Caso con termine successivo positivo: viene restituita " + ". -}
formattaSegno :: Double -> Bool -> String
formattaSegno coefficiente True  | coefficiente < 0 = "-"
                                 | otherwise        = ""
formattaSegno coefficiente False | coefficiente < 0 = " - "
                                 | otherwise        = " + "

{- La funzione formattaMonomio restituisce la rappresentazione testuale di un monomio,
   omettendo i coefficienti unitari e le potenze di esponente zero o uno:
   - il primo argomento è il grado del monomio;
   - il secondo argomento è il valore assoluto del coefficiente del monomio.

   Caso base con grado zero: viene restituito il solo valore del coefficiente.
   Caso con grado uno e coefficiente unitario: viene restituita la stringa "x".
   Caso con grado uno: viene restituita la stringa del coefficiente seguita da "x".
   Caso con grado superiore e coefficiente unitario: viene restituita la stringa "x^grado".
   Caso generale: viene restituita la stringa del coefficiente seguita da "x^grado". -}
formattaMonomio :: Int -> Double -> String
formattaMonomio 0 coefficiente = formattaCoefficiente coefficiente
formattaMonomio 1 coefficiente
  | abs (coefficiente - 1) < tolleranzaNumerica = "x"
  | otherwise                                   = formattaCoefficiente coefficiente ++ "x"
formattaMonomio grado coefficiente
  | abs (coefficiente - 1) < tolleranzaNumerica = "x^" ++ show grado
  | otherwise                                   = formattaCoefficiente coefficiente ++ "x^" ++ show grado

{- La funzione sommaPolinomi calcola la somma di due polinomi:
   - il primo argomento è il primo dei due polinomi;
   - il secondo argomento è il secondo dei due polinomi.

   Caso base con primo polinomio vuoto: viene restituito il secondo polinomio normalizzato.
   Caso base con secondo polinomio vuoto: viene restituito il primo polinomio normalizzato.
   Caso generale: le teste vengono sommate e si procede ricorsivamente sulle code. -}
sommaPolinomi :: [Double] -> [Double] -> [Double]
sommaPolinomi [] secondoPolinomio = rimuoviZeriInTesta secondoPolinomio
sommaPolinomi primoPolinomio [] = rimuoviZeriInTesta primoPolinomio
sommaPolinomi (c1 : resto1) (c2 : resto2) =
    rimuoviZeriInTesta ((c1 + c2) : sommaPolinomi resto1 resto2)

{- La funzione sottraiPolinomi calcola la differenza tra due polinomi:
   - il primo argomento è il primo dei due polinomi;
   - il secondo argomento è il secondo dei due polinomi.

   Caso base con primo polinomio vuoto: i coefficienti del secondo vengono negati e restituiti.
   Caso base con secondo polinomio vuoto: viene restituito il primo polinomio normalizzato.
   Caso generale: le teste vengono sottratte e si procede ricorsivamente sulle code. -}
sottraiPolinomi :: [Double] -> [Double] -> [Double]
sottraiPolinomi [] secondoPolinomio = rimuoviZeriInTesta (map negate secondoPolinomio)
sottraiPolinomi primoPolinomio [] = rimuoviZeriInTesta primoPolinomio
sottraiPolinomi (c1 : resto1) (c2 : resto2) =
    rimuoviZeriInTesta ((c1 - c2) : sottraiPolinomi resto1 resto2)

{- La funzione moltiplPolinomi calcola il prodotto di due polinomi per distribuzione:
   - il primo argomento è il primo dei due polinomi;
   - il secondo argomento è il secondo dei due polinomi.

   Caso base con primo polinomio vuoto: viene restituita la lista vuota.
   Caso base con secondo polinomio vuoto: viene restituita la lista vuota.
   Caso generale: si moltiplica la testa del primo polinomio per il secondo, poi si somma
   il risultato al prodotto della coda del primo per il secondo, scalato di un grado
   tramite la prepend di uno zero. -}
moltiplPolinomi :: [Double] -> [Double] -> [Double]
moltiplPolinomi [] _ = []
moltiplPolinomi _ [] = []
moltiplPolinomi (coeffTesta : coeffResto) secondoPolinomio =
    rimuoviZeriInTesta
        (sommaPolinomi
            (map (* coeffTesta) secondoPolinomio)
            (0 : moltiplPolinomi coeffResto secondoPolinomio))

{- La funzione divisioneConResto calcola il quoziente e il resto della divisione euclidea tra due polinomi:
   - il primo argomento è il polinomio dividendo;
   - il secondo argomento è il polinomio divisore. -}
divisioneConResto :: [Double] -> [Double] -> ([Double], [Double])
divisioneConResto dividendo divisore =
    passoDiv (rimuoviZeriInTesta dividendo) (rimuoviZeriInTesta divisore) []

{- La funzione passoDiv esegue ricorsivamente la divisione lunga tra polinomi, accumulando il quoziente:
   - il primo argomento è il polinomio dividendo corrente;
   - il secondo argomento è il polinomio divisore;
   - il terzo argomento è il quoziente parziale accumulato fino al passo corrente.

   Caso base: il grado del dividendo corrente è inferiore a quello del divisore; il quoziente
   parziale e il dividendo corrente vengono restituiti rispettivamente come quoziente e resto.
   Caso generale: si calcola il termine del quoziente dividendo i coefficienti direttori,
   si sottrae dal dividendo corrente il prodotto del divisore per tale termine e si procede
   ricorsivamente con il dividendo ridotto e il quoziente aggiornato. -}
passoDiv :: [Double] -> [Double] -> [Double] -> ([Double], [Double])
passoDiv dividendoCorrente divisore quozienteParziale
  | length dividendoCorrente < length divisore =
        (rimuoviZeriInTesta quozienteParziale, rimuoviZeriInTesta dividendoCorrente)
  | otherwise = passoDiv dividendoRidotto divisore quozienteAggiornato
  where
    coeffDirettoreDividendo = last dividendoCorrente
    coeffDirettoreDivisore  = last divisore
    differenzaDiGrado       = length dividendoCorrente - length divisore
    coefficienteDelPasso    = coeffDirettoreDividendo / coeffDirettoreDivisore
    termineCorrente         = replicate differenzaDiGrado 0 ++ [coefficienteDelPasso]
    termineDaSottrarre      = moltiplPolinomi divisore termineCorrente
    dividendoRidotto        = sottraiPolinomi dividendoCorrente termineDaSottrarre
    quozienteAggiornato     = sommaPolinomi quozienteParziale termineCorrente

{- La funzione calcolaMCD calcola il massimo comun divisore di due polinomi:
   - il primo argomento è il primo dei due polinomi;
   - il secondo argomento è il secondo dei due polinomi. -}
calcolaMCD :: [Double] -> [Double] -> [Double]
calcolaMCD poliA poliB = algoritmoEuclide (rimuoviZeriInTesta poliA) (rimuoviZeriInTesta poliB)

{- La funzione algoritmoEuclide calcola il massimo comun divisore di due polinomi
   tramite l'algoritmo di Euclide, restituendo il risultato reso monico:
   - il primo argomento è il primo dei due polinomi;
   - il secondo argomento è il secondo dei due polinomi.

   Caso base: il secondo polinomio è vuoto; il primo viene reso monico e restituito come MCD.
   Caso generale: si sostituisce la coppia (A, B) con (B, resto della divisione di A per B)
   e si procede ricorsivamente. -}
algoritmoEuclide :: [Double] -> [Double] -> [Double]
algoritmoEuclide poliA [] = rendiMonico poliA
algoritmoEuclide poliA poliB = algoritmoEuclide poliB (snd (divisioneConResto poliA poliB))

{- La funzione rendiMonico divide tutti i coefficienti di un polinomio per il suo coefficiente direttore:
   - il suo unico argomento è la lista dei coefficienti del polinomio in ordine crescente di grado.

   Caso base con polinomio vuoto: viene restituita la lista vuota.
   Caso generale: ogni coefficiente viene diviso per il coefficiente direttore, ovvero l'ultimo
   elemento della lista. -}
rendiMonico :: [Double] -> [Double]
rendiMonico [] = []
rendiMonico coefficienti = map (/ last coefficienti) coefficienti

{- L'azione parametrica di input/output leggiPolinomio acquisisce un polinomio leggendo i suoi
   coefficienti da tastiera separati da spazi:
   - il suo unico argomento è una stringa che specifica di quale polinomio si tratta.

   Se il parsing di tutti i token ha successo, viene restituita la lista dei coefficienti normalizzata.
   Se almeno un token non è convertibile in numero, viene segnalato l'errore e l'acquisizione
   viene ripetuta. -}
leggiPolinomio :: String -> IO [Double]
leggiPolinomio etichetta = do
    putStr $ "Inserisci i coefficienti del polinomio " ++ etichetta ++ " separati da spazi (ordine crescente): "
    rigaInput <- getLine
    verificaEConverti (words rigaInput)
  where
    verificaEConverti token = case mapM readMaybe token of
        Just coefficienti -> return (rimuoviZeriInTesta coefficienti)
        Nothing           -> putStrLn "Formato non valido! Riprova." >> leggiPolinomio etichetta

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    poliA <- leggiPolinomio "A"
    poliB <- leggiPolinomio "B"
    putStrLn $ "\nA:          " ++ mostraPolinomio poliA
    putStrLn $ "B:          " ++ mostraPolinomio poliB
    putStrLn $ "Somma:      " ++ mostraPolinomio (sommaPolinomi poliA poliB)
    putStrLn $ "Differenza: " ++ mostraPolinomio (sottraiPolinomi poliA poliB)
    putStrLn $ "Prodotto:   " ++ mostraPolinomio (moltiplPolinomi poliA poliB)
    let (quoziente, resto) = divisioneConResto poliA poliB
    putStrLn $ "Quoziente:  " ++ mostraPolinomio quoziente
    putStrLn $ "Resto:      " ++ mostraPolinomio resto
    putStrLn $ "MCD:        " ++ mostraPolinomio (calcolaMCD poliA poliB)
