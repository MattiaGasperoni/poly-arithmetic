module Main where
 
import System.IO (hSetBuffering, stdout, BufferMode (..))
import Text.Read (readMaybe)
import Data.List (dropWhileEnd)
 
-- Soglia di tolleranza per i confronti in virgola mobile.
-- Evita di trattare valori come 0.0000001 come se fossero diversi da zero.
tolleranzaNumerica :: Double
tolleranzaNumerica = 1e-6
 
-- Rimuove i coefficienti nulli dal grado massimo verso il basso.
-- Necessario per calcolare il vero grado del polinomio dopo ogni operazione.
rimuoviZeriInTesta :: [Double] -> [Double]
rimuoviZeriInTesta = dropWhileEnd (\coefficiente -> abs coefficiente < tolleranzaNumerica)
 
-- Restituisce il grado del polinomio (lunghezza della lista ripulita meno 1).
gradoPolinomio :: [Double] -> Int
gradoPolinomio polinomio = max 0 (length (rimuoviZeriInTesta polinomio) - 1)
 
-- Formatta un Double per la visualizzazione.
-- Se è praticamente intero, omette la parte decimale.
-- Altrimenti arrotonda a 4 cifre decimali.
formattaCoefficiente :: Double -> String
formattaCoefficiente x
  | abs (x - fromIntegral (round x :: Int)) < tolleranzaNumerica = show (round x :: Int)
  | otherwise = show (fromIntegral (round (x * 10000)) / 10000)
 
-- ---------------------------------------------------------------------------
-- Visualizzazione Algebrica Canonica
-- ---------------------------------------------------------------------------
 
-- Punto di ingresso per la stampa: associa ogni coefficiente al suo grado [0..],
-- poi inverte la lista per stampare dal grado massimo a scendere.
mostraPolinomio :: [Double] -> String
mostraPolinomio polinomio = formattaTermini (reverse (zip [0..] (rimuoviZeriInTesta polinomio))) True
 
-- Funzione ricorsiva che elabora ogni coppia (grado, coefficiente).
-- 'ePrimoTermine' indica se stiamo stampando il primo termine
-- (per evitare il "+" iniziale sui termini positivi).
formattaTermini :: [(Int, Double)] -> Bool -> String
formattaTermini [] True  = "0"  -- Polinomio vuoto: visualizza zero.
formattaTermini [] False = ""
formattaTermini ((grado, coefficiente) : resto) ePrimoTermine
  | abs coefficiente < tolleranzaNumerica = formattaTermini resto ePrimoTermine  -- Salta i termini nulli.
  | otherwise = formattaSegno coefficiente ePrimoTermine
             ++ formattaMonomio grado (abs coefficiente)
             ++ formattaTermini resto False
 
-- Restituisce la stringa del segno da anteporre al termine.
formattaSegno :: Double -> Bool -> String
formattaSegno coefficiente True  | coefficiente < 0 = "-"    -- Primo termine negativo.
                                 | otherwise        = ""     -- Primo termine positivo: nessun segno.
formattaSegno coefficiente False | coefficiente < 0 = " - "  -- Termine successivo negativo.
                                 | otherwise        = " + "  -- Termine successivo positivo.
 
-- Formatta un singolo monomio, omettendo notazioni ridondanti
-- (es. "1x" diventa "x", "x^1" diventa "x", "x^0" è solo il numero).
formattaMonomio :: Int -> Double -> String
formattaMonomio 0 coefficiente = formattaCoefficiente coefficiente
formattaMonomio 1 coefficiente
  | abs (coefficiente - 1) < tolleranzaNumerica = "x"
  | otherwise                                   = formattaCoefficiente coefficiente ++ "x"
formattaMonomio grado coefficiente
  | abs (coefficiente - 1) < tolleranzaNumerica = "x^" ++ show grado
  | otherwise                                   = formattaCoefficiente coefficiente ++ "x^" ++ show grado
 
-- ---------------------------------------------------------------------------
-- Operazioni Aritmetiche di Base
-- ---------------------------------------------------------------------------
 
-- Somma due polinomi termine a termine.
sommaPolinomi :: [Double] -> [Double] -> [Double]
sommaPolinomi [] secondoPolinomio = rimuoviZeriInTesta secondoPolinomio
sommaPolinomi primoPolinomio [] = rimuoviZeriInTesta primoPolinomio
sommaPolinomi (c1 : resto1) (c2 : resto2) =
    rimuoviZeriInTesta ((c1 + c2) : sommaPolinomi resto1 resto2)
 
-- Sottrae il secondo polinomio dal primo, termine a termine.
-- Se il primo termina prima, i coefficienti restanti del secondo vengono negati.
sottraiPolinomi :: [Double] -> [Double] -> [Double]
sottraiPolinomi [] secondoPolinomio = rimuoviZeriInTesta (map negate secondoPolinomio)
sottraiPolinomi primoPolinomio [] = rimuoviZeriInTesta primoPolinomio
sottraiPolinomi (c1 : resto1) (c2 : resto2) =
    rimuoviZeriInTesta ((c1 - c2) : sottraiPolinomi resto1 resto2)
 
-- Moltiplica due polinomi per distribuzione:
-- moltiplica il primo coefficiente di A per tutto B,
-- poi somma il risultato al prodotto restante scalato di un grado (preponi 0).
moltiplPolinomi :: [Double] -> [Double] -> [Double]
moltiplPolinomi [] _ = []
moltiplPolinomi _ [] = []
moltiplPolinomi (coeffTesta : coeffResto) secondoPolinomio =
    rimuoviZeriInTesta
        (sommaPolinomi
            (map (* coeffTesta) secondoPolinomio)
            (0 : moltiplPolinomi coeffResto secondoPolinomio))
 
-- ---------------------------------------------------------------------------
-- Divisione Euclidea e MCD
-- ---------------------------------------------------------------------------
 
-- Interfaccia pubblica per la divisione con resto.
-- Restituisce la coppia (quoziente, resto).
divisioneConResto :: [Double] -> [Double] -> ([Double], [Double])
divisioneConResto dividendo divisore =
    passoDiv (rimuoviZeriInTesta dividendo) (rimuoviZeriInTesta divisore) []
 
-- Divisione lunga ricorsiva.
-- Accumula il quoziente in 'quozienteParziale'.
-- Caso base: il grado del dividendo è inferiore a quello del divisore.
passoDiv :: [Double] -> [Double] -> [Double] -> ([Double], [Double])
passoDiv dividendoCorrente divisore quozienteParziale
  | length dividendoCorrente < length divisore =
        (rimuoviZeriInTesta quozienteParziale, rimuoviZeriInTesta dividendoCorrente)
  | otherwise = passoDiv dividendoRidotto divisore quozienteAggiornato
  where
    coeffDirettoreDividendo = last dividendoCorrente                -- Coefficiente direttore del dividendo.
    coeffDirettoreDivisore  = last divisore                         -- Coefficiente direttore del divisore.
    differenzaDiGrado       = length dividendoCorrente - length divisore  -- Differenza di grado.
    coefficienteDelPasso    = coeffDirettoreDividendo / coeffDirettoreDivisore  -- Coeff. del termine quoziente.
    termineCorrente         = replicate differenzaDiGrado 0 ++ [coefficienteDelPasso]  -- Monomio come lista densa.
    termineDaSottrarre      = moltiplPolinomi divisore termineCorrente    -- Prodotto da sottrarre.
    dividendoRidotto        = sottraiPolinomi dividendoCorrente termineDaSottrarre  -- Nuovo dividendo.
    quozienteAggiornato     = sommaPolinomi quozienteParziale termineCorrente       -- Quoziente accumulato.
 
-- Calcola l'MCD di due polinomi, normalizzando prima gli input.
calcolaMCD :: [Double] -> [Double] -> [Double]
calcolaMCD poliA poliB = algoritmoEuclide (rimuoviZeriInTesta poliA) (rimuoviZeriInTesta poliB)
 
-- Algoritmo di Euclide: sostituisce ripetutamente (a, b) con (b, resto di a/b)
-- finché il resto non è zero. Restituisce l'ultimo divisore non nullo reso monico.
algoritmoEuclide :: [Double] -> [Double] -> [Double]
algoritmoEuclide poliA [] = rendiMonico poliA
algoritmoEuclide poliA poliB = algoritmoEuclide poliB (snd (divisioneConResto poliA poliB))
 
-- Divide tutti i coefficienti per il coefficiente direttore,
-- rendendo il polinomio monico (coefficiente direttore = 1).
rendiMonico :: [Double] -> [Double]
rendiMonico [] = []
rendiMonico coefficienti = map (/ last coefficienti) coefficienti
 
-- ---------------------------------------------------------------------------
-- Ciclo Principale e I/O
-- ---------------------------------------------------------------------------
 
-- Legge una riga dall'utente, la divide per spazi
-- e tenta il parsing di ogni token come Double.
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