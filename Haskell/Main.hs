module Main where

import System.IO (hSetBuffering, stdout, BufferMode (..))
import Text.Read (readMaybe)
import Data.List (dropWhileEnd)

-- Soglia di tolleranza numerica per i calcoli in virgola mobile (Double).
-- Serve a prevenire errori di precisione (es. trattare 0.0000001 come zero).
limite :: Double
limite = 1e-6

-- Rimuove i coefficienti nulli (o quasi nulli) di grado massimo.
-- Fondamentale per calcolare il vero grado di un polinomio dopo le operazioni.
normalizza :: [Double] -> [Double]
normalizza = dropWhileEnd (\c -> abs c < limite)

-- Restituisce il grado massimo del polinomio (lunghezza della lista normalizzata - 1).
calcoloGrado :: [Double] -> Int
calcoloGrado p = max 0 (length (normalizza p) - 1)

-- Gestisce la formattazione stringa dei numeri.
-- Se il numero è intero o vicinissimo a un intero, omette la parte decimale.
-- Se è decimale, lo arrotonda stabilmente a 4 cifre decimali.
converti :: Double -> String
converti x
  | abs (x - fromIntegral (round x :: Int)) < limite = show (round x :: Int)
  | otherwise                                        = show (fromIntegral (round (x * 10000)) / 10000)

-- ---------------------------------------------------------------------------
-- Visualizzazione Algebrica Canonica
-- ---------------------------------------------------------------------------

-- Punto di ingresso per la stampa: effettua lo "zip" associando a ogni coefficiente 
-- il rispettivo grado [0..], inverte la lista per stampare dal grado massimo a scendere.
stampaPolinomio :: [Double] -> String
stampaPolinomio p = formattaP (reverse (zip [0..] (normalizza p))) 1

-- Funzione ricorsiva che esamina le coppie (Grado, Coefficiente).
-- Il parametro "primo" (1 o 0) serve a capire se stiamo stampando il primo monomio
-- (evitando di stampare il "+" iniziale se il coefficiente è positivo).
formattaP :: [(Int, Double)] -> Int -> String
formattaP [] 1 = "0" -- Se la lista è vuota e siamo all'inizio, il polinomio è lo zero idoneo.
formattaP [] 0 = ""
formattaP ((g, c):xs) primo
  | abs c < limite = formattaP xs primo -- Salta i monomi con coefficiente nullo.
  | otherwise      = stampaSegno c primo ++ mostraMonomio g (abs c) ++ formattaP xs 0

-- Determina la stringa del segno logico-algebrico da interporre.
stampaSegno :: Double -> Int -> String
stampaSegno c 1 | c < 0     = "-"      -- Primo monomio negativo.
                | otherwise = ""       -- Primo monomio positivo (nessun segno).
stampaSegno c 0 | c < 0     = " - "    -- Monomi intermedi negativi.
                | otherwise = " + "    -- Monomi intermedi positivi.

-- Formatta il singolo monomio omettendo l'unità (1x diventa x) e le potenze ridondanti (x^1 -> x, x^0 -> n).
mostraMonomio :: Int -> Double -> String
mostraMonomio 0 c = converti c                                       -- Grado 0: stampa solo il numero.
mostraMonomio 1 c | abs (c - 1) < limite = "x"                       -- Grado 1, coeff 1: stampa "x".
                  | otherwise            = converti c ++ "x"         -- Grado 1, coeff != 1.
mostraMonomio g c | abs (c - 1) < limite = "x^" ++ show g            -- Grado > 1, coeff 1: stampa "x^g".
                  | otherwise            = converti c ++ "x^" ++ show g

-- ---------------------------------------------------------------------------
-- Operazioni Aritmetiche Elementari
-- ---------------------------------------------------------------------------

-- Somma termine a termine sfruttando il pattern matching sulle liste.
addizione :: [Double] -> [Double] -> [Double]
addizione [] ys = normalizza ys
addizione xs [] = normalizza xs
addizione (x:xs) (y:ys) = normalizza ((x + y) : addizione xs ys)

-- Sottrazione termine a termine. Se la prima lista finisce, inverte il segno della seconda.
sottrazione :: [Double] -> [Double] -> [Double]
sottrazione [] ys = normalizza (map negate ys)
sottrazione xs [] = normalizza xs
sottrazione (x:xs) (y:ys) = normalizza ((x - y) : sottrazione xs ys)

-- Moltiplicazione basata sulla distribuzione: moltiplica il primo coefficiente di A 
-- per tutto B, e vi somma il resto riscalato di un grado (inserendo lo 0 in testa).
moltiplicazione :: [Double] -> [Double] -> [Double]
moltiplicazione [] _ = []
moltiplicazione _ [] = []
moltiplicazione (x:xs) ys = normalizza (addizione (map (* x) ys) (0 : moltiplicazione xs ys))

-- ---------------------------------------------------------------------------
-- Divisione Euclidea e Algoritmo di Euclide (MCD)
-- ---------------------------------------------------------------------------

-- Interfaccia pubblica per la divisione. Invoca la funzione ricorsiva con accumulatore vuoto [].
divisioneConResto :: [Double] -> [Double] -> ([Double], [Double])
divisioneConResto n d = dividiRic (normalizza n) (normalizza d) []

-- Funzione ricorsiva di divisione (Algoritmo Top-Down).
dividiRic :: [Double] -> [Double] -> [Double] -> ([Double], [Double])
dividiRic divid divis qAcc
  | length divid < length divis = (normalizza qAcc, normalizza divid) -- Caso base: grado Dividendo < grado Divisore.
  | otherwise                   = dividiRic nuovoDivid divis nuovoQAcc
  where
    coeffDivid = last divid                             -- Coefficiente direttore del dividendo.
    coeffDivis = last divis                             -- Coefficiente direttore del divisore.
    diffGrado  = length divid - length divis            -- Differenza di grado tra i due polinomi.
    coeffQ     = coeffDivid / coeffDivis                -- Coefficiente del monomio quoziente parziale.
    monomio    = replicate diffGrado 0 ++ [coeffQ]      -- Creazione del monomio in forma di lista densa.
    sottraendo = moltiplicazione divis monomio         -- Calcolo del sottraendo.
    nuovoDivid = sottrazione divid sottraendo           -- Sottrazione per ottenere il nuovo dividendo parziale.
    nuovoQAcc  = addizione qAcc monomio                 -- Accumulazione del quoziente.

-- Calcola l'MCD ripulendo preventivamente gli input.
calcoloMCD :: [Double] -> [Double] -> [Double]
calcoloMCD a b = mcdEuclide (normalizza a) (normalizza b)

-- Algoritmo di Euclide puro: scorre i resti finché il divisore non si azzera ([]).
mcdEuclide :: [Double] -> [Double] -> [Double]
mcdEuclide a [] = monico a -- Quando il resto si azzera, l'MCD è l'ultimo divisore non nullo reso monico.
mcdEuclide a b  = mcdEuclide b (snd (divisioneConResto a b))

-- Rende monico un polinomio dividendo tutti i suoi coefficienti per il coefficiente direttore (l'ultimo).
monico :: [Double] -> [Double]
monico [] = []
monico xs = map (/ last xs) xs

-- ---------------------------------------------------------------------------
-- Main Loop ed I/O
-- ---------------------------------------------------------------------------

-- Legge una riga inserita dall'utente, la spezza sugli spazi e tenta il parsing sicuro in Double.
leggi :: String -> IO [Double]
leggi nome = do
    putStr $ "Inserisci i coefficienti del polinomio " ++ nome ++ " separati da spazi (ordine crescente): "
    line <- getLine
    validaParsea (words line)
  where
    validaParsea s = case mapM readMaybe s of
        Just coeffs -> return (normalizza coeffs)
        Nothing     = putStrLn "Errore di formato! Riprova." >> leggi nome

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    pA <- leggi "A"
    pB <- leggi "B"
    putStrLn $ "\nA: "       ++ stampaPolinomio pA
    putStrLn $ "B: "         ++ stampaPolinomio pB
    putStrLn $ "Somma: "     ++ stampaPolinomio (addizione pA pB)
    putStrLn $ "Differenza: " ++ stampaPolinomio (sottrazione pA pB)
    putStrLn $ "Prodotto: "  ++ stampaPolinomio (moltiplicazione pA pB)
    
    let (q, r) = divisioneConResto pA pB
    putStrLn $ "Quoziente: " ++ stampaPolinomio q
    putStrLn $ "Resto: "     ++ stampaPolinomio r
    putStrLn $ "MCD: "       ++ stampaPolinomio (calcoloMCD pA pB)