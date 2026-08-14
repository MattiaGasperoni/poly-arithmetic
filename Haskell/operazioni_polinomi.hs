{- Programma Haskell per operazioni ed algoritmi su polinomi. -}
module Main where
 
import System.IO (hSetBuffering, stdout, BufferMode (..)) -- necessario per disabilitare il buffering dell'output
import Text.Read (readMaybe)                              -- necessario per il parsing sicuro dei coefficienti inseriti da tastiera
import Data.List (dropWhileEnd)                           -- necessario per rimuovere i coefficienti nulli di grado massimo
 
{- La costante tolleranza rappresenta la soglia al di sotto della quale un valore Double
   viene considerato pari a zero, al fine di compensare gli errori di arrotondamento
   tipici dell'aritmetica in virgola mobile. -}
tolleranza :: Double
tolleranza = 1e-6

{- L'azione main coordina l'intero programma: acquisisce i due polinomi,
   li visualizza, calcola e stampa il grado di ciascuno, la loro somma,
   differenza e prodotto, il quoziente e il resto della divisione euclidea
   (segnalando l'eventuale impossibilità di dividere per il polinomio nullo)
   e il loro massimo comun divisore. -}
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    polinomioA <- acquisisci_polinomio "A"
    polinomioB <- acquisisci_polinomio "B"
    putStrLn $ "\nPolinomio A:  " ++ mostra polinomioA
    putStrLn $ "Polinomio B:  " ++ mostra polinomioB
    putStrLn $ "Grado A:      " ++ show (grado_polinomio polinomioA)
    putStrLn $ "Grado B:      " ++ show (grado_polinomio polinomioB)
    putStrLn $ "Somma:        " ++ mostra (somma polinomioA polinomioB)
    putStrLn $ "Differenza:   " ++ mostra (differenza polinomioA polinomioB)
    putStrLn $ "Prodotto:     " ++ mostra (prodotto polinomioA polinomioB)
    case divisione polinomioA polinomioB of
      Nothing -> putStrLn "Errore:       impossibile dividere per il polinomio nullo."
      Just (quoz, resto) -> do
         putStrLn $ "Quoziente:    " ++ mostra quoz
         putStrLn $ "Resto:        " ++ mostra resto
    putStrLn $ "MCD:          " ++ mostra (mcd polinomioA polinomioB)

{- L'azione parametrica di input/output acquisisci_polinomio acquisisce un polinomio di coefficienti
   Double da tastiera, restituendo la lista dei coefficienti in ordine crescente di grado:
   - il suo unico argomento è l'etichetta (nome) del polinomio da acquisire, usata nel messaggio
     mostrato all'utente. -}
acquisisci_polinomio :: String -> IO [Double]
acquisisci_polinomio etichetta = do
    putStr $ "Inserisci i coefficienti del polinomio " ++ etichetta ++ " separati da spazi (ordine crescente): "
    riga <- getLine
    case words riga of
      [] -> putStrLn "Devi inserire almeno un coefficiente esplicito!" >> acquisisci_polinomio etichetta
      token -> case mapM readMaybe token of
        Just coeff -> return (normalizza coeff)
        Nothing    -> putStrLn "Formato non valido! Riprova." >> acquisisci_polinomio etichetta

{- La funzione grado_polinomio calcola il grado di un polinomio come la lunghezza del polinomio
   normalizzato meno uno, oppure 0 se il polinomio è nullo:
   - il suo unico argomento è la lista dei coefficienti in ordine crescente di grado. -}
grado_polinomio :: [Double] -> Int
grado_polinomio coeff = case normalizza coeff of
  []   -> 0
  norm -> length norm - 1

{- La funzione normalizza elimina gli zeri di testa di un polinomio:
   il suo unico argomento è la lista dei coefficienti in ordine crescente di grado. -}
normalizza :: [Double] -> [Double]
normalizza = dropWhileEnd (\c -> abs c < tolleranza)

{- La funzione mostra restituisce la rappresentazione algebrica di un polinomio, dal grado massimo al minimo:
   - il suo unico argomento è la lista dei coefficienti in ordine crescente di grado.
   Le funzioni ausiliarie segno e monomio sono usate solo qui, da cui il where. -}
mostra :: [Double] -> String
mostra polinomio = termini (reverse (zip [0..] (normalizza polinomio))) True
  where
    -- La funzione ausiliaria termini stampa ricorsivamente i termini del polinomio:
    -- - il primo argomento è la lista delle coppie (grado, coefficiente) residue da stampare,
    --   ordinata dal grado massimo al minimo;
    -- - il secondo argomento è un flag che indica se il termine da stampare è il primo
    --   termine non nullo del polinomio.
    termini :: [(Int, Double)] -> Bool -> String
    termini [] True  = "0"
    termini [] False = ""
    termini ((grado, c) : resto) primo
      | abs c < tolleranza = termini resto primo
      | otherwise = segno c primo ++ monomio grado (abs c) ++ termini resto False

    -- La funzione ausiliaria segno restituisce il segno da anteporre al termine corrente:
    -- - il primo argomento è il coefficiente del termine corrente;
    -- - il secondo argomento è il flag che indica se il termine è il primo termine
    --   non nullo del polinomio.
    segno :: Double -> Bool -> String
    segno c True  | c < 0     = "-"
                  | otherwise = ""
    segno c False | c < 0     = " - "
                  | otherwise = " + "

    -- La funzione ausiliaria monomio formatta grado e coefficiente, omettendo coefficienti
    -- unitari e potenze 0 e 1:
    -- - il primo argomento è il grado del monomio;
    -- - il secondo argomento è il valore assoluto del coefficiente del monomio.
    monomio :: Int -> Double -> String
    monomio 0 c = formatta_coefficienti c
    monomio 1 c | abs (c - 1) < tolleranza = "x"
                | otherwise                = formatta_coefficienti c ++ "x"
    monomio g c | abs (c - 1) < tolleranza = "x^" ++ show g
                | otherwise                = formatta_coefficienti c ++ "x^" ++ show g

{- La funzione formatta_coefficienti restituisce la rappresentazione testuale di un coefficiente,
   come intero se la parte decimale è trascurabile, altrimenti arrotondato a 4 cifre decimali:
   - il suo unico argomento è il valore del coefficiente. -}
formatta_coefficienti :: Double -> String
formatta_coefficienti x
  | abs (x - fromIntegral (round x :: Int)) < tolleranza = show (round x :: Int)
  | otherwise = show (fromIntegral (round (x * 10000)) / 10000)

{- La funzione somma calcola la somma di due polinomi in tempo lineare O(n):
   - il primo argomento è la lista dei coefficienti del primo polinomio;
   - il secondo argomento è la lista dei coefficienti del secondo polinomio;
   il risultato è la lista dei coefficienti del polinomio somma. -}
somma :: [Double] -> [Double] -> [Double]
somma polinomioA polinomioB = normalizza (somma' polinomioA polinomioB)
  where
    -- La funzione ausiliaria somma' somma termine a termine le due liste di coefficienti,
    -- restituendo la coda residua della lista più lunga oltre la fine dell'altra:
    -- - il primo argomento è la lista dei coefficienti del primo polinomio;
    -- - il secondo argomento è la lista dei coefficienti del secondo polinomio.
    somma' [] ys = ys
    somma' xs [] = xs
    somma' (x:xs) (y:ys) = (x + y) : somma' xs ys

{- La funzione differenza calcola la differenza tra due polinomi in tempo lineare O(n):
   - il primo argomento è la lista dei coefficienti del primo polinomio (minuendo);
   - il secondo argomento è la lista dei coefficienti del secondo polinomio (sottraendo);
   il risultato è la lista dei coefficienti del polinomio differenza. -}
differenza :: [Double] -> [Double] -> [Double]
differenza polinomioA polinomioB = normalizza (differenza' polinomioA polinomioB)
  where
    -- La funzione ausiliaria differenza' sottrae termine a termine le due liste di coefficienti,
    -- cambiando segno alla coda residua del secondo polinomio se il primo termina prima:
    -- - il primo argomento è la lista dei coefficienti del primo polinomio (minuendo);
    -- - il secondo argomento è la lista dei coefficienti del secondo polinomio (sottraendo).
    differenza' [] ys = map negate ys
    differenza' xs [] = xs
    differenza' (x:xs) (y:ys) = (x - y) : differenza' xs ys

{- La funzione prodotto calcola il prodotto di due polinomi:
   - il primo argomento è la lista dei coefficienti del primo polinomio;
   - il secondo argomento è la lista dei coefficienti del secondo polinomio;
   il risultato è la lista dei coefficienti del polinomio prodotto. -}
prodotto :: [Double] -> [Double] -> [Double]
prodotto polinomioA polinomioB = normalizza (prodotto' polinomioA polinomioB)
  where
    -- La funzione ausiliaria prodotto' calcola il prodotto tramite somma', senza richiamare
    -- normalizza ad ogni passo della ricorsione:
    -- - il primo argomento è la lista dei coefficienti del primo polinomio;
    -- - il secondo argomento è la lista dei coefficienti del secondo polinomio.
    prodotto' [] _ = []
    prodotto' _ [] = []
    prodotto' (testa : resto) polinomioB' = 
        somma' (map (* testa) polinomioB') (0 : prodotto' resto polinomioB')
    
    -- La funzione ausiliaria somma' è una somma locale non normalizzata, usata per evitare
    -- di richiamare normalizza ad ogni passo del prodotto:
    -- - il primo argomento è la lista dei coefficienti del primo addendo;
    -- - il secondo argomento è la lista dei coefficienti del secondo addendo.
    somma' [] ys = ys
    somma' xs [] = xs
    somma' (x:xs) (y:ys) = (x + y) : somma' xs ys

{- La funzione divisione calcola quoziente e resto della divisione euclidea tra due polinomi:
   - il primo argomento è la lista dei coefficienti del polinomio dividendo;
   - il secondo argomento è la lista dei coefficienti del polinomio divisore;
   il risultato, se il divisore non è il polinomio nullo, è la coppia formata dalla lista dei
   coefficienti del polinomio quoziente e dalla lista dei coefficienti del polinomio resto. -}
divisione :: [Double] -> [Double] -> Maybe ([Double], [Double])
divisione dividendo divisore
  | null (normalizza divisore) = Nothing
  | otherwise = Just (divisione_ricorsiva (normalizza dividendo) (normalizza divisore) [])
  where
    -- La funzione ausiliaria divisione_ricorsiva esegue la divisione lunga accumulando il quoziente:
    -- - il primo argomento è la lista dei coefficienti del dividendo corrente;
    -- - il secondo argomento è la lista dei coefficienti del divisore, normalizzata;
    -- - il terzo argomento è la lista dei coefficienti del quoziente parziale accumulato finora.
    divisione_ricorsiva :: [Double] -> [Double] -> [Double] -> ([Double], [Double])
    divisione_ricorsiva resto_corrente divisore_normalizzato quoziente
      | length resto_corrente < length divisore_normalizzato = (normalizza quoziente, normalizza resto_corrente)
      | otherwise = divisione_ricorsiva resto_aggiornato divisore_normalizzato quoziente_aggiornato
      where
        coefficiente_termine = last resto_corrente / last divisore_normalizzato
        termine_quoziente    = replicate (length resto_corrente - length divisore_normalizzato) 0 ++ [coefficiente_termine]
        resto_aggiornato     = normalizza (differenza resto_corrente (prodotto divisore_normalizzato termine_quoziente))
        quoziente_aggiornato = somma quoziente termine_quoziente

{- La funzione mcd calcola il massimo comun divisore di due polinomi tramite l'algoritmo
   di Euclide, restituendo il risultato reso monico:
   - il primo argomento è la lista dei coefficienti del primo polinomio;
   - il secondo argomento è la lista dei coefficienti del secondo polinomio;
   il risultato è la lista dei coefficienti del polinomio massimo comun divisore. -}
mcd :: [Double] -> [Double] -> [Double]
mcd polinomioA polinomioB = euclide (normalizza polinomioA) (normalizza polinomioB)
  where
    -- La funzione ausiliaria euclide applica ricorsivamente l'algoritmo di Euclide ai due
    -- polinomi, tramite monico, fino a ridurre il secondo polinomio al polinomio nullo:
    -- - il primo argomento è la lista dei coefficienti del primo polinomio;
    -- - il secondo argomento è la lista dei coefficienti del secondo polinomio.
    euclide a [] = monico a
    euclide a b = case divisione a b of
        Nothing        -> monico a
        Just (_, resto) -> euclide b resto

    -- La funzione ausiliaria monico divide tutti i coefficienti per il coefficiente direttore
    -- (l'ultimo della lista):
    -- - il suo unico argomento è la lista dei coefficienti da rendere monica.
    monico [] = []
    monico coeff = map (/ last coeff) coeff
