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

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    pa <- acquisisci_polinomio "A"
    pb <- acquisisci_polinomio "B"
    putStrLn $ "\nPolinomio A:  " ++ mostra pa
    putStrLn $ "Polinomio B:  " ++ mostra pb
    putStrLn $ "Grado A:      " ++ show (grado_polinomio pa)
    putStrLn $ "Grado B:      " ++ show (grado_polinomio pb)
    putStrLn $ "Somma:        " ++ mostra (somma pa pb)
    putStrLn $ "Differenza:   " ++ mostra (differenza pa pb)
    putStrLn $ "Prodotto:     " ++ mostra (prodotto pa pb)
    case divisione pa pb of
      Nothing -> putStrLn "Errore:       impossibile dividere per il polinomio nullo."
      Just (quoz, resto) -> do
         putStrLn $ "Quoziente:    " ++ mostra quoz
         putStrLn $ "Resto:        " ++ mostra resto
    putStrLn $ "MCD:          " ++ mostra (mcd pa pb)

grado_polinomio :: [Double] -> Int
grado_polinomio [] = 0
grado_polinomio coeff = length (normalizza coeff) - 1

{- L'azione parametrica di input/output acquisisci_polinomio acquisisce un polinomio di coefficienti:
   - il suo unico argomento è una stringa che specifica di quale polinomio si tratta. -}
acquisisci_polinomio :: String -> IO [Double]
acquisisci_polinomio etichetta = do
    putStr $ "Inserisci i coefficienti del polinomio " ++ etichetta ++ " separati da spazi (ordine crescente): "
    riga <- getLine
    let token = words riga
    if null token
      then putStrLn "Devi inserire almeno un coefficiente esplicito!" >> acquisisci_polinomio etichetta
      else case mapM readMaybe token of
        Just coeff -> return (normalizza coeff)
        Nothing    -> putStrLn "Formato non valido! Riprova." >> acquisisci_polinomio etichetta

{- La funzione normalizza elimina gli zeri di testa (grado massimo) di un polinomio:
   - il suo unico argomento è la lista dei coefficienti in ordine crescente di grado. -}
normalizza :: [Double] -> [Double]
normalizza = dropWhileEnd (\c -> abs c < tolleranza)

{- La funzione mostra restituisce la rappresentazione algebrica di un polinomio, dal grado massimo al minimo:
   - il suo unico argomento è la lista dei coefficienti in ordine crescente di grado.
   Le funzioni ausiliarie segno e monomio sono usate solo qui, da cui il where. -}
mostra :: [Double] -> String
mostra polinomio = termini (reverse (zip [0..] (normalizza polinomio))) True
  where
    termini :: [(Int, Double)] -> Bool -> String
    termini [] True  = "0"
    termini [] False = ""
    termini ((grado, c) : resto) primo
      | abs c < tolleranza = termini resto primo
      | otherwise = segno c primo ++ monomio grado (abs c) ++ termini resto False

    -- Segno restituisce il segno da anteporre al termine corrente.
    segno :: Double -> Bool -> String
    segno c True  | c < 0     = "-"
                  | otherwise = ""
    segno c False | c < 0     = " - "
                  | otherwise = " + "

    -- Monomio formatta grado e coefficiente, omettendo coefficienti unitari e potenze 0/1.
    monomio :: Int -> Double -> String
    monomio 0 c = formatta_coefficienti c
    monomio 1 c | abs (c - 1) < tolleranza = "x"
                | otherwise                = formatta_coefficienti c ++ "x"
    monomio g c | abs (c - 1) < tolleranza = "x^" ++ show g
                | otherwise                = formatta_coefficienti c ++ "x^" ++ show g

{- La funzione formatta_coefficienti restituisce la rappresentazione testuale di un coefficiente,
   come intero se la parte decimale è trascurabile, altrimenti arrotondato a 4 cifre:
   - il suo unico argomento è il valore del coefficiente. -}
formatta_coefficienti :: Double -> String
formatta_coefficienti x
  | abs (x - fromIntegral (round x :: Int)) < tolleranza = show (round x :: Int)
  | otherwise = show (fromIntegral (round (x * 10000)) / 10000)

{- La funzione somma calcola la somma di due polinomi in tempo lineare O(n). -}
somma :: [Double] -> [Double] -> [Double]
somma pa pb = normalizza (somma' pa pb)
  where
    somma' [] ys = ys
    somma' xs [] = xs
    somma' (x:xs) (y:ys) = (x + y) : somma' xs ys

{- La funzione differenza calcola la differenza tra due polinomi in tempo lineare O(n). -}
differenza :: [Double] -> [Double] -> [Double]
differenza pa pb = normalizza (differenza' pa pb)
  where
    differenza' [] ys = map negate ys
    differenza' xs [] = xs
    differenza' (x:xs) (y:ys) = (x - y) : differenza' xs ys

{- La funzione prodotto calcola il prodotto di due polinomi in modo efficiente, 
   senza sovraccaricare la ricorsione interna di chiamate a normalizza. -}
prodotto :: [Double] -> [Double] -> [Double]
prodotto pa pb = normalizza (prodotto' pa pb)
  where
    prodotto' [] _ = []
    prodotto' _ [] = []
    prodotto' (testa : resto) pb' = 
        somma' (map (* testa) pb') (0 : prodotto' resto pb')
    
    -- Funzione di somma grezza locale per non chiamare normalizza ad ogni passo del prodotto
    somma' [] ys = ys
    somma' xs [] = xs
    somma' (x:xs) (y:ys) = (x + y) : somma' xs ys

{- La funzione divisione calcola quoziente e resto della divisione euclidea tra due polinomi. -}
divisione :: [Double] -> [Double] -> Maybe ([Double], [Double])
divisione dividendo divisore
  | null (normalizza divisore) = Nothing
  | otherwise = Just (divisione_ricorsiva (normalizza dividendo) (normalizza divisore) [])
  where
    -- divisione_ricorsiva esegue la divisione lunga accumulando il quoziente.
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
   di Euclide, restituendo il risultato reso monico. -}
mcd :: [Double] -> [Double] -> [Double]
mcd pa pb = euclide (normalizza pa) (normalizza pb)
  where
    euclide a [] = monico a
    euclide a b = case divisione a b of
        Nothing        -> monico a
        Just (_, resto) -> euclide b resto

    -- monico divide tutti i coefficienti per il coefficiente direttore (l'ultimo della lista)
    monico [] = []
    monico coeff = map (/ last coeff) coeff