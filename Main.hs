module Main where
 
import System.IO (hSetBuffering, stdout, BufferMode (..))
import Text.Read (readMaybe)
import Data.List (dropWhileEnd)
 
-- ---------------------------------------------------------------------------
-- Utility e Normalizzazione
-- ---------------------------------------------------------------------------
 
-- Definisce un limite per considerare un coefficiente come zero
limite :: Double
limite = 1e-6
 
-- Rimuove i coefficienti finali che sono zero o assimilabili a zero
normalizza :: [Double] -> [Double]
normalizza = dropWhileEnd (\c -> abs c < limite)
 
-- Calcola il grado del polinomio
calcoloGrado :: [Double] -> Int
calcoloGrado p = length (normalizza p) - 1
 
-- Converte il numero in una stringa, rimuovendo la parte decimale se è un intero
converti :: Double -> String
converti x
  | abs (x - fromIntegral (round x :: Int)) < limite = show (round x :: Int)
  | otherwise = show x
 
-- ---------------------------------------------------------------------------
-- Visualizzazione
-- ---------------------------------------------------------------------------
 
-- Stampa un polinomio in formato leggibile
stampaPolinomio :: [Double] -> String
stampaPolinomio p
    | null np   = "0"
    | otherwise = format (reverse (zip [0..] np)) True
  where
    np = normalizza p
 
    format [] _ = ""
    format ((e, c):xs) isFirst =
        sign ++ coeff ++ variable ++ format xs False
      where
        -- Gestione del segno
        sign | isFirst   = if c < 0 then "-" else ""
             | c > 0     = " + "
             | otherwise = " - "
 
        absC = abs c
 
        -- Gestione coefficiente (nascondi 1 se c'è la x)
        coeff | absC /= 1 || e == 0 = converti absC
              | otherwise            = ""
 
        -- Gestione x e potenze
        variable | e == 0    = ""
                 | e == 1    = "x"
                 | otherwise = "x^" ++ show e
 
-- ---------------------------------------------------------------------------
-- Aritmetica Polinomiale
-- ---------------------------------------------------------------------------
 
-- Funzione di supporto per zipWith che gestisce liste di lunghezza diversa
zipWithAll :: (Double -> Double -> Double) -> [Double] -> [Double] -> [Double]
zipWithAll _ [] []         = []
zipWithAll f (x:xs) []    = f x 0 : zipWithAll f xs []
zipWithAll f [] (y:ys)    = f 0 y : zipWithAll f [] ys
zipWithAll f (x:xs) (y:ys) = f x y : zipWithAll f xs ys
 
addizione :: [Double] -> [Double] -> [Double]
addizione xs ys = normalizza $ zipWithAll (+) xs ys
 
sottrazione :: [Double] -> [Double] -> [Double]
sottrazione xs ys = normalizza $ zipWithAll (-) xs ys
 
moltiplicazione :: [Double] -> [Double] -> [Double]
moltiplicazione [] _ = []
moltiplicazione (a:as) bs =
    normalizza $ zipWithAll (+) (map (*a) bs) (0 : moltiplicazione as bs)
 
-- Divisione euclidea con resto
divisioneConResto :: [Double] -> [Double] -> ([Double], [Double])
divisioneConResto n d
    | null nd   = error "Divisione per zero"
    | gN < gD   = ([], nn)
    | otherwise =
        let diff              = gN - gD
            qCoeff            = (nn !! gN) / (nd !! gD)
            qMonomio          = replicate diff 0 ++ [qCoeff]
            restoParziale     = sottrazione n (moltiplicazione qMonomio d)
            (qRest, rFinal)   = divisioneConResto restoParziale d
        in (addizione qMonomio qRest, rFinal)
  where
    nn = normalizza n
    nd = normalizza d
    gN = calcoloGrado n
    gD = calcoloGrado d
 
calcoloMCD :: [Double] -> [Double] -> [Double]
calcoloMCD a b
    | null nb   = monico na
    | otherwise = calcoloMCD nb (snd $ divisioneConResto na nb)
  where
    na = normalizza a
    nb = normalizza b
    monico [] = []
    monico xs = map (/ last xs) xs
 
-- ---------------------------------------------------------------------------
-- Main e I/O
-- ---------------------------------------------------------------------------
 
leggi :: String -> IO [Double]
leggi nome = do
    putStr $ "Inserisci coeff " ++ nome ++ " (ordine crescente): "
    line <- getLine
    case mapM readMaybe (words line) of
        Just coeffs -> return (normalizza coeffs)
        Nothing     -> putStrLn "Errore input!" >> leggi nome
 
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    pA <- leggi "A"
    pB <- leggi "B"
    putStrLn $ "\nA: "       ++ stampaPolinomio pA
    putStrLn $ "B: "         ++ stampaPolinomio pB
    putStrLn $ "Somma: "     ++ stampaPolinomio (addizione pA pB)
    putStrLn $ "Prodotto: "  ++ stampaPolinomio (moltiplicazione pA pB)
    let (q, r) = divisioneConResto pA pB
    putStrLn $ "Quoziente: " ++ stampaPolinomio q
    putStrLn $ "Resto: "     ++ stampaPolinomio r
    putStrLn $ "MCD: "       ++ stampaPolinomio (calcoloMCD pA pB)
