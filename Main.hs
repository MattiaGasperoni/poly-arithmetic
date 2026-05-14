{- |
= Main -- Interfaccia interattiva per l'aritmetica dei polinomi

Ciclo REPL a menu che permette di:
  - inserire i polinomi A e B (con validazione dell'input),
  - eseguire le operazioni aritmetiche su di essi,
  - visualizzare il risultato in forma algebrica leggibile.
-}
module Main where

import System.IO  (hSetBuffering, stdout, BufferMode (..))
import Text.Read  (readMaybe)
import Data.List (dropWhileEnd)


-- ---------------------------------------------------------------------------
-- Tipo e istanze
-- ---------------------------------------------------------------------------
newtype Poly = Poly [Int]

-- | Due polinomi sono uguali se hanno la stessa lista normalizzata.
instance Eq Poly where
  (Poly as) == (Poly bs) = as == bs

{- | Stampa il polinomio in notazione algebrica standard.

     Esempi: @3x^2 - 2x + 1@,  @-x^3 + 5@,  @0@. -}
instance Show Poly where
  show :: Poly -> String
  show p
    | isZero p  = "0"
    | otherwise =
        concat
          . zipWith formatTermine [0 ..]
          . reverse
          . filter ((/= 0) . snd)
          $ zip [0 ..] (toList p)
    where
      -- Formatta il k-esimo termine (in ordine decrescente di grado).
      formatTermine :: Int -> (Int, Int) -> String
      formatTermine idx (espo, coeff) = segno ++ coeffStr ++ espoStr
        where
          -- Il segno: per il primo termine usa "-" o "",
          -- per i successivi usa " - " o " + ".
          segno
            | idx == 0 && coeff < 0 = "-"
            | idx == 0              = ""
            | coeff < 0             = " - "
            | otherwise             = " + "

          -- Il valore assoluto del coefficiente.
          -- Per i termini non costanti, il coefficiente 1 è implicito.
          ac = abs coeff
          coeffStr
            | espo == 0 = show ac   -- termine costante: sempre esplicito
            | ac == 1   = ""        -- 1·xⁿ  → scrive solo xⁿ
            | otherwise = show ac

          -- La parte dell'esponente: "", "x", "x^n".
          espoStr
            | espo == 0 = ""
            | espo == 1 = "x"
            | otherwise = "x^" ++ show espo



-- ---------------------------------------------------------------------------
-- Costruzione e decostruzione
-- ---------------------------------------------------------------------------

{- | Crea un polinomio dalla lista di coefficienti @[a0, a1, …, an]@.
     La lista viene normalizzata rimuovendo gli zeri finali. -}
fromList :: [Int] -> Poly
fromList = Poly . dropWhileEnd (== 0)

-- | Restituisce la lista normalizzata dei coefficienti.
toList :: Poly -> [Int]
toList (Poly cs) = cs

-- | Il polinomio zero.
zeroPoly :: Poly
zeroPoly = Poly []

-- | Il polinomio costante 1.
onePoly :: Poly
onePoly = Poly [1]


-- ---------------------------------------------------------------------------
-- Predicati e misure
-- ---------------------------------------------------------------------------

-- | Restituisce 'True' se il polinomio è il polinomio zero.
isZero :: Poly -> Bool
isZero (Poly []) = True
isZero _         = False

{- | Grado del polinomio.

     __Convenzione__: @degree zeroPoly = -1@ (grado del polinomio zero). -}
degree :: Poly -> Int
degree (Poly []) = -1
degree (Poly cs) = length cs - 1

-- | Coefficiente dominante (del termine di grado massimo).
--   Restituisce 0 per il polinomio zero.
leadCoeff :: Poly -> Int
leadCoeff (Poly []) = 0
leadCoeff (Poly cs) = last cs

{- | Contenuto di un polinomio: MCD (in valore assoluto) di tutti i coefficienti.

     Restituisce 0 per il polinomio zero. -}
content :: Poly -> Int
content (Poly []) = 0
content (Poly cs) = foldl1 gcd (map abs cs)

{- | Parte primitiva: divide ogni coefficiente per il 'content'.

     Il risultato soddisfa @content (primitivePart p) == 1@. -}
primitivePart :: Poly -> Poly
primitivePart p
  | isZero p  = zeroPoly
  | otherwise = fromList $ map (`div` c) (toList p)
  where
    c = content p

-- ---------------------------------------------------------------------------
-- Operazioni aritmetiche: funzioni di utilità interne
-- ---------------------------------------------------------------------------

-- Allunga una lista con zeri a destra fino alla lunghezza n.
padRight :: Int -> [Int] -> [Int]
padRight n xs = xs ++ replicate (max 0 (n - length xs)) 0

-- Moltiplica tutti i coefficienti per la costante intera k.
polyScale :: Int -> Poly -> Poly
polyScale k = fromList . map (* k) . toList


-- ---------------------------------------------------------------------------
-- Stato dell'applicazione
-- ---------------------------------------------------------------------------

-- | Mantiene i due polinomi correnti su cui operare.
data AppState = AppState
  { poliA :: Poly   -- ^ Polinomio A
  , poliB :: Poly   -- ^ Polinomio B
  }

-- | Stato iniziale: entrambi i polinomi sono zero.
statoIniziale :: AppState
statoIniziale = AppState zeroPoly zeroPoly

-- ---------------------------------------------------------------------------
-- Punto d'ingresso
-- ---------------------------------------------------------------------------

main :: IO ()
main = do 
  hSetBuffering stdout NoBuffering
  stampaBanner

  putStrLn   "  (interi separati da spazio, dal termine costante al termine di grado n)"
  putStrLn   "  Esempio:  '1 -2 3'  =>  1 - 2x + 3x^2"
  putStrLn   "  [Invio senza testo = polinomio zero]"
  putStrLn   ""

  poliA <- leggiPolinomio "A"
  poliB <- leggiPolinomio "B"

  putStrLn "--- Polinomi Generati ---"
  stampaStato (AppState  poliA  poliB )

  putStrLn "-- Grado dei Polinomi Generati ---"
  stampaGradi (AppState  poliA  poliB )

  putStrLn "-- Somma dei Polinomi ---\n"
  putStrLn $ show (poliA `polyAdd` poliB) ++ "\n"

  putStrLn "-- Differenza dei Polinomi ---\n"
  putStrLn $ show (poliA `polySub` poliB) ++ "\n"

  putStrLn "-- Prodotto dei Polinomi ---\n"
  putStrLn $ show (poliA `polyMul` poliB) ++ "\n"

  putStrLn "-- Divisione Euclidea ---\n"
  eseguiDivisione (AppState  poliA  poliB )

  putStrLn "-- Massimo Comune Divisore ---\n"
  eseguiMcd (AppState  poliA  poliB )

  putStrLn "\nProgramma terminato. Arrivederci!"


stampaBanner :: IO ()
stampaBanner = mapM_ putStrLn
  [ ""
  , "+------------------------------------------------+"
  , "|          Aritmetica dei Polinomi               |"
  , "|   Progetto d'Esame PLF  -  UniUrb Carlo Bo     |"
  , "+------------------------------------------------+"
  ]

-- ---------------------------------------------------------------------------
-- Acquisizione e validazione dell'input
-- ---------------------------------------------------------------------------

{- | Legge interattivamente un polinomio dall'utente.

     L'utente inserisce i __coefficienti interi separati da spazio__,
     dal termine di grado 0 al termine di grado massimo.

     Esempi:
       @1 -2 3@    =>  1 - 2x + 3x^2
       @0 0 1@     =>  x^2
       @5@         =>  5   (polinomio costante)
       (invio)     =>  0   (polinomio zero)

     In caso di input non valido, la funzione chiede di riprovare. -}
leggiPolinomio :: String -> IO Poly
leggiPolinomio nome = do
  putStrLn $ "Inserisci i coefficienti del polinomio " ++ nome ++ ":"
  riga <- getLine
  case analizzaCoefficienti riga of
    Left err -> do
      putStrLn $ "Input non valido: " ++ err
      putStrLn   "Riprova."
      leggiPolinomio nome
    Right cs -> do
      let p = fromList cs
      putStrLn   ""
      return p

{- | Analizza una stringa come lista di interi separati da spazio.

     Restituisce @Right [Int]@ in caso di successo,
     oppure @Left messaggio_errore@ in caso di fallimento. -}
analizzaCoefficienti :: String -> Either String [Int]
analizzaCoefficienti ""  = Right []
analizzaCoefficienti str =
  case mapM readMaybe (words str) of
    Nothing -> Left "stringa non interpretabile come lista di interi"
    Just cs -> Right cs

-- ---------------------------------------------------------------------------
-- Stampa dello stato e del menu
-- ---------------------------------------------------------------------------

stampaStato :: AppState -> IO ()
stampaStato st = do
  putStrLn ""
  putStrLn $ "  A  =  " ++ show (poliA st)
  putStrLn ""
  putStrLn $ "  B  =  " ++ show (poliB st)
  putStrLn ""

stampaGradi :: AppState -> IO ()
stampaGradi st = do
  putStrLn ""
  putStrLn $ "Grado di A  =  " ++ show (degree (poliA st))
  putStrLn ""
  putStrLn $ "Grado di B  =  " ++ show (degree (poliB st))
  putStrLn ""

-- ---------------------------------------------------------------------------
-- Operazioni con output
-- ---------------------------------------------------------------------------
-- | Somma di due polinomi.
polyAdd :: Poly -> Poly -> Poly
polyAdd (Poly as) (Poly bs) =
  fromList $ zipWith (+) (padRight n as) (padRight n bs)
  where
    n = max (length as) (length bs)

-- | Differenza di due polinomi (@p - q@).
polySub :: Poly -> Poly -> Poly
polySub (Poly as) (Poly bs) =
  fromList $ zipWith (-) (padRight n as) (padRight n bs)
  where
    n = max (length as) (length bs)

{- | Prodotto di due polinomi (convoluzione dei vettori di coefficienti).

     @polyMul p q@ calcola il polinomio il cui coefficiente di grado @k@ è:
     @∑ { p[i] · q[j] | i + j = k }@ -}
polyMul :: Poly -> Poly -> Poly
polyMul p _ | isZero p = zeroPoly
polyMul _ q | isZero q = zeroPoly
polyMul (Poly as) (Poly bs) = fromList
  [ sum [ as !! i * bs !! j
        | i <- [0 .. na - 1]
        , j <- [0 .. nb - 1]
        , i + j == k ]
  | k <- [0 .. na + nb - 2] ]
  where
    na = length as
    nb = length bs


{- | Divisione euclidea /esatta/ tra polinomi a coefficienti interi.

     Restituisce @Just (quoziente, resto)@ tali che:

     @dividendo = divisore · quoziente + resto@

     con @grado(resto) < grado(divisore)@.

     Restituisce @Nothing@ se la divisione non è esatta a qualche passo,
     ovvero se il coefficiente dominante del divisore non divide quello
     del resto corrente.

     __Nota__: la divisione è sempre esatta quando @lc(divisore) = ±1@
     (polinomio monico o antimonico). Per il caso generale su Z[x],
     si può usare la pseudo-divisione (vedi 'polyPseudoRem').

     __Precondizione__: il divisore non deve essere il polinomio zero
     (altrimenti viene sollevata un'eccezione). -}
polyDivMod :: Poly -> Poly -> Maybe (Poly, Poly)
polyDivMod _ d | isZero d = error "polyDivMod: divisione per il polinomio zero"
polyDivMod n d
  | degree n < degree d = Just (zeroPoly, n)
  | otherwise           = go [] n
  where
    lcD = leadCoeff d
    dd  = degree d

    go acc rem
      | degree rem < dd =
          -- Divisione completata: ricostruiamo il quoziente
          Just (fromList (reverse acc), rem)
      | r /= 0 =
          -- Il coefficiente non divide esattamente: divisione non possibile
          Nothing
      | otherwise =
          let e    = degree rem - dd
              -- Monomio q·xᵉ che azzera il termine dominante di rem
              mono = fromList (replicate e 0 ++ [q])
              rem' = polySub rem (polyMul mono d)
          in go (q : acc) rem'
      where
        -- Divisione intera con resto
        (q, r) = leadCoeff rem `divMod` lcD


eseguiDivisione :: AppState -> IO ()
eseguiDivisione st
  | isZero (poliB st) =
      putStrLn "Errore: il divisore B e' il polinomio zero."
  | otherwise =
      case polyDivMod (poliA st) (poliB st) of
        Nothing ->
          mapM_ putStrLn
            [ "Divisione non esatta su Z[x]:"
            , "  il coefficiente dominante di B non divide esattamente"
            , "  i coefficienti del quoziente a ogni passo."
            , "  (Suggerimento: la divisione esatta funziona quando lc(B) = +/-1)\n"
            ]
        Just (q, r) -> do
          putStrLn $ "Quoziente  :  " ++ show q
          putStrLn $ "Resto      :  " ++ show r
          -- Verifica: B*Q + R deve essere uguale ad A
          let verifica = polyAdd (polyMul (poliB st) q) r
          putStrLn $ "Verifica   :  B*Q + R  =  " ++ show verifica
          if verifica == poliA st
            then putStrLn   "             (uguale ad A OK)"
            else putStrLn   "             (ATTENZIONE: diverso da A -- bug?)"



-- ---------------------------------------------------------------------------
-- Pseudo-divisione (per MCD su Z[x])
-- ---------------------------------------------------------------------------

{- | Pseudo-resto di @a@ modulo @b@.

     Calcola il polinomio @r@ tale che esiste un intero @δ ≥ 0@ per cui:

     @lc(b)^δ · a = b · q + r@,   con @grado(r) < grado(b)@.

     A differenza di 'polyDivMod', la pseudo-divisione funziona sempre
     su polinomi a coefficienti interi, senza richiedere divisibilità dei
     coefficienti. Viene usata internamente da 'polyGcd'.

     __Precondizione__: il divisore non deve essere il polinomio zero. -}
polyPseudoRem :: Poly -> Poly -> Poly
polyPseudoRem a b
  | isZero b            = error "polyPseudoRem: divisore zero"
  | degree a < degree b = a
  | otherwise           = polyPseudoRem (passo a) b
  where
    lcB = leadCoeff b
    db  = degree b

    -- Un singolo passo di pseudo-divisione.
    -- Sottrae lc(a)·b·xᵉ da lc(b)·a, azzerando il termine dominante.
    -- Formula: rem' = lcB·rem − lcA·b·xᵉ,  con e = deg(rem) − deg(b)
    passo rem =
      let e   = degree rem - db
          lcA = leadCoeff rem
          xE  = fromList (replicate e 0 ++ [1])   -- il monomio xᵉ
      in polySub
           (polyScale lcB rem)
           (polyScale lcA (polyMul xE b))

-- ---------------------------------------------------------------------------
-- Massimo Comune Divisore
-- ---------------------------------------------------------------------------

{- | Massimo Comune Divisore di due polinomi tramite algoritmo di Euclide.

     Utilizza la pseudo-divisione e la parte primitiva per lavorare
     correttamente su polinomi a coefficienti interi (anello Z[x]).

     Il risultato è espresso come /parte primitiva/ con __coefficiente
     dominante positivo__ (forma canonica). -}
polyGcd :: Poly -> Poly -> Poly
polyGcd p q
  | isZero q  = normalizza p
  | otherwise = polyGcd q (primitivePart (polyPseudoRem p q))
  where
    -- Rende positivo il coefficiente dominante per la forma canonica.
    normalizza r
      | leadCoeff r < 0 = polyScale (-1) r
      | otherwise       = r

eseguiMcd :: AppState -> IO ()
eseguiMcd st
  | isZero (poliA st) && isZero (poliB st) =
      putStrLn "MCD(0, 0) non e' definito."
  | otherwise = do
      let g = polyGcd (poliA st) (poliB st)
      putStrLn $ "MCD(A, B)  =  " ++ show g
