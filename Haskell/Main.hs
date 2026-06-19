{- Haskell program for computing polynomial arithmetic operations. -}
 
import Text.Read  (readMaybe)
import Data.List  (dropWhileEnd)
import Data.Ratio (Rational, numerator, denominator)
 
{- Type alias for a polynomial represented as a list of rational coefficients
   in ascending order of degree (index 0 = constant term). -}
type Polynomial = [Rational]
 
{- Function normalizza removes trailing zero coefficients from a polynomial:
   - its only argument is the polynomial to normalize. -}
normalizza :: Polynomial -> Polynomial
normalizza = dropWhileEnd (== 0)
 
{- Function calcoloGrado computes the degree of a polynomial:
   - its only argument is the polynomial whose degree is to be computed. -}
calcoloGrado :: Polynomial -> Int
calcoloGrado = subtract 1 . length . normalizza
 
{- Function convertiR converts a rational number to a readable string:
   - its only argument is the rational number to convert;
   - a whole number is printed without denominator (e.g. 3 % 1 -> "3");
   - a fraction is printed as numerator/denominator (e.g. 1 % 2 -> "1/2"). -}
convertiR :: Rational -> String
convertiR r
    | denominator r == 1 = show (numerator r)
    | otherwise          = show (numerator r) ++ "/" ++ show (denominator r)
 
{- Function stampaPolinomio formats a polynomial as a human-readable string:
   - its only argument is the polynomial to format;
   - the zero polynomial is printed as "0";
   - coefficients equal to 1 or -1 are omitted when multiplying a variable;
   - terms are printed from the highest degree to the constant term. -}
stampaPolinomio :: Polynomial -> String
stampaPolinomio p
    | null np   = "0"
    | otherwise = format (reverse (zip [0..] np)) True
  where
    np = normalizza p
    format [] _               = ""
    format ((e, c) : xs) isFirst =
        sign ++ coeff ++ variable ++ format xs False
      where
        sign | isFirst   = if c < 0 then "-" else ""
             | c > 0     = " + "
             | otherwise = " - "
        absC = abs c
        coeff    | absC /= 1 || e == 0 = convertiR absC
                 | otherwise           = ""
        variable | e == 0              = ""
                 | e == 1              = "x"
                 | otherwise           = "x^" ++ show e
 
{- Function zipWithAll applies a binary operation to two polynomials
   coefficient by coefficient, padding the shorter one with zeros:
   - the first argument is the binary operation to apply;
   - the second argument is the first polynomial;
   - the third argument is the second polynomial. -}
zipWithAll :: (Rational -> Rational -> Rational) -> Polynomial -> Polynomial -> Polynomial
zipWithAll _ []     []     = []
zipWithAll f (x:xs) []     = f x 0 : zipWithAll f xs []
zipWithAll f []     (y:ys) = f 0 y : zipWithAll f [] ys
zipWithAll f (x:xs) (y:ys) = f x y : zipWithAll f xs ys
 
{- Function addizione computes the sum of two polynomials:
   - the first argument is the first polynomial;
   - the second argument is the second polynomial. -}
addizione :: Polynomial -> Polynomial -> Polynomial
addizione xs ys = normalizza $ zipWithAll (+) xs ys
 
{- Function sottrazione computes the difference of two polynomials:
   - the first argument is the first polynomial;
   - the second argument is the second polynomial. -}
sottrazione :: Polynomial -> Polynomial -> Polynomial
sottrazione xs ys = normalizza $ zipWithAll (-) xs ys
 
{- Function moltiplicazione computes the product of two polynomials:
   - the first argument is the first polynomial;
   - the second argument is the second polynomial. -}
moltiplicazione :: Polynomial -> Polynomial -> Polynomial
moltiplicazione []     _ = []
moltiplicazione (a:as) bs =
    normalizza $ zipWithAll (+) (map (* a) bs) (0 : moltiplicazione as bs)
 
{- Function divisioneConResto computes the Euclidean division of two polynomials,
   returning the quotient and the remainder as an exact pair using rational arithmetic:
   - the first argument is the dividend polynomial;
   - the second argument is the divisor polynomial. -}
divisioneConResto :: Polynomial -> Polynomial -> (Polynomial, Polynomial)
divisioneConResto n d
    | null nd   = error "Division by zero polynomial"
    | gN < gD   = ([], nn)
    | otherwise =
        let diff            = gN - gD
            qCoeff          = (nn !! gN) / (nd !! gD)
            qMonomio        = replicate diff 0 ++ [qCoeff]
            restoParziale   = sottrazione n (moltiplicazione qMonomio d)
            (qRest, rFinal) = divisioneConResto restoParziale d
        in  (addizione qMonomio qRest, rFinal)
  where
    nn = normalizza n
    nd = normalizza d
    gN = calcoloGrado n
    gD = calcoloGrado d
 
{- Function calcoloMCD computes the monic greatest common divisor of two polynomials
   using the Euclidean algorithm:
   - the first argument is the first polynomial;
   - the second argument is the second polynomial. -}
calcoloMCD :: Polynomial -> Polynomial -> Polynomial
calcoloMCD a b
    | null nb   = monico na
    | otherwise = calcoloMCD nb (snd $ divisioneConResto na nb)
  where
    na = normalizza a
    nb = normalizza b
    monico []  = []
    monico xs  = map (/ last xs) xs
 
{- The parametric input/output action leggi reads a polynomial from the keyboard
   as a sequence of integer coefficients in ascending order of degree:
   - its only argument is a string labeling the polynomial being read. -}
leggi :: String -> IO Polynomial
leggi nome = do
    putStr $ "Inserisci i coefficienti del polinomio " ++ nome ++ " (ordine crescente, interi): "
    line <- getLine
    case mapM (fmap toRational . (readMaybe :: String -> Maybe Int)) (words line) of
        Just coeffs -> return (normalizza coeffs)
        Nothing     -> do putStrLn ">> Errore: inserire solo numeri interi separati da spazi."
                          leggi nome
 
main :: IO ()
main = do
    pA <- leggi "A"
    pB <- leggi "B"
    putStrLn "Polinomio A:"
    putStrLn $ show (stampaPolinomio pA)
    putStrLn "Polinomio B:"
    putStrLn $ show (stampaPolinomio pB)
    putStrLn "Somma:"
    putStrLn $ show (stampaPolinomio (addizione pA pB))
    putStrLn "Prodotto:"
    putStrLn $ show (stampaPolinomio (moltiplicazione pA pB))
    putStrLn "Quoziente:"
    putStrLn $ show (stampaPolinomio (fst (divisioneConResto pA pB)))
    putStrLn "Resto:"
    putStrLn $ show (stampaPolinomio (snd (divisioneConResto pA pB)))
    putStrLn "MCD:"
    putStrLn $ show (stampaPolinomio (calcoloMCD pA pB))
