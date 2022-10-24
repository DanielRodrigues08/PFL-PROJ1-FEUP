module Polynomial
  ( sum2Polynomials,
    sumPolynomials,
    prod2Polynomials,
    prodPolynomials,
    derivPolynomial,
    parsePolynomial,
    outPolynomial,
    normalizePolynomial,
    equal2Polynomial,
    Polynomial,
  )
where

import Data.Char
import Data.List
import Data.Set hiding (drop, filter, foldl, foldr, map, null)
import PFL2022TP1Tests

---------------------------- Types -------------------------------

type Literal = (Char, Int)

type Monomial = (Int, [Literal])

type Polynomial = [Monomial]

------------------------------- Utils ------------------------------

degree :: Monomial -> Int -- Calcula o grau de um monómio
degree m = foldl (\acc x -> acc + snd x) 0 (snd m)

compareMonomial :: Monomial -> Monomial -> Ordering -- Compara dois monómios
compareMonomial m1 m2
  | d1 > d2 = LT
  | d1 < d2 = GT
  | c1 > c2 = LT
  | c1 < c2 = GT
  | otherwise = EQ
  where
    d1 = degree m1
    d2 = degree m2
    c1 = fst m1
    c2 = fst m2

sortPolynomial :: Polynomial -> Polynomial -- Ordena um polinómio
sortPolynomial = sortBy compareMonomial

equalLiteral :: Monomial -> Monomial -> Bool -- Verifica se a parte literal de dois monómios são iguais
equalLiteral m1 m2 = fromList (snd m1) == fromList (snd m2)

equalMonomial :: Monomial -> Monomial -> Bool -- Verficia se dois monómios são iguais
equalMonomial m1 m2 = fst m1 == fst m2 && equalLiteral m1 m2

equal2Polynomial :: Polynomial -> Polynomial -> Bool -- Verifica se dois polinómios são iguais
equal2Polynomial p1 p2 = length p1 == length p2 && foldl (&&) True (zipWith equalMonomial (sortPolynomial p1) (sortPolynomial p2))

------------------------------- normalize -----------------------------------

reduceLiterals :: [Literal] -> [Literal] -- Simplifica a parte literal de dois monómios, removendo as letras cujo o expoente é zero e adicionando os expoentes de letras iguais
reduceLiterals [] = []
reduceLiterals (x : xs) = if snd a /= 0 then a : b else b
  where
    a = (fst x, snd x + sum [snd y | y <- xs, fst y == fst x])
    b = reduceLiterals ([y | y <- xs, fst y /= fst x])

reduceLiteralsPolynomial :: Polynomial -> Polynomial -- Aplica função "reduceLiterals" num polinómio
reduceLiteralsPolynomial p1 = [(fst y, reduceLiterals (snd y)) | y <- p1, fst y /= 0]

reducePolynomial :: Polynomial -> Polynomial -- Simplifica um monómio adicionando monómios cuja a parte é igual e removendo aqueles que têm coefeciente nulo
reducePolynomial [] = []
reducePolynomial (x : xs) = if fst a /= 0 then a : b else b
  where
    a = sumListMonomials (x : [y | y <- xs, equalLiteral x y])
    b = reducePolynomial [y | y <- xs, not (equalLiteral x y)]

normalizePolynomial :: Polynomial -> Polynomial -- Normaliza um polinómio
normalizePolynomial = sortPolynomial . reducePolynomial . reduceLiteralsPolynomial

-------------------------------- sum ----------------------------------------

sumListMonomials :: [Monomial] -> Monomial -- Soma uma lista de monómios
sumListMonomials = foldl1 (\acc x -> if equalLiteral acc x then (fst acc + fst x, snd acc) else error "The literal part of the monomials are different!")

sum2Polynomials :: Polynomial -> Polynomial -> Polynomial -- Soma dois polinómios
sum2Polynomials p1 p2 = normalizePolynomial (p1 ++ p2)

sumPolynomials :: [Polynomial] -> Polynomial -- Soma uma lista de polinómios
sumPolynomials p1 = normalizePolynomial (foldl1 (++) p1)

-------------------------------- prod ---------------------------------------

reduceProdLiterals :: [Literal] -> [Literal] --Multiplica a parte literal de dois monómios e simplifica-a
reduceProdLiterals [] = []
reduceProdLiterals (x : xs)
  | snd a == 0 = reduceProdLiterals ([y | y <- xs, fst y /= fst x])
  | otherwise = a : reduceProdLiterals ([y | y <- xs, fst y /= fst x])
  where
    a = (fst x, snd x + sum [snd y | y <- xs, fst y == fst x])

prod2Monomials :: Monomial -> Monomial -> Monomial -- Multiplica dois monómios
prod2Monomials m1 m2 = (fst m1 * fst m2, reduceProdLiterals (snd m1 ++ snd m2))

prod2Polynomials :: Polynomial -> Polynomial -> Polynomial -- Multiplica dois polinómios
prod2Polynomials p1 p2 = normalizePolynomial [prod2Monomials x y | x <- p1, y <- p2]

prodPolynomials :: [Polynomial] -> Polynomial -- Multiplica uma lista de polinómios
prodPolynomials p1
  | null p1 = []
  | otherwise = foldl1 prod2Polynomials p1

-------------------------------- deriv --------------------------------------

reduceDerivLiterals :: Char -> [Literal] -> [Literal] -- Modifica a parte literal do monómio consoante a derivação do monómio
reduceDerivLiterals l l1 = [(i, j -1) | (i, j) <- l1, i == l] ++ [(i, j) | (i, j) <- l1, i /= l]

derivMonomial :: Char -> Monomial -> Monomial -- Deriva um monómio em função de uma variável
derivMonomial l m1 = if any (== l) [i | (i, j) <- snd m1] then (fst m1 * head [j | (i, j) <- snd m1, i == l], reduceDerivLiterals l (snd m1)) else (0, [])

derivPolynomial :: Char -> Polynomial -> Polynomial -- Deriva um polinómio em função de uma variável
derivPolynomial l p1 = normalizePolynomial [derivMonomial l x | x <- normalizePolynomial p1]

-------------------------------- Parse --------------------------------------

outMonomial :: Monomial -> String -- Transforma um monómio na representação interna numa string
outMonomial m = (if fst m == 1 then "" else if fst m == -1 then "-" else show (fst m)) ++ concat [if snd x > 1 then fst x : "^" ++ show (snd x) else fst x : "" | x <- snd m]

outPolynomial :: Polynomial -> String -- Transforma um monómio na representação interna numa string
outPolynomial p1 = foldl (\acc x -> if not (null acc) then (if head x == '-' then acc ++ " - " ++ tail x else acc ++ " + " ++ x) else acc ++ x) "" (map outMonomial p1)

parseLiteral :: String -> [Literal] -- -- Converte uma string que contém uma lista de literais
parseLiteral "" = []
parseLiteral s
  | length s == 1 = [(head s, 1)]
  | otherwise = a : parseLiteral (dropWhile (not . isAlpha) (tail s))
  where
    a = if s !! 1 /= '^' then (head s, 1) else (head s, read (takeWhile isDigit (drop 2 s)))

parseMonomial :: String -> Monomial -- Converte uma string que contém um monómio
parseMonomial s = (if a == "" then 1 else read a, parseLiteral (drop (length a) s))
  where
    a = takeWhile (not . isAlpha) s

parsePolynomial :: String -> Polynomial -- -- Converte uma string que contém um polinómio
parsePolynomial p1 = parsePolynomialAux (filter (/= ' ') p1)

parsePolynomialAux :: String -> Polynomial
parsePolynomialAux "" = []
parsePolynomialAux p1 = parseMonomial a : parsePolynomialAux (drop b p1)
  where
    a
      | head p1 == '+' = takeWhile (\x -> (x /= '+') && (x /= '-')) (tail p1)
      | head p1 == '-' = '-' : takeWhile (\x -> (x /= '+') && (x /= '-')) (tail p1)
      | otherwise = takeWhile (\x -> (x /= '+') && (x /= '-')) p1
    b = if head p1 == '+' || head p1 == '-' then length a + 1 else length a
