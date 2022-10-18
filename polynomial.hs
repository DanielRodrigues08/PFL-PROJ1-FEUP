module Polynomial
  ( sum2Polynomials,
    sumPolynomials,
    prod2Polynomials,
    prodPolynomials,
    derivPolynomial,
    parsePolynomial,
    outPolynomial,
  )
where

import Data.List
import Data.Set hiding (filter, map)

-- Literal -> (letter, exponent)
type Literal = (Char, Int)

-- Monomial -> (coefficient, list literal)
type Monomial = (Int, [Literal])

-- Polynomial -> list Monomial
type Polynomial = [Monomial]

degree :: Monomial -> Int
degree monomial = sum [snd x | x <- snd monomial]

equalLiteral :: Monomial -> Monomial -> Bool
equalLiteral monomial1 monomial2 = fromList (snd monomial1) == fromList (snd monomial2)

compareMonomial :: Monomial -> Monomial -> Ordering
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

sortPolynomial :: Polynomial -> Polynomial
sortPolynomial = sortBy compareMonomial

removeZeroCoefficient :: Polynomial -> Polynomial
removeZeroCoefficient = filter (\x -> fst x /= 0)

removeZeroLiterals :: Polynomial -> Polynomial
removeZeroLiterals p1 = [(fst m1, filter (\x -> snd x /= 0) (snd m1)) | m1 <- p1]

sumListMonomials :: [Monomial] -> Monomial
sumListMonomials = foldl1 (\acc x -> if equalLiteral acc x then (fst acc + fst x, snd acc) else error "The literal part of the monomials are different!")

reducePolynomial :: Polynomial -> Polynomial
reducePolynomial [] = []
reducePolynomial (x : xs) = sumListMonomials (x : [y | y <- xs, equalLiteral x y]) : reducePolynomial [y | y <- xs, not (equalLiteral x y)]

normalizePolynomial :: Polynomial -> Polynomial
normalizePolynomial p1 = sortPolynomial . removeZeroLiterals . removeZeroCoefficient $ reducePolynomial p1

sum2Polynomials :: Polynomial -> Polynomial -> Polynomial
sum2Polynomials p1 p2 = normalizePolynomial (p1 ++ p2)

sumPolynomials :: [Polynomial] -> Polynomial
sumPolynomials p1 = normalizePolynomial (foldl1 (++) p1)

reduceProdLiterals :: [Literal] -> [Literal]
reduceProdLiterals [] = []
reduceProdLiterals (x : xs)
  | snd a == 0 = reduceProdLiterals ([y | y <- xs, fst y /= fst x])
  | otherwise = a : reduceProdLiterals ([y | y <- xs, fst y /= fst x])
  where
    a = (fst x, snd x + sum [snd y | y <- xs, fst y == fst x])

prod2Monomials :: Monomial -> Monomial -> Monomial
prod2Monomials m1 m2 = (fst m1 * fst m2, reduceProdLiterals (snd m1 ++ snd m2))

prod2Polynomials :: Polynomial -> Polynomial -> Polynomial
prod2Polynomials p1 p2 = normalizePolynomial [prod2Monomials x y | x <- p1, y <- p2]

prodPolynomials :: [Polynomial] -> Polynomial
prodPolynomials = foldl1 prod2Polynomials

reduceDerivLiterals :: Char -> [Literal] -> [Literal]
reduceDerivLiterals l l1 = [(i, j -1) | (i, j) <- l1, i == l] ++ [(i, j) | (i, j) <- l1, i /= l]

derivMonomial :: Char -> Monomial -> Monomial
derivMonomial l m1 = if any (\n -> n == l) [i | (i, j) <- snd m1] then (fst m1 * head [j | (i, j) <- snd m1, i == l], reduceDerivLiterals l (snd m1)) else (0, [])

derivPolynomial :: Char -> Polynomial -> Polynomial
derivPolynomial l p1 = normalizePolynomial [derivMonomial l x | x <- p1]

parsePolynomial :: String -> Polynomial
parsePolynomial input = []

outPolynomial :: Polynomial -> String
outPolynomial p1 = ""