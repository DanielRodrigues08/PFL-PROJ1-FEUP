module Polynomial
  ( sum2Polynomials,
    sumPolynomials,
    prod2Polynomials,
    prodPolynomials,
    derivPolynomial,
    parsePolynomial,
    outPolynomial,
    normalizePolynomial,
  )
where

import Data.Char
import Data.List
--import Data.List.Split
import Data.Set hiding (drop, filter, foldl, foldr, map, null)
import Test.QuickCheck

type Literal = (Char, Int)

type Monomial = (Int, [Literal])

type Polynomial = [Monomial]

--Utils

degree :: Monomial -> Int
degree m = foldl (\acc x -> acc + snd x) 0 (snd m)

lit :: Monomial -> Char
lit m = if length (snd m) > 0 then fst(head(snd m)) else 'a'

compareLiteral :: Literal -> Literal -> Ordering
compareLiteral l1 l2
  | c1 < c2 = LT
  | c1 > c2 = GT
  | d1 < d2 = LT
  | d1 > d2 = GT
  | otherwise = EQ
  where
    d1 = snd l1
    d2 = snd l2
    c1 = fst l1
    c2 = fst l2


compareMonomial :: Monomial -> Monomial -> Ordering
compareMonomial m1 m2
  | d1 > d2 = LT
  | d1 < d2 = GT
  | c1 < c2 = LT
  | c1 > c2 = GT
  | e1 > e2 = LT
  | e1 < e2 = GT
  | otherwise = EQ
  where
    d1 = degree m1
    d2 = degree m2
    c1 = fst m1
    c2 = fst m2
    e1 = lit m1
    e2 = lit m2

sortPolynomial :: Polynomial -> Polynomial
sortPolynomial p1 = sortBy compareMonomial [(fst m1, sortBy compareLiteral (snd m1)) | m1 <-p1 ]

removeZeroCoefficient :: Polynomial -> Polynomial
removeZeroCoefficient = filter (\x -> fst x /= 0)

removeZeroLiterals :: Polynomial -> Polynomial
removeZeroLiterals p1 = [(fst m1, filter (\x -> snd x /= 0) (snd m1)) | m1 <- p1]

equalLiteral :: Monomial -> Monomial -> Bool
equalLiteral m1 m2 = fromList (snd m1) == fromList (snd m2)

sumListMonomials :: [Monomial] -> Monomial
sumListMonomials = foldl1 (\acc x -> if equalLiteral acc x then (fst acc + fst x, snd acc) else error "The literal part of the monomials are different!")

reducePolynomial :: Polynomial -> Polynomial
reducePolynomial [] = []
reducePolynomial (x : xs) = sumListMonomials (x : [y | y <- xs, equalLiteral x y]) : reducePolynomial [y | y <- xs, not (equalLiteral x y)]

normalizePolynomial :: Polynomial -> Polynomial
normalizePolynomial p1 = sortPolynomial . removeZeroLiterals . removeZeroCoefficient . reducePolynomial $ removeZeroLiterals p1

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
derivPolynomial l p1 = normalizePolynomial [derivMonomial l x | x <- normalizePolynomial p1]

outMonomial :: Monomial -> String
outMonomial m = (if fst m == 1 then "" else if fst m == -1 then "-" else show (fst m)) ++ concat [if snd x > 1 then fst x : "^" ++ show (snd x) else fst x : "" | x <- snd m]

outPolynomial :: Polynomial -> String
outPolynomial p1 = foldl (\acc x -> if not (null acc) then (if head x == '-' then acc ++ " - " ++ tail x else acc ++ " + " ++ x) else acc ++ x) "" (map outMonomial p1)

parseLiteral :: String -> [Literal]
parseLiteral "" = []
parseLiteral s
  | length s == 1 = [(head s, 1)]
  | otherwise = a : parseLiteral (dropWhile (not . isAlpha) (tail s))
  where
    a = if s !! 1 /= '^' then (head s, 1) else (head s, read (takeWhile isDigit (drop 2 s)))

parseMonomial :: String -> Monomial
parseMonomial s = (if a == "" then 1 else read a, parseLiteral (drop (length a) s))
  where
    a = takeWhile (not . isAlpha) s

parsePolynomial :: String -> Polynomial
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



----------------------------------------norm-------------------------------------

prop_null_element_nom :: Polynomial -> Bool
prop_null_element_nom p1 = normalizePolynomial (p1++[(0,[('x',2)])])  == normalizePolynomial p1

prop_null_element_nom :: Polynomial -> Bool
prop_null_element_nom p1 = normalizePolynomial (p1++[(0,[('x',2)])])  == normalizePolynomial p1

----------------------------------------mult-------------------------------------


prop_null_element_mult :: Polynomial -> Bool
prop_null_element_mult p1 = prod2Polynomials p1 [] == []

--prop_associativity_mult :: Polynomial -> Polynomial -> Bool
--prop_associativity_mult p1 p2 = prod2Polynomials p1 p2 == prod2Polynomials p2 p1

prop_prod1 ::  Bool
prop_prod1  = prod2Polynomials [(1,[('x',1)])] [(1,[('y',1)])] == [(1,[('x',1),('y',1)])]

--prop_prod2::  Bool
--prop_prod2  = prod2Polynomials [(12,[('x',2)]), (5,[('a',2)]), (6,[('y',1)])] [(1,[('x',3)]), (-5,[('b',2)]), (2,[('y',1)])] ==
  --[(5,[('x',3),('a',2)]), (12,[('x',5)]), (-60,[('b',2),('x',2)]), (-25,[('a',2),('b',2)]), (6,[('x',3),('y',1)]),
   --(-30,[('b',2),('y',1)]), (10,[('a',2),('y',1)]), (24,[('x',2),('y',1)]), (12,[('y',2)])]


----------------------------------------sum-------------------------------------

--prop_null_element_sum :: Polynomial -> Bool
--prop_null_element_sum p1 = sum2Polynomials p1 [] == normalizePolynomial p1

prop_associativity_sum :: Polynomial -> Polynomial -> Bool
prop_associativity_sum p1 p2 = sum2Polynomials p1 p2 == sum2Polynomials p2 p1

prop_sum1 ::  Bool
prop_sum1  = sum2Polynomials [(1,[('b',2)])] [(0,[])] == [(1,[('b',2)])]

prop_sum2 ::  Bool
prop_sum2  = sum2Polynomials [(1,[('b',2)])] [(2,[('c',1),('b',2),('a',5)])] == [ (2,[('a',5),('b',2),('c',1)]), (1,[('b',2)])]

--prop_sum1 ::  Bool
--prop_sum1  = sum2Polynomials [(1,[('b',2)])] [(0,[])] == [(1,[('b',2)])]


----------------------------------------deriv-------------------------------------

prop_null_elem_deriv :: Polynomial -> Char -> Bool
prop_null_elem_deriv p1 a = derivPolynomial a ( p1 ++ [(5,[])]) ==  derivPolynomial a p1

prop_sum_deriv :: Polynomial -> Polynomial -> Char -> Bool
prop_sum_deriv p1 p2 a = derivPolynomial a (sum2Polynomials p1 p2) == sum2Polynomials( derivPolynomial a p1) (derivPolynomial a p2)
