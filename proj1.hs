--imports
import Data.List
import Data.Set

--

-- Literal -> (letter, exponent)
type Literal = (Char, Int)

-- Monomial -> (coefficient, list literal)
type Monomial = (Int, [Literal])

-- Polynomial -> list Monomial
type Polynomial = [Monomial]

degree :: Monomial -> Int
degree monomial = sum [snd x | x <- snd monomial]

equalLiteral :: Monomial -> Monomial -> Bool -- PQ não pode ser Monomial a => a -> a -> Bool
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
removeZeroCoefficient p1 = [x | x <- p1, fst x /= 0]

sumListMonomials :: [Monomial] -> Monomial
sumListMonomials = foldl1 (\acc x -> if equalLiteral acc x then (fst acc + fst x, snd acc) else error "The literal part of the monomials are different!")

--prodMonomials :: Monomial -> Monomial -> Monomial
--prodMonomials m1 m2 = 

reducePolynomial :: Polynomial -> Polynomial
reducePolynomial [] = []
reducePolynomial (x : xs) = sumListMonomials (x : [y | y <- xs, equalLiteral x y]) : reducePolynomial [y | y <- xs, not (equalLiteral x y)]

normalizePolynomial :: Polynomial -> Polynomial
normalizePolynomial p1 = sortPolynomial (removeZeroCoefficient (reducePolynomial p1))

sumPolynomials :: Polynomial -> Polynomial -> Polynomial
sumPolynomials p1 p2 = normalizePolynomial (p1 ++ p2)
