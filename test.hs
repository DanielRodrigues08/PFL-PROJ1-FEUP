import Polynomial
import Test.QuickCheck

----------------------------------------norm-------------------------------------

prop_norm1 :: Polynomial -> Bool
prop_norm1 p1 = equal2Polynomial (normalizePolynomial (p1 ++ [(0, [('x', 2)])])) (normalizePolynomial p1)

prop_norm2 :: Polynomial -> Bool
prop_norm2 p1 = equal2Polynomial (normalizePolynomial (p1 ++ [(4, [('x', 0), ('y', 0), ('r', 0)])])) (normalizePolynomial (p1 ++ [((4, []))]))

prop_norm3 :: Bool
prop_norm3 = equal2Polynomial (normalizePolynomial [(9, [('x', 2)]), (2, [('x', 1)]), (5, [('x', 1)])]) [(7, [('x', 1)]), (9, [('x', 2)])]

prop_norm4 :: Bool
prop_norm4 = equal2Polynomial (normalizePolynomial [(-2, [('x', 2)]), (2, [('x', 2)]), (8, [('u', 1), ('i', 0)]), (8, [('u', 1)])]) [(16, [('u', 1)])]

----------------------------------------mult-------------------------------------

prop_prod1 :: Polynomial -> Bool
prop_prod1 p1 = equal2Polynomial (prod2Polynomials p1 []) []

prop_prod2 :: Bool
prop_prod2 = equal2Polynomial (prod2Polynomials [(1, [('x', 1)])] [(1, [('y', 1)])]) [(1, [('x', 1), ('y', 1)])]

prop_prod3 :: Bool
prop_prod3 = equal2Polynomial (prod2Polynomials [(12, [('x', 2)]), (5, [('a', 2)]), (6, [('y', 1)])] [(1, [('x', 3)]), (-5, [('b', 2)]), (2, [('y', 1)])]) [(5, [('x', 3), ('a', 2)]), (12, [('x', 5)]), (-60, [('b', 2), ('x', 2)]), (-25, [('a', 2), ('b', 2)]), (6, [('x', 3), ('y', 1)]), (-30, [('b', 2), ('y', 1)]), (10, [('a', 2), ('y', 1)]), (24, [('x', 2), ('y', 1)]), (12, [('y', 2)])]

----------------------------------------sum-------------------------------------

prop_sum1 :: Polynomial -> Bool
prop_sum1 p1 = equal2Polynomial (sum2Polynomials p1 []) (normalizePolynomial p1)

prop_sum2 :: Bool
prop_sum2 = equal2Polynomial (sum2Polynomials [(1, [('b', 2)])] [(0, [])]) [(1, [('b', 2)])]

prop_sum3 :: Bool
prop_sum3 = equal2Polynomial (sum2Polynomials [(1, [('b', 2)])] [(2, [('c', 1), ('b', 2), ('a', 5)])]) [(2, [('a', 5), ('b', 2), ('c', 1)]), (1, [('b', 2)])]

----------------------------------------deriv-------------------------------------

prop_dev1 :: Polynomial -> Char -> Bool
prop_dev1 p1 a = equal2Polynomial (derivPolynomial a (p1 ++ [(5, [])])) (derivPolynomial a p1)

prop_dev2 :: Polynomial -> Polynomial -> Char -> Bool
prop_dev2 p1 p2 a = equal2Polynomial (derivPolynomial a (sum2Polynomials p1 p2)) (sum2Polynomials (derivPolynomial a p1) (derivPolynomial a p2))

prop_dev3 :: Bool
prop_dev3 = equal2Polynomial (derivPolynomial 'o' [(1, [('o', 0), ('c', 1)]), (-4, [('o', 1)]), (2, [('o', 3), ('r', 3)]), (-4, [('o', 6)])]) [(6, [('o', 2), ('r', 3)]), (-4, []), (-24, [('o', 5)])]

prop_dev4 :: Bool
prop_dev4 = equal2Polynomial (derivPolynomial 'x' [(2, [('x', 0), ('p', 1)]), (-4, [('x', 0)]), (2, []), (-4, [('i', 19)])]) []

