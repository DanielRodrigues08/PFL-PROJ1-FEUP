import Polynomial
import Test.QuickCheck

----------------------------------------norm-------------------------------------

prop_null_element_nom :: Polynomial -> Bool
prop_null_element_nom p1 = normalizePolynomial (p1 ++ [(0, [('x', 2)])]) == normalizePolynomial p1

prop_null_element_nom :: Polynomial -> Bool
prop_null_element_nom p1 = normalizePolynomial (p1 ++ [(0, [('x', 2)])]) == normalizePolynomial p1

----------------------------------------mult-------------------------------------

prop_null_element_mult :: Polynomial -> Bool
prop_null_element_mult p1 = prod2Polynomials p1 [] == []

--prop_associativity_mult :: Polynomial -> Polynomial -> Bool
--prop_associativity_mult p1 p2 = prod2Polynomials p1 p2 == prod2Polynomials p2 p1

prop_prod1 :: Bool
prop_prod1 = prod2Polynomials [(1, [('x', 1)])] [(1, [('y', 1)])] == [(1, [('x', 1), ('y', 1)])]

--prop_prod2::  Bool
--prop_prod2  = prod2Polynomials [(12,[('x',2)]), (5,[('a',2)]), (6,[('y',1)])] [(1,[('x',3)]), (-5,[('b',2)]), (2,[('y',1)])] ==
--[(5,[('x',3),('a',2)]), (12,[('x',5)]), (-60,[('b',2),('x',2)]), (-25,[('a',2),('b',2)]), (6,[('x',3),('y',1)]),
--(-30,[('b',2),('y',1)]), (10,[('a',2),('y',1)]), (24,[('x',2),('y',1)]), (12,[('y',2)])]

----------------------------------------sum-------------------------------------

--prop_null_element_sum :: Polynomial -> Bool
--prop_null_element_sum p1 = sum2Polynomials p1 [] == normalizePolynomial p1

prop_associativity_sum :: Polynomial -> Polynomial -> Bool
prop_associativity_sum p1 p2 = sum2Polynomials p1 p2 == sum2Polynomials p2 p1

prop_sum1 :: Bool
prop_sum1 = sum2Polynomials [(1, [('b', 2)])] [(0, [])] == [(1, [('b', 2)])]

prop_sum2 :: Bool
prop_sum2 = sum2Polynomials [(1, [('b', 2)])] [(2, [('c', 1), ('b', 2), ('a', 5)])] == [(2, [('a', 5), ('b', 2), ('c', 1)]), (1, [('b', 2)])]

--prop_sum1 ::  Bool
--prop_sum1  = sum2Polynomials [(1,[('b',2)])] [(0,[])] == [(1,[('b',2)])]

----------------------------------------deriv-------------------------------------

prop_null_elem_deriv :: Polynomial -> Char -> Bool
prop_null_elem_deriv p1 a = derivPolynomial a (p1 ++ [(5, [])]) == derivPolynomial a p1

prop_sum_deriv :: Polynomial -> Polynomial -> Char -> Bool
prop_sum_deriv p1 p2 a = derivPolynomial a (sum2Polynomials p1 p2) == sum2Polynomials (derivPolynomial a p1) (derivPolynomial a p2)
