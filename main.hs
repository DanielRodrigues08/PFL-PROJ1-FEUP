import Polynomial

main :: IO ()
main = do
  putStrLn "Operation? (sum, prod or deriv)"
  operator <- getLine
  putStrLn "Poly 1?"
  poly1 <- getLine
  if operator == "deriv"
   then do
      putStrLn "Letter?"
      letter <- getChar
      putStrLn $ "Result: " ++ outPolynomial (derivPolynomial letter (parsePolynomial poly1))
    else do
      putStrLn "Poly2 ?"
      poly2 <- getLine
      if operator == "sum"
        then do
          putStrLn $ "Result: " ++ outPolynomial (sum2Polynomials (parsePolynomial poly1) (parsePolynomial poly2))
        else
          if operator == "prod"
            then do
              putStrLn $ "Result: " ++ outPolynomial (prod2Polynomials (parsePolynomial poly1) (parsePolynomial poly2))
            else putStrLn "Invalid Operation!"

test1 :: IO ()
test1 = do
  putStrLn "Operation 1 -  Normalize"
  putStrLn "Operator: 2x^2 + 4o^2l + 6x^2 - 6p^7 + 0m^4 + 30x^0 - 5y^0v^8"
  putStrLn $ "Result: " ++ outPolynomial (normalizePolynomial (parsePolynomial "2x^2 + 4o^2l + 6x^2 - 6p^7 + 0m^4 + 30x^0 - 5y^0v^8"))
  putStrLn "Operation 2 -  Sum"
  putStrLn "Operator 1: 2x^2 + 4o^2l + 6x^2 - 6p^7 + 0m^4 + 30x^0 - 5y^0v^8"
  putStrLn "Operator 2: -8x^2 + 6x^2 + 6p^7 + 9m^4 + 5x^0 - 5y^1v^8"
  putStrLn $ "Result: " ++ outPolynomial (sum2Polynomials (parsePolynomial "2x^2 + 4o^2l + 6x^2 - 6p^7 + 0m^4 + 30x^0 - 5y^0v^8") (parsePolynomial "-8x^2 + 6x^2 + 6p^7 + 9m^4 + 5x^0 - 5y^1v^8"))
  putStrLn "Operation 3 -  Multiplication"
  putStrLn "Operator 1: 2x^2 + 4o^2l + 6x^2 - 6p^7 + 0m^4 + 30x^0 "
  putStrLn "Operator 2: -8x^2 + 6x^2 + 9m^4 + 5x^0 "
  putStrLn $ "Result: " ++ outPolynomial (prod2Polynomials (parsePolynomial "2x^2 + 4o^2l + 6x^2 - 6p^7 + 0m^4 + 30x^0 ") (parsePolynomial "-8x^2 + 6x^2 + 9m^4 + 5x^0"))
  putStrLn "Operation 4 -  Derivative"
  putStrLn "Variable: o "
  putStrLn "Operator 1: 2x^2 + 4o^2l + 6o^9 - 6p^7 + 0o^4 + 30x^0 "
  putStrLn $ "Result: " ++ outPolynomial (derivPolynomial 'o' (parsePolynomial "2x^2 + 4o^2l + 6o^9 - 6p^7 + 0o^4 + 30x^0 "))

test2 :: IO ()
test2 = do
  putStrLn "Operation 1 -  Normalize"
  putStrLn "Operator: [(2,[('x',7)]), (0,[('x',7),('i',2)]), (2,[('x',0),('u',0),('p',5)]), (-5,[('x',7)]), (7,[('j',7),('i',2)])]"
  putStrLn $ "Result: " ++ show (normalizePolynomial  [(2,[('x',7)]), (0,[('x',7),('i',2)]), (2,[('x',0),('u',0),('p',5)]), (-5,[('x',7)]), (7,[('j',7),('i',2)])])
  putStrLn "Operation 2 -  Sum"
  putStrLn "Operator 1: [(-5,[('s',0),('x',3)]),  (7,[('j',1),('i',0),('p',4)])]"
  putStrLn "Operator 2: [(2,[('x',3)]),  (2,[('j',0),('i',0),('p',4)]), (5,[('p',4)]), (-5,[('j',1),('i',0),('p',4)])]"
  putStrLn $ "Result: " ++ show (sum2Polynomials [(-5,[('s',0),('x',3)]),  (7,[('j',1),('i',0),('p',4)])] [(2,[('x',3)]),  (2,[('j',0),('i',0),('p',4)]), (5,[('p',4)]), (-5,[('j',1),('i',0),('p',4)]) ])
  putStrLn "Operation 3 -  Multiplication"
  putStrLn "Operator 1: [(-7,[('k',0),('x',3)]),  (3,[('j',1)]), (1,[('j',1)]), (-5,[])]"
  putStrLn "Operator 2: [(2 ,[('t',6),('x',3)]),  (0,[('m',4)]), (1,[('j',4)])]"
  putStrLn $ "Result: " ++ show (prod2Polynomials  [(-7,[('k',0),('x',3)]),  (3,[('j',1)]), (1,[('j',1)]), (-5,[])] [(2 ,[('t',6),('x',3)]),  (0,[('m',4)]), (1,[('j',4)])])
  putStrLn "Operation 4 -  Derivative"
  putStrLn "Variable: l "
  putStrLn "Operator 1: [(2,[('l',7)]), (0,[('l',7),('i',2)]), (8,[('x',0),('u',0),('p',5)]), (-5,[('l',2)]), (2,[('l',2)]), (7,[('l',7),('i',2)])]"
  putStrLn $ "Result: " ++ show (derivPolynomial 'l' [(2,[('l',7)]), (0,[('l',7),('i',2)]), (8,[('x',0),('u',0),('p',5)]), (-5,[('l',2)]), (2,[('l',2)]), (7,[('l',7),('i',2)])])
