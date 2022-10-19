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
