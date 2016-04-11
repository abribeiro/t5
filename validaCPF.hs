import Data.Char

isCpfOk :: [Int] -> Bool
isCpfOk cpf = ((funcAux (take 9 cpf) == cpf !! 9) && (funcAux (take 10 cpf) == cpf !! 10))

funcAux :: [Int] -> Int
funcAux cpf = if saida < 2 then 0 else (11 - saida)
   where tam = length cpf
         saida = mod (sum $ zipWith (*) cpf [tam+1,tam..2]) 11


main = do
  let cpf = "01749578077"
      digitos = (map digitToInt cpf)
      result = isCpfOk digitos
  putStrLn (show result)