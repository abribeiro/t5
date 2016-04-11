-- 1 Escreva uma função addSuffix :: String -> [String] -> [String] usando list comprehension, para adicionar um dado sufixo às strings contidas numa lista
addSuffix :: String -> [String] -> [String]
addSuffix suf lis = [ x++suf | x <- lis] 


-- 2 screva uma função countShorts :: [String] -> Int, que receba uma lista de palavras e retorne a quantidade de palavras dessa lista que possuem menos de 5 caracteres. Use recursão.
countShorts1 :: [String] -> Int
countShorts1 [] = 0
countShorts1 (x:xs) = if (length x < 5)
 then 1 + countShorts1 xs
 else 0 + countShorts1 xs


-- 3 Reescreva a função do exercício acima, desta vez usando list comprehension.
countShorts :: [String] -> Int
countShorts lis = length [ x | x <- lis,(length x)<5]


-- 4 Escreva uma função ciclo :: Int -> [Int] -> [Int] que receba um número N e uma lista de inteiros, retornando uma nova lista com N repetições da lista original, conforme o exemplo abaixo:
ciclo :: Int -> [Int] -> [Int]
ciclo 0 _ = []
ciclo n lis = tail lis ++ head lis:ciclo (n-1) lis 


-- 5 Escreva uma função numera :: [String] -> [(Int,String)], que receba uma lista de palavras e retorne outra lista contendo tuplas com as palavras numeradas a partir de 1. Use recursão. Exemplo de uso da função:
numera :: [String] -> [(Int,String)]
numera lis = funcaoaux 1 lis

funcaoaux :: Int -> [String] -> [(Int,String)]
funcaoaux _ [] = []
funcaoaux n (x:xs) = (n, x): funcaoaux (n+1) xs


-- 6 Explique, em forma de comentário, o resultado de cada expressão abaixo.
{- 
a)	Vai gerar uma lista através de compreensão, essa lista deverá ter o seguinte formato [(Int, Int)], isso é representado
	no trecho: (x,y). Esses inteiros serão formados a partir da seguinte logica: x será uma lista de 1 até 5 ([1,2,3,4,5]),
	dessa lista será retirados somente os pares atraz da função even, o y será formado por uma lista que começa em x+1 e vai
	ate 6 e dela e retirada somente os impares através da função odd. A saída será [(2,3),(2,5),(4,5)]. (2,3) será a primeira
	saída pois o 2 é o primeiro par encontrado em x e o primeiro em y pois y começa em x+1, (2,5) segunda saída pois 5 é o
	segundo numero impar de y, (4,5) é a ultima saída se dá por ser o próximo numero par em x e 5 o único numero impar em y já
	que começa em x+1. Sendo essas 3 saídas as únicas que satisfazem a condição explicada inicialmente.

b)	O comando concatena A e B onde A e B recebem uma lista de string cada A recebe:["lazy","big"] e B recebe: ["frog", "dog"],
	para fazer essa concatenação é pegado a o primeiro elemento de A e concatenado com cada elemento de B gerando 2 saidas, após
	isso é concatenado o segundo elemento de A com todos elementos de B, sendo assim é gerada uma saída assim: 
	["lazyfrog","lazydog","bigfrog","bigdog"]

c)	O comando substitui as vogais por “-“da string, para isso A recebe a string ou de é usado o comando de negação “not” para
	comparar com as vogais usando o “elem” assim substituindo as vogais pelo caractere desejado. O comando “concat” concatena as saida
	em uma string senão a resposta sairia como um lista de char assim: ["p-","r-","l-","l-","p-","p-","d-"] com comando  concat une
	esse caractere gerando uma única string assim: “p-r-l-l-p-p-d-"

-}

-- 7 Write a function crossProduct :: [a] -> [b] -> [(a,b)] that takes two lists xs and ys, and returns the list of all possible pairings: [ (x,y) | x <- xs, y <- ys ] without using the above list comprehension. (As an exercise in problem decomposition, try first defining a "helper" function pairWithAll :: a -> [b] -> [(a,b)] that pairs its first argument with each element in its second.)
crossProduct :: [a] -> [b] -> [(a,b)]
crossProduct [] _ = []
crossProduct _ [] = []
crossProduct (x:xs) lis = (pairWithAll x lis) ++ crossProduct xs lis

pairWithAll :: a -> [b] -> [(a,b)]
pairWithAll _ [] = []
pairWithAll a (x:xs) = (a,x) : pairWithAll a xs


-- 8 que receba um número N e um ponto (x,y) e gere uma sequência de N retângulos não sobrepostos.
--fromIntegral :: (Num b, Integral a) => a -> b
genRects :: Int -> (Int,Int) -> [(Float,Float,Float,Float)]
genRects n (i,j) = [((fromIntegral i)+x,fromIntegral j,5.5,5.5) | x <- [0.0,5.5..5.5*(fromIntegral n-1)]]


-- 9 Escreva uma função recursiva que receba uma lista de tuplas e decomponha cada uma delas, gerando uma tupla de listas
func :: [(Int,Int)] -> ([Int],[Int])
func [] = ([],[])
func (x:xs) = (fst x:(fst (func xs)), snd x: (snd (func xs)))


-- 10 Refaça o exercício anterior usando list comprehension.
func2 :: [(Int,Int)] -> ([Int],[Int])
func2 lis = ([fst x | x <- lis],[snd x| x <-lis])


-- 11  
func3 :: [(Int,Int)] -> ([Int],[Int])
func3 lis = (map fst lis,map snd lis)