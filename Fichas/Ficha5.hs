module Ficha5 where

import Data.List

-- 1.
-- a) Testar se um predicado é verdade para algum elemento de uma lista

any':: (a -> Bool) -> [a] -> Bool
any' pred [] = False
any' pred (h:t) = pred h || any' pred t

-- b) Combina os elementos de duas listas usando a função zipWith

zipWith':: (a -> b-> c) -> [a] -> [b] -> [c]
zipWith' fun [] _ = []
zipWith' fun _ [] = []
zipWith' fun (h:t) (x:xs) = fun h x:zipWith' fun t xs

-- c) Determina os primeiros elementos da lista que satisfazem um dado predicado

takeWhite:: (a -> Bool) -> [a] -> [a]
takeWhite _ [] = []
takeWhite pred (h:t) | pred h = h:takeWhite pred t 
                     | otherwise = []

-- d) Elimina os primeiros elementos da lista que satisfazem um dado predicado

dropWhite:: (a -> Bool) -> [a] -> [a]
dropWhite _ [] = []
dropWhite pred (h:t) | pred h = dropWhite pred t
                     | otherwise = (h:t)

-- e) Calcula simultaneamente o takeWhile e o dropWhile devolvendo um tuplo
{-
spam:: (a -> Bool) -> [a] -> ([a],[a])
spam pred _ = ([],[])
spam pred (h:t) = 
-}

-- f) Apaga o primeiro elemento de uma lista que é igual a um dado elemento de acordo com a função de comparação que é passada como parâmetro

deleteBye:: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBye _ _ [] = []
deleteBye f x (h:t) | f x h = t
                    | otherwise = h:deleteBye f x t

-- g) Ordena uma lista comparando os resultados de aplicar uma função de extração de uma chave a cada elemento de uma lista.

sortOff:: Ord b => (a -> b) -> [a] -> [a]
sortOff f [] = []
sortOff _ [x] = [x]
sortOff f (x:y:t) | f x >= f y = y:sortOff f (x:t)
                  | otherwise = x:y:sortOff f t

-- ou

sorte:: Ord b => (a -> b) -> [a] -> [a]
sorte f [] = []
sorte f (h:t) = insertOn f h (sorte f t)

insertOn:: Ord b => (a -> b) -> a -> [a] -> [a]
insertOn _ x [] = [x]
insertOn f x (h:t) | f x > f h = h:insertOn f x t
                   | otherwise = x:h:t

-- 3. Funções de ordem superior aplicadas a matrizes
-- a) Testar se uma matriz está bem construída.

type Mat a = [[a]]

m1:: Mat Int
m1 = [ [1,2,3],
       [0,4,5],
       [0,0,6]
     ]

dimOk:: Mat a -> Bool
dimOk m = let d = length (head m)
          in and (map(\ l -> length l == d) m)

-- usar com um foldr ou foldl ^^ 
 
-- b) Calcula a dimensão de uma matriz

dimMat:: Mat a -> (Int,Int)
dimMat m = (a,b)
    where (a,b) = ((length m),(length (head m)))

-- c) Soma de matrizes, para matrizes de igual dimensão

addMat:: Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat (a:as) (b:bs) = (zipWith (+) a b):addMat as bs 
               
-- d) Calcula a transposta de uma matriz

transpose':: Mat a -> Mat a
transpose' ([]:_) = []
transpose' m1 = firstCol m1:transpose'(otherCols m1)

firstCol:: Mat a -> [a]
firstCol m = map head m

otherCols:: Mat a -> Mat a
otherCols m = map tail m

-- e) Multiplicação de matrizes

multMat:: Num a => Mat a -> Mat a -> Mat a
multMat m1 m2 = multMat' m1 (transpose' m2)

multMat':: Num a => Mat a -> Mat a -> Mat a
multMat' [] _ = []
multMat' _ [] = []
multMat' (h:t) m2 = multLinha h m2: multMat' t m2

multLinha:: Num a => [a] -> Mat a -> [a]
multLinha l (h:t) = sum(zipWith (*) l h):multLinha l t







