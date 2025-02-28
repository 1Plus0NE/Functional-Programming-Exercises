module Ficha4 where
import Data.Char

-- 1.

digitAlpha:: String -> (String,String)
digitAlpha "" = ("","")
digitAlpha (h:t) | isDigit h = (h:ar,br)
                 | isAlpha h = (ar,h:br)
                 | otherwise = (ar,br)
    where (ar,br) = digitAlpha t

-- 2.

nzp:: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) | h < 0 = (a+1,b,c)
          | h == 0 = (a,b+1,c)
          | otherwise = (a,b,c+1)
    where (a,b,c) = nzp t

-- 3.

divMod':: Integral a => a -> a -> (a,a)
divMod' n x | n - x < 0 = (0,n)
            | otherwise = (q+1,r)
    where (q,r) = divMod' (n-x) x

-- 4.

fromDigits:: [Int] -> Int
fromDigits [] = 0
fromDigits l = auxFromDigits l 0

auxFromDigits:: [Int] -> Int -> Int
auxFromDigits [] x = x
auxFromDigits (h:t) x = auxFromDigits t (h+10*x)

-- 5.



-- 6.

fib:: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = auxFib (n-2) 0 1

auxFib:: Int -> Int -> Int -> Int
auxFib 0 n _ = n
auxFib a b c = auxFib (a-1) c (b+c)

-- 8. Exprima por enumeração a lista correspondente.
--    Tente ainda, para cada caso, descobrir uma outra forma de obter o mesmo resultado.

-- a)

f1 = [x | x <- [1..20], mod x 2 == 0, mod x 3 == 0]
-- R: [6,12,18]
-- Alternativa: f1 = [x | x <- [1..20], mod x 6 == 0]

-- b)

f2 = [x | x <- [y |y <- [1..20], mod y 2 == 0], mod x 3 == 0]
-- R:[6,12,18]
-- Alternativa: f2 = [x | x <- [1..20], mod x 6 == 0]

-- c)

f3 = [(x,y) | x <- [0..20], y <- [0..20], x+y == 30]
-- R: [(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]
-- Alternativa: f3 = [(x,30-x) | x <- [1..20]]

-- d)

f4 = [sum [y | y <- [1..x], odd y] | x <- [1..10]]
-- R: [1,1,4,4,9,9,16,16,25,25]
-- Alternativa: f4 = [x^2 | x <- [1..5], y <- [1..2]]

-- 9. Listas por compreensão.
-- a)

powerOf2To10 = [2^x | x <- [1..10]]

-- b)

parFun = [(x,y) | x <- [1..5], y <- [1..5], x+y == 6]

-- c)

listFun = [[1..x] | x <- [1..5]]

-- d)

listRepeat = [replicate x 1 | x <- [1..5]]

-- e)

newList = [product [y | y <- [1..x]] | x <- [1..6]] 

