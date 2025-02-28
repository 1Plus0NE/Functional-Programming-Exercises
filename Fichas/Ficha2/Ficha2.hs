module Ficha2 where

import Data.Char

-- Ficha 2
-- 2.a) Recebe uma lista e faz o dobro de cada elemento

dobros:: [Float] -> [Float]
dobros [] = []
dobros (x:xs) = x*2:dobros xs

-- b) Calcula o numero de vezes que um char ocorre numa string

numOcorre:: Char -> String -> Int
numOcorre x [] = 0
numOcorre x (c:str) = if(x == c) then 1 + numOcorre x str else numOcorre x str

-- c) Testar se uma lista so tem elementos positivos

positivos:: [Int] -> Bool
positivos [] = True
positivos (x:xs) = if(x<0) then False else positivos xs

-- d) Retira todos o elementos negativos de uma lista

soPos:: [Int] -> [Int]
soPos [] = []
soPos (x:xs) = if(x<0) then soPos xs else x:soPos xs

-- e) Soma todos os numeros negativos de uma lista

somaNeg:: [Int] -> Int
somaNeg [] = 0
somaNeg (x:xs) = if(x<0) then x + somaNeg xs else somaNeg xs

-- f) Devolve os 3 ultimos elementos da lista, se a lista tiver menos de 3 elementos, devolve a lista

tresUlt:: [a] -> [a]
tresUlt [] = []
tresUlt (x:xs) = if(length xs < 3) then x:xs else tresUlt xs

-- g) Calcula a lista das segundas componentes dos pares

segundos:: [(a,b)] -> [b]
segundos [] = []
segundos ((a,b):l) = b:segundos l

-- h) Testa se um elemento aparece na lista como primeira componenente de alguns dos pares

nosPrimeiros:: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros a [] = False
nosPrimeiros a ((x,y):l) = if(a == x) then True else nosPrimeiros a l

-- i) Soma uma lista de triplos componente a componente

sumTriplos:: (Num a,Num b,Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [(a,b,c)] = (a,b,c)
sumTriplos ((a,b,c):l) = (a+sumA, b+sumB, c+sumC)
            where (sumA,sumB,sumC) = sumTriplos l

-- questao group das 50
group:: Eq a => [a] -> [[a]]
group [] = [[]]
group [x] = [[x]]
group (x:xs) | elem x (head r) = (x:(head r)):tail r
             | otherwise = [x]:r
            where r = group xs
        
-- 2.a) Recebe uma lista de caracteres e seleciona apenas os que sao algarismos

soDigitos:: [Char] -> [Char]
soDigitos [] = []
soDigitos (x:xs) = if(isDigit x == True) then x:soDigitos xs else soDigitos xs

-- b) Recebe uma lista de caracteres e conta quantos sao minusculas

minusculas:: [Char] -> Int
minusculas [] = 0
minusculas (x:xs) = if(isLower x == True) then 1+minusculas xs else minusculas xs

-- c) Recebe uma string e devolve uma lista com os algarismos que ocorrem nessa string, pela mesma ordem

nums:: String -> [Int]
nums [] = []
nums (x:xs) = if(isDigit x == True) then digitToInt x:nums xs else nums xs

type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- 3.a) Conta quantos monomios de grau n ha em um dado polinomio

conta:: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((x,y):t) = if(n == y) then 1+conta n t else conta n t

-- b) Indicar o grau de um polinomio

grau:: Polinomio -> Int
grau [(x,y)] = y
grau ((x,y):t) = if(y>grau t) then y else grau t

-- c) Seleciona os monomios com um dado grau de um polinomio

selGrau:: Int -> Polinomio -> Polinomio
selGrau _ [] = []
selGrau n ((x,y):t) = if(n == y) then (x,y):selGrau n t else selGrau n t

-- d) Calcula a derivada de um polinomio

deriv:: Polinomio -> Polinomio
deriv [] = []
deriv ((x,y):t) = if(y>0) then (x* fromIntegral y,y-1):deriv t else deriv t

-- e) Calcula o valor de um polinomio para um dado valor de x

calcula:: Float -> Polinomio -> Float
calcula n [] = 0
calcula n ((x,y):t) = (x*n^y) + calcula n t

-- f) Retira de um polinomio os monomios de coeficiente 0

simp:: Polinomio -> Polinomio
simp [] = []
simp ((x,y):t) = if(x==0) then simp t else (x,y):simp t

-- g) Calcula o resultado da multiplicacao de um monomio por um polinomio

mult:: Monomio -> Polinomio -> Polinomio
mult (_,_) [] = []
mult (a,b) ((x,y):t) = (a*x,b+y):mult (a,b) t

-- h) Construir um polinomio equivalente ??

normaliza:: Polinomio -> Polinomio
normaliza [] = []
normaliza [(x,y)] = [(x,y)]
normaliza ((x,y):(x2,y2):t) | y == y2 = normaliza ((x+x2,y):t)
                            | conta y t == 0 = (x,y):normaliza((x2,y2):t)
                            | otherwise = normaliza ((x,y):t ++ [(x2,y2)])

-- i) Soma de polinomios

soma:: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1++p2)











