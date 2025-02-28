module Ficha1 where

import Data.Char

-- Ficha 1 - Programacao Funcional 
-- 1.a) Perimetro de uma circuferencia

perimetro r = 2*3.14*r

-- b) Distancia entre 2 pontos 

dist (x1,y1) (x2,y2) = sqrt((x1-x2)^2 + (y1-y2))

-- c) primeiro e ultimo de uma lista

primUlt l = (head l, last l)

-- d) multiplo 

multiplo:: Int-> Int-> Bool
multiplo x y = mod x y == 0

-- e) recebe lista se impar retira primeiro elemento, se nao devolve

truncaImpar:: [a]->[a]
truncaImpar l = if(mod(length l)2==1) then tail l else l

-- f) maximo de 2 numeros

max2:: Int-> Int-> Int
max2 x y = if(x>y) then x else y 

-- g) maximo de 3 numeros usando a alinea f

max3:: Int-> Int-> Int-> Int
max3 x y z = max2 x (max2 y z)

-- 2.a) Calcular o numero de raizes de um polinomio
nRaizes:: (Double,Double,Double) -> Int
nRaizes (a,b,c)
    | delta > 0 = 2
    | delta == 0 = 1
    | delta < 0 = 0
    where delta = b^2 - 4*a*c

-- b) Calcular a lista de raizes

raizes:: (Double,Double,Double) -> [Double]
raizes (a,b,c)
    | n == 2 = [(-b+sqrt(delta)/2*a),(-b-sqrt(delta)/2*a)]
    | n == 1 = [(-b+sqrt(delta)/2*a)]
    | n == 0 = []
    where n = nRaizes(a,b,c)
          delta = b^2 - 4*a*c
    
-- 3.a) Verificar se par de inteiros e uma hora valida

type Hora = (Int,Int) 

isHourValid:: Hora -> Bool
{-isHourValid (h,m) = 0<=h && h<=23 && 
                      0<=m && m<=59
-}
-- ou

isHourValid (h,m) = h `elem`[0..23] && m `elem`[0..59]

-- b) Verificar se uma hora e depois da outra

isHourAfter:: Hora -> Hora -> Bool
isHourAfter (h1,m1) (h2,m2) = h1 > h2 || (h1 == h2 && m1 > m2)

-- c) horas para minutos

hourToMinute:: Hora -> Int
hourToMinute (h,m) = 60*h+m

-- d) minutos para horas

minuteToHour:: Int -> Hora
minuteToHour m = divMod m 60

-- e) diferenca de horas

subHours:: Hora -> Hora -> Int
subHours (h1,m1) (h2,m2) = hourToMinute (h1,m1) - hourToMinute (h2,m2)

-- f) adicionar um numero de min a uma dada hora

addMin:: Hora -> Int -> Hora
addMin (h,m) n = (h + hr,mr)
    where (hr,mr) = minuteToHour(m+n)

-- 5 estados de um semaforo

data Semaforo = Verde | Amarelo | Vermelho deriving (Show, Eq)

-- a) Proximo estado de um semaforo

next:: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

-- b) determinar se deve parar num semaforo

stop:: Semaforo -> Bool
stop Vermelho = True
stop _ = False

-- ou

{-
stop s = if(s==Vermelho) then True else false
-}

-- c) testa se o estado de dois semaforos num cruzamento e seguro

safe:: Semaforo -> Semaforo -> Bool
safe Vermelho Vermelho = True
safe _ Vermelho = True
safe Vermelho _ = True
safe _ _ = False

-- 6. Ponto Cartesiano

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

-- a) Calcular a distancia de um ponto ao eixo vertical

posx:: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar r a) = if(a == pi/2) then 0 else r*cos a

-- b) Calcular a distancia de um ponto ao eixo horizontal

posy:: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar r a) = r*sin a

-- c) Calcula a distancia de um ponto a origem

raio:: Ponto -> Double
raio (Cartesiano x y) = sqrt(x^2+y^2)
raio (Polar r a) = r

-- d) Calcula o angulo

angulo:: Ponto -> Double
angulo (Cartesiano x y) = atan y/x
angulo (Polar r a) = a

--e) calcular a distancia entre dois pontos

distP:: Ponto -> Ponto -> Double
distP p1 p2 = sqrt((x'-x)^2 +(y'-y)^2)
    where x = posx p1
          y = posy p1
          x'= posx p2
          y'= posy p2


--distP (Cartesiano x y) (Cartesiano x' y') = sqrt((x'-x)^2 +(y'-y)^2)

-- 7. Figuras Geometricas num plano

data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
              deriving (Show,Eq)

-- a) testar se uma figura e um poligono

poligono:: Figura -> Bool
poligono (Circulo _ _) = False
poligono (Retangulo p1 p2) = True 
poligono (Triangulo p1 p2 p3) = True

-- b) calcula a lista dos vertices de uma figura

vertices:: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices (Retangulo p1 p2) = [p1, Cartesiano (posx p1) (posy p2), p2, Cartesiano (posx p2) (posy p1)]
vertices (Triangulo p1 p2 p3) = [p1,p2,p3]

-- c) Calcular a area de uma figura

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
        let a = distP p1 p2
            b = distP p2 p3
            c = distP p3 p1
            s = (a+b+c) / 2 -- semi-perimetro
        in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area (Retangulo p1 p2) = (posx p2 - posx p1) * (posy p2 - posy p1)
area (Circulo _ r) = pi*(r^2)

-- d) Calcular o perimetro de uma figura

perimetroF:: Figura -> Double
perimetroF (Circulo _ r) = 2*pi*r
perimetroF (Retangulo p1 p2) = (posx p2 - posx p1)*2 + (posy p2 - posy p1)*2
perimetroF (Triangulo p1 p2 p3) = distP p1 p2 + distP p2 p3 + distP p1 p3

-- 8. Utilizando as funcoes ord :: Char -> Int e chr :: Int -> Char

-- a) Testar se um Char e uma minuscula

isCharLower:: Char -> Bool
isCharLower c = ord c >= ord 'a' && ord c <= ord 'z'

-- b) Testar se um Char e um digito

isCharDigit:: Char -> Bool
isCharDigit c = ord c >= ord '0' && ord c <= ord '9'

-- c) Testar se um Char e uma letra

isCharAlpha:: Char -> Bool
isCharAlpha c | (ord c >= ord 'A' && ord c <= ord 'Z') = True
              | (ord c >= ord 'a' && ord c <= ord 'z') = True
              | otherwise = False

-- d) Converter uma letra para maiuscula

charToUpper:: Char -> Char
charToUpper c = if(isCharLower c == True) then chr (ord c - 32) else c

-- e) Converter um numero de 0a9 para digito

intDigit:: Int -> Char
intDigit x = chr (x+48)

-- f) Converter digito para inteiro

digitInt:: Char -> Int
digitInt x = ord x - 48