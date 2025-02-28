module Ficha1ex4 where

-- 4 novo tipo de dados
-- a) verificar data se e valida

data Hora = H Int Int deriving (Show,Eq)

isHourValid:: Hora -> Bool
isHourValid (H h m) = 0<=h && h<=23 && 
                      0<=m && m<=59

-- b) Verificar se uma hora e depois da outra

isHourAfter:: Hora -> Hora -> Bool
isHourAfter (H h1 m1) (H h2 m2) = h1 > h2 || (h1 == h2 && m1 > m2)

-- c) horas para minutos

hourToMinute:: Hora -> Int
hourToMinute (H h m) = 60*h+m

-- d) minutos para horas

minuteToHour:: Int -> Hora
minuteToHour m = H a b 
    where (a,b) = divMod m 60

-- e) diferenca de horas

subHours:: Hora -> Hora -> Int
subHours (H h1 m1) (H h2 m2) = hourToMinute (H h1 m1) - hourToMinute (H h2 m2)

-- f) adicionar um numero de min a uma dada hora

addMin:: Hora -> Int -> Hora
addMin (H h m) n = H (h + mb) mc --sendo mb os min da hora e mc os min a adicionar
    where(H mb mc) = minuteToHour(m+n)


