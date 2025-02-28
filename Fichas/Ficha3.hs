module Ficha4 where
import Ficha1ex4

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

-- 1.a) Testar se uma etapa esta bem construida (se o tempo de chegada e superior ao de partida e se as horas estao validas)

isEtapaValid:: Etapa -> Bool
isEtapaValid (H h1 m1,H h2 m2) = isHourValid(H h1 m1) && isHourValid(H h2 m2) && (isHourAfter(H h2 m2) (H h1 m1))

-- b) Testar se uma viagem esta bem construida, i.e se para cada etapa o tempo de chegada e superior ao tempo de partida
--    e se cada etapa seguinte começa depois da etapa anterior ter chegado

isViagemValid:: Viagem -> Bool
isViagemValid [e] = isEtapaValid e
isViagemValid ((h1,h2):(h3,h4):t) = isEtapaValid(h1,h2) && isEtapaValid(h3,h4) && (isHourAfter(h3) (h2)) && isViagemValid ((h3,h4):t)

-- c) Calcular a hora de partida e chegada de uma dada viagem

partidaCh:: Viagem -> (Hora,Hora)
partidaCh v = (fst (head v),snd (last v))

-- partidaCh v = (hi,hf)
--    where (hi,_) = head v
--          (_,hf) = last v

-- d) Calcular o tempo total de uma viagem

tempoViagem:: Viagem -> Int
tempoViagem [] = 0
tempoViagem (e:t) = (hourToMinute(snd e)-hourToMinute(fst e))+tempoViagem t

-- e) Calcular o total tempo de espera

tempoEspera:: Viagem -> Int
tempoEspera [e] = 0
tempoEspera (e1:e2:t) = hourToMinute (fst e2) - hourToMinute (snd e1) + tempoViagem(e2:t)

-- f) Tempo total de uma viagem

tempoTotal:: Viagem -> Int
tempoTotal v = tempoEspera v + tempoViagem v

-- 5. Tipo de dados de um extrato bancario

data Movimento = Credito Float
                | Debito Float
                deriving Show

data Data = D Int Int Int
            deriving Show

data Extrato = Ext Float [(Data, String, Movimento)]
                deriving Show

outubro:: Extrato
outubro = Ext 100.0 [(D 20 10 2022, "MB",       Debito   10)
                    ,(D 21 10 2022, "Salario",  Credito 500)
                    ]


-- a) Produzir uma lista de todos os movimentos superior a um determinado valor

extValor:: Extrato -> Float -> [Movimento]
extValor (Ext _ mvs) v = movimentosValor mvs v

movimentosValor:: [(Data,String,Movimento)] -> Float -> [Movimento]
movimentosValor [] _ = []
movimentosValor ((d,desc,Debito x):mov) v = if(x >= v) then Debito x:movimentosValor mov v else movimentosValor mov v
movimentosValor ((d,desc,Credito x):mov) v = if(x >= v) then Credito x:movimentosValor mov v else movimentosValor mov v


-- b) Retorna informaçao relativa apenas aos movimentos cuja descricao esteja incluida na lista fornecida

filtro:: Extrato -> [String] -> [(Data,Movimento)]
filtro (Ext _ []) _ = []
filtro (Ext c ((d,desc,mov):t)) listaStr = if(desc `elem` listaStr) then (d,mov):filtro (Ext c t) listaStr else filtro (Ext c t) listaStr










