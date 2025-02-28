module Ficha8 where

import Data.List 
import Data.Char

-- 1. Considere o seguinte tipo de dados para apresentar frações

data Frac = F Integer Integer

-- a) Definir a função normaliza

normaliza:: Frac -> Frac
normaliza (F a b) | b < 0 = normaliza (F (-a) (-b))
                  | otherwise = let d = mdc a b in F (a `div` d) (b `div` d)

mdc:: Integer -> Integer -> Integer
mdc x 0 = x 
mdc 0 y = y
mdc x y = mdc y (x `mod` y)

-- b) Defina Frac como instância da classe Eq

instance Eq Frac where

    f1 == f2 = a1 == a2 && b1 == b2
        where F a1 b1 = normaliza f1
              F a2 b2 = normaliza f2

-- c) Define Fraccomo instância da classe Ord

instance Ord Frac where

    f1 <= f2 = a1 * b2 <= a2 * b1
        where F a1 b1 = normaliza f1
              F a2 b2 = normaliza f2

-- d) Defina Frac como instância da classe Show

instance Show Frac where

    show (F a b) = show a ++ "/" ++ show b

-- e) Define Frac como instância da classe Num

instance Num Frac where

    (F a b) + (F c d) = normaliza $ F (a*d + b*c) (b*d)
    (F a b) - (F c d) = normaliza $ F (a*d - b*c) (b*d)
    (F a b) * (F c d) = normaliza $ F (a*c) (b*d)

    negate (F a b) = normaliza $ F (-a) b

    abs f = F (abs a) b
        where F a b = normaliza f

    signum f = F (signum a) 1
        where F a b = normaliza f

    fromInteger x = F x 1

-- f) Dada uma fração F e uma lista de frações L, seleciona de L os elementos que são maiores do que o dobro de F 

biggestF:: Frac -> [Frac] -> [Frac]
biggestF = filter . (<) . (2*) 

-- 2. Considere o seguinte tipo para representar express ̃oes inteiras.

data Exp a = Const a
            | Simetrico (Exp a)
            | Mais (Exp a) (Exp a)
            | Menos (Exp a) (Exp a)
            | Mult (Exp a) (Exp a)

e1 = Mais (Const 3) (Mult (Const 4) (Const 5))
e2 = Mais (Const 3) (Mult (Const 4) (Const 5))
e3 = Menos (Const 1) (Mult (Const 2) (Const 5))

-- a) Declare Exp como instância de Show

instance Show a => Show (Exp a) where

    show (Const x) = show x ++ ""
    show (Simetrico e) = "-"++show e
    show (Mais e1 e2) = show e1 ++ " + " ++ show e2
    show (Menos e1 e2) = show e1 ++ " - " ++ show e2
    show (Mult e1 e2) = show e1 ++ " * " ++ show e2

-- b) Declare Exp como instância de Eq

calcula:: Num a => Exp a -> a
calcula (Const x)     = x
calcula (Simetrico e) = - (calcula e)
calcula (Mais e1 e2)  = calcula e1 + calcula e2
calcula (Menos e1 e2) = calcula e1 - calcula e2
calcula (Mult e1 e2)  = calcula e1 * calcula e2

instance (Num a, Eq a) => Eq (Exp a) where

    e1 == e2 = calcula e1 == calcula e2

-- c) Declare Exp como instância de Num

instance (Ord a, Num a) => Num (Exp a) where

    e1 + e2 = Const $ calcula $ Mais e1 e2
    e1 - e2 = Const $ calcula $ Menos e1 e2
    e1 * e2 = Const $ calcula $ Mult e1 e2

    negate (Simetrico e) = e 
    negate e = Simetrico e

    abs e = Const $ abs $ calcula e
    signum e = Const $ signum $ calcula e

    fromInteger e = Const (fromInteger e)

-- 3. Contas bancárias

data Movimento = Credito Float | Debito Float
data Data = D Int Int Int 
    deriving Eq
data Extrato = Ext Float [(Data, String, Movimento)]

ext = (Ext 200 [(D 10 11 2022, "Compra", Credito 100)])


-- a) Defina Data como instãncia de Ord

instance Ord Data where

--                13 11 2022 14 11 2022 -> Menor
--                14 11 2022 14 10 2022 -> Menor
--                14 11 2022 14 11 2021 -> Menor

    D d1 m1 a1 <= D d2 m2 a2 = a1 <= a2 && ((m1 <= m2 && d1 <= d2) || (m1 < m2 && d1 >= d2))

-- b) Defina Data como instância de Show

instance Show Data where

    show (D d1 m1 a1) = show d1 ++ "/" ++ show m1 ++ "/" ++ show a1
                             
-- c) Define a função ordena que transforma um extrato de modo que a lista de movimentos apareça ordenada por ordem crescente de data

ordena:: Extrato -> Extrato
ordena (Ext v l)= Ext v (sortBy (\(data1,_,_) (data2,_,_) -> compare data1 data2) l)

-- d) Defina Extrato como instância de Show

saldo :: Extrato -> Float
saldo (Ext si []) = si
saldo (Ext si ((_,_,Debito x):t)) = saldo (Ext (si + x) t)
saldo (Ext si ((_,_,Credito x):t)) = saldo (Ext (si - x) t)

instance Show Extrato where

    show ext = "Saldo anterior: " ++ show n ++
               "\n---------------------------------------" ++
               "\nData       Descricao" ++ replicate (desc_max - 9) ' ' ++ "Credito" ++ replicate (cred_max - 7) ' ' ++ "Debito" ++
               "\n---------------------------------------\n" ++
               unlines (map (\(dat,desc,mov) -> 
                    show dat ++ replicate (data_max - length (show dat)) ' ' 
                    ++ map toUpper desc ++ replicate (desc_max - length desc) ' ' 
                    ++ case mov of Credito quant -> show quant ++ replicate (cred_max - length (show quant)) ' '; Debito _ -> replicate cred_max ' '
                    ++ case mov of Debito quant -> show quant; Credito _ -> ""
               ) movs) ++
               "---------------------------------------" ++
               "\nSaldo atual: " ++ show (saldo ext)
        where (Ext n movs) = ordena ext
              data_max = 11
              desc_max = max (length "Descricao   ") (maximum $ map (\(_,desc,_) -> length desc) movs)
              cred_max = max (length "Credito   ") (maximum $ map (\(_,_,mov) -> case mov of Credito x -> length (show x); _ -> 0) movs)

