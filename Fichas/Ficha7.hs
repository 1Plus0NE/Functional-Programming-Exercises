module Ficha7 where

-- Outras Arvores Binarias

--1. Considere o seguinte tipo para representar express ̃oes inteiras.

data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt
            deriving Show

e:: ExpInt
e = Mais (Const 3) (Mult (Const 4) (Const 5))

-- a) Dada uma destas expressões calcula o seu valor

calcula:: ExpInt -> Int
calcula (Const x)     = x
calcula (Simetrico e) = - (calcula e)
calcula (Mais e1 e2)  = calcula e1 + calcula e2
calcula (Menos e1 e2) = calcula e1 - calcula e2
calcula (Mult e1 e2)  = calcula e1 * calcula e2

-- b) Apresenta o calculo como uma string infixa

infixa:: ExpInt -> String
infixa (Const x)     = show x
infixa (Simetrico e) = "-" ++ infixa e
infixa (Mais e1 e2)  = "(" ++ infixa e1 ++ "+" ++ infixa e2 ++ ")"
infixa (Menos e1 e2) = "(" ++ infixa e1 ++ "-" ++ infixa e2 ++ ")"
infixa (Mult e1 e2)  = "(" ++ infixa e1 ++ "*" ++ infixa e2 ++ ")"

-- c) Apresenta o calculo como uma string posfixa

posfixa:: ExpInt -> String
posfixa (Const x)     = show x ++ " "
posfixa (Simetrico e) = "-" ++ posfixa e 
posfixa (Mais e1 e2)  = posfixa e1 ++ posfixa e2 ++ " + "
posfixa (Menos e1 e2) = posfixa e1 ++ posfixa e2 ++ " - "
posfixa (Mult e1 e2)  = posfixa e1 ++ posfixa e2 ++ " * "

-- 2. Arvores irregulares (rose trees)

data RTree a = R a [RTree a]
               deriving Show

rt:: RTree Int
rt = R 2 [R 3[ R 8 [],
               R 1 []],
               
          R 7 [],
          R 4 [R 14 []]
          ]

-- a) Soma os elementos da arvore

soma:: Num a => RTree a -> a
soma (R x l) = x + sum (map soma l)

-- b) Calcula a altura da arvore

altura:: RTree a -> Int
altura (R x []) = 1
altura (R x l) = 1 + maximum(map altura l)

-- c) Remove de uma árvore todos os elementos apartir de uma determinada profundidade

prune:: Int -> RTree a -> RTree a
prune 0 (R x l) = R x []
prune n (R x l) = R x (map(prune (n - 1)) l)

-- d) Gera a árvore simétrica

mirror:: RTree a -> RTree a
mirror (R x l) = R x (map mirror (reverse l))

-- e) Faz a travessia post-order

postOrder:: RTree a -> [a]
postOrder (R x []) = [x]
postOrder (R x l) = concatMap postOrder l ++ [x]

-- 3. Outro tipo de árvore binária

data LTree a = Tip a | Fork (LTree a) (LTree a)
               deriving Show

lt:: LTree Int
lt = Fork (Fork (Tip 5)
                (Fork (Tip 6)
                      (Tip 4)))
            (Fork (Fork (Tip 3)
                        (Tip 7))
                  (Tip 5))

-- a) Somar as folhas de uma árvore

ltSum:: Num a => LTree a -> a
ltSum (Tip x) = x
ltSum (Fork e d) = ltSum e + ltSum d 

-- b) Lista as folhas de uma árvore da esquerda para a direita

listaLT:: LTree a -> [a]
listaLT (Tip x) = [x]
listaLT (Fork e d) = (listaLT e) ++ (listaLT d) 

-- c) Calcula a altura da árvore

ltHeight:: LTree a -> Int
ltHeight (Tip x) = 1
ltHeight (Fork e d) = 1 + max (ltHeight e) (ltHeight d)

-- 4. Full Trees

data FTree a b = Leaf b | No a (FTree a b) (FTree a b)
                deriving Show

ft = No 8 (No 1 (Leaf 5)
                (No 2 (Leaf 6)
                      (Leaf 4)))
          (No 9 (No 10 (Leaf 3)
                       (Leaf 7))
                (Leaf 5))

data BTree a = Empty
               | Node a (BTree a) (BTree a)
               deriving Show

t1:: BTree Int
t1 = Node 5 (Node 3 Empty (Node 2 Empty Empty))
            (Node 4 Empty Empty)

-- a) Separa uma árvore com informação nos nodos e nas folhas em duas árvores de tipos diferentes

splitFTree:: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf b) = (Empty,Tip b)
splitFTree (No x d e) = (Node x dt et,Fork dl el)
    where (dt,dl) = splitFTree d
          (et,el) = splitFTree e

-- b) Junta duas árvores numa só

joinTrees:: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees (Empty) (Tip x) = Just (Leaf x)
joinTrees (Node n et dt) (Fork el dl) = case (joinTrees et el, joinTrees dt dl) of 
                                                                                  (Just x, Just y) -> Just (No n x y)
                                                                                   _ -> Nothing
joinTrees _ _ = Nothing

