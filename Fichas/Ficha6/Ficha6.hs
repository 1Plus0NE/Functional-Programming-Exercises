module Ficha6 where

-- Arvores Binarias

data BTree a = Empty
               | Node a (BTree a) (BTree a)
               deriving Show

t1:: BTree Int
t1 = Node 5 (Node 3 Empty (Node 2 Empty Empty))
            (Node 4 Empty Empty)

t2:: BTree Int
t2 = Node 17 (Node 15 Empty Empty)
             (Node 13 (Node 10 Empty Empty)
                      (Node 9 Empty Empty))


-- 1.a) Calcular a altura de uma arvore

altura:: BTree a -> Int
altura Empty        = 0
altura (Node _ e d) = 1 + max (altura e) (altura d)

-- b) Determina o numero de nodos de uma arvore

contaNodos:: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node _ e d) = 1 + contaNodos e + contaNodos d

-- c) Determina o numero de folhas de uma arvore, i.e, um nodo sem descendentes

folhas:: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ e d) = folhas e + folhas d

-- d) Remove de uma arvore todos os elementos de uma determinada profundidade 

prune:: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty
prune n (Node x e d) = Node x (prune (n-1) e) (prune (n-1) d)

-- e) Dado um caminho e uma árvore, dar a lista com a informaçáo dos nodos por onde o caminho passa

path:: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node x e d) = [x]
path (h:t) (Node x e d) | h == True = x:path t d
                        | otherwise = x:path t e

-- f) Devolve a árvore binária

mirror:: BTree a -> BTree a 
mirror Empty = Empty
mirror (Node x e d) = Node x (mirror d) (mirror e)

-- g) Generaliza a função zipWith para árvores binárias

zipWithBT:: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node x e d) (Node x' e' d') = Node (f x x') (zipWithBT f e e') (zipWithBT f d d')
zipWithBT _ _ _ = Empty

-- h) Generaliza a função unZip para árvores binárias

unzipBT:: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (a,b,c) e d) = (Node a unzipE1 unzipD1, Node b unzipE2 unzipD2, Node c unzipE3 unzipD3)
      where (unzipE1,unzipE2,unzipE3) = unzipBT e
            (unzipD1,unzipD2,unzipD3) = unzipBT d

-- 2. Arvores binárias de procura

-- a) Determina o menor elemento de uma arvore binaria de procura nao vazia

minimo:: Ord a => BTree a -> a
minimo (Node x Empty _) = x
minimo (Node x e d) = minimo e

-- b) Remove o menor elemento de uma arvore binaria de procura nao vazia

semMinimo:: Ord a => BTree a -> BTree a
semMinimo (Node _ Empty d) = d
semMinimo (Node x e d) = Node x (semMinimo e) d

-- c) Calcula com uma única travessia da árvore o minimo e a árvore sem mínimo

minSmin:: Ord a => BTree a -> (a,BTree a)
minSmin (Node x Empty d) = (x,d)
minSmin (Node x e d) = (a,Node x b d)
      where (a,b) = minSmin e

-- d) Remove um elemento de uma arvore binaria de procura, usando a função anterior

remove:: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node y e d) | x < y = Node y (remove x e) d 
                      | x > y = Node y e (remove x d)
                      | otherwise = case d of 
                                                Empty -> e
                                                _     -> let (g,h) = minSmin d in Node g e h

-- 3. 

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
    deriving Show
type Turma = BTree Aluno -- arvore binaria de procura (ordenada por numero)

a1:: Turma
a1 = Node (10,"Ana",ORD,Aprov 16)
          (Node (2,"Rui",ORD,Aprov 15) 
                (Node (1,"Ze",TE,Faltou) Empty Empty)
                (Node (7,"To",ORD,Rep) Empty Empty))
          (Node (20,"Joao",ORD,Aprov 10)
                (Node (15,"Maria",ORD,Aprov 11) Empty Empty)
                Empty)

-- a) Verifica se um aluno com um dado número esta inscrito

{-
inscNum:: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum n (Node (num,_,_,_) e d) = (n == num) || inscNum n (if n < num then e else d)
-}

-- ou

inscNum:: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum n (Node (num,_,_,_) e d) | n == num = True
                                 | n < num = inscNum n e 
                                 | n > num = inscNum n d

-- b) Verifica se um aluno com um dado nome esta inscrito

inscNome:: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome s (Node (_,nome,_,_) e d) | s == nome = True
                                   | otherwise = inscNome s e || inscNome s d 

-- c) Lista o numero e o nome dos alunos Trabalhadores-Estudantes

trabEst:: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,nome,TE,_) e d) = trabEst e ++ [(num,nome)] ++ trabEst d
trabEst (Node _ e d) = trabEst e ++ trabEst d


-- d) Calcula a classificação de um aluno 

nota:: Numero -> Turma -> Maybe Classificacao
nota _ Empty = Nothing
nota n (Node (na,_,_,nt) e d) | n == na = Just nt
                              | n < na = nota n e
                              | n > na = nota n d

-- e) Calcula a percentagem de alunos que faltaram à avaliação

percFaltas:: Turma -> Float
percFaltas Empty = 0
percFaltas turma = sumFaltas turma / fromIntegral(contaNodos turma) * 100

sumFaltas:: Turma -> Float
sumFaltas Empty = 0
sumFaltas (Node (_,_,_,Faltou) e d) = 1 + sumFaltas e + sumFaltas d 
sumFaltas (Node _ e d) = sumFaltas e + sumFaltas d

-- f) Calcula a media das notas dos alunos que passaram

mediaAprov:: Turma -> Float
mediaAprov Empty = 0
mediaAprov turma = (sumMedia turma) / (sumAprov turma)

sumAprov:: Turma -> Float
sumAprov Empty = 0
sumAprov (Node (_,_,_,Aprov _) e d) = 1 + sumAprov e + sumAprov d 
sumAprov (Node _ e d) = sumAprov e + sumAprov d

sumMedia:: Turma -> Float
sumMedia Empty = 0
sumMedia (Node (_,_,_,Aprov n) e d) = (fromIntegral n + sumMedia e + sumMedia d)
sumMedia (Node _ e d) = sumMedia e + sumMedia d

-- g) Calcula o ratio de alunos aprovados por avaliados

aprovAv:: Turma -> Float
aprovAv Empty = 0
aprovAv turma = (sumAprov turma / auxAval turma) * 100

auxAval:: Turma -> Float
auxAval Empty = 0
auxAval (Node (_,_,_,cla) e d) = case cla of
                                                Aprov nota -> 1 + auxAval e + auxAval d
                                                Rep        -> 1 + auxAval e + auxAval d
                                                Faltou     -> auxAval e + auxAval d





