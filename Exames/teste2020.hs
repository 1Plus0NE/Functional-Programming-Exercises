{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Teste where

{- 1. Apresente uma defini¸c˜ao recursiva da fun¸c˜ao (\\) :: Eq a => [a] -> [a] -> [a] que retorna a lista
resultante de remover (as primeiras ocorrˆencias) dos elementos da segunda lista da primeira. Por exemplo,
(\\) [1,2,3,4,5,1,2] [2,3,4,1,2] == [5,1].
 -}

delXs:: Eq a => [a] -> [a] -> [a]
delXs l1 [] = l1
delXs [] _ = []
delXs (h:t) l = delXs(del h l) t

del:: Eq a => a -> [a] -> [a]
del _ [] = []
del n (h:t) | n == h = t
            | otherwise = h:del n t

{- 2. Considere o tipo MSet a para representar multi-conjuntos de elementos de a: type MSet a = [(a,Int)]
Considere ainda que nestas listas n˜ao h´a pares cuja primeira componente coincida, nem cuja segunda
componente seja menor ou igual a zero.
 -}

type MSet a = [(a,Int)]

{- (a) Defina a fun¸c˜ao removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] que remove um elemento a um multi-conjunto.
Se o elemento n˜ao existir, deve ser retornado o multi-conjunto recebido.
Por exemplo, removeMSet ’c’ [(’b’,2), (’a’,4), (’c’,1)] == [(’b’,2), (’a’,4)].
 -}

removeMSet:: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet n ((x,y):t) | n == x = if y > 1 then (x,(y-1)):t else t
                       | otherwise = (x,y):removeMSet n t

{- (b) Usando uma fun¸c˜ao de ordem superior, defina a fun¸c˜ao calcula :: MSet a -> ([a],Int) que,
numa ´unica travessia do multi-conjunto, calcula simulanemente a lista (sem repetidos) de elementos
do multi-conjunto e o n´umero total de elementos. Por exemplo, calcula [(’b’,2), (’a’,4),
(’c’,1)] == ([’b’,’a’,’c’],7).
 -}

calcula :: MSet a -> ([a],Int)
calcula = foldr(\(x,y) (elems,total) -> (x:elems,y+total)) ([],0)

{- 3. Defina a fun¸c˜ao partes :: String -> Char -> [String], que parte uma string pelos pontos onde um
dado caracter ocorre. Por exemplo, partes "um;bom;exemplo;" ’;’ == ["um","bom","exemplo"] e
partes "um;exemplo;qualquer" ’;’ == ["um","exemplo","qualquer"].
 -}

partes :: String -> Char -> [String]
partes [] _ = []
partes [x] _ = [[x]]
partes (h:t) c | h == c = "":a
               | otherwise = (h:head a):tail a
    where a = partes t c

{- 4. Considere a seguinte arvore -}

data BTree a = Empty | Node a (BTree a) (BTree a) 
a1 = Node 5 (Node 3 Empty Empty)
            (Node 7 Empty (Node 9 Empty Empty))
    

-- (a) Defina a fun¸c˜ao remove :: Ord a => a -> BTree a -> BTree a, que remove um elemento de uma ´arvore bin´aria de procura.

remove:: Ord a => a -> BTree a -> BTree a 
remove _ Empty = Empty
remove y (Node x e d) | y > x = Node x e (remove y d)
                      | y < x = Node x (remove y e) d 
                      | otherwise = case d of
                                    Empty -> e
                                    _     -> let (g,h) = minSmin d in Node g e h


minSmin:: Ord a => BTree a -> (a,BTree a)
minSmin (Node x Empty d) = (x,d)
minSmin (Node x e d) = (a,Node x b d)
      where (a,b) = minSmin e

-- (b) Defina BTree a como uma instˆancia da classe Show de forma a que show a1 produza a string "((* <-3-> *) <-5-> (* <-7-> (* <-9-> *)))"

instance Show a => Show (BTree a) where

    show Empty = "*"
    show (Node x e d) = "(" ++ show e ++ " <-" ++show x ++ "-> " ++ show d ++ ")"

{- 5. Apresente uma defini¸c˜ao da fun¸c˜ao sortOn :: Ord b => (a -> b) -> [a] -> [a] que ordena uma
lista comparando os resultados de aplicar uma fun¸c˜ao de extrac¸c˜ao de uma chave a cada elemento de uma
lista. Por exemplo: sortOn snd [(3,1),(2,5),(1,2)] == [(3,1),(1,2),(2,5)]. -}

sorte:: Ord b => (a -> b) -> [a] -> [a]
sorte f [] = []
sorte f (h:t) = insertOn f h (sorte f t)

insertOn:: Ord b => (a -> b) -> a -> [a] -> [a]
insertOn _ x [] = [x]
insertOn f x (h:t) | f x > f h = h:insertOn f x t
                   | otherwise = x:h:t

-- 6.

data FileSystem = File Nome | Dir Nome [FileSystem]
type Nome = String
fs1 = Dir "usr" [Dir "xxx" [File "abc.txt", File "readme", Dir "PF" [File "exemplo.hs"]],
      Dir "yyy" [], Dir "zzz" [Dir "tmp" [], File "teste.c"] ]

-- (a) Defina a fun¸c˜ao fichs :: FileSystem -> [Nome], que lista o nome de todos os ficheiros de um file system.

fichs:: FileSystem -> [Nome]
fichs (File n) = [n]
fichs (Dir d ds) = concatMap fichs ds











