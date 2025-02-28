{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use second" #-}
module Teste2021 where

import System.Random

{-1. Apresente uma definicao recursiva da funcao (pr´e-definida) zip :: [a] -> [b] -> [(a,b)]
constroi uma lista de pares a partir de duas listas. Por exemplo, zip [1,2,3] [10,20,30,40]
corresponde a [(1,10),(2,20),(3,30)]. -}

zipList:: [a] -> [b] -> [(a,b)]
zipList [] _ = []
zipList _ [] = []
zipList (x:xs) (y:ys) = (x,y):zipList xs ys

{- 2. Defina a fun¸c˜ao preCrescente :: Ord a => [a] -> [a] que calcula o maior prefixo crescente de uma lista. 
Por exemplo, preCrescente [3,7,9,6,10,22] corresponde a [3,7,9] e preCrescente [1,2,7,9,9,1,8] corresponde a [1,2,7,9]. -}

preCrescente:: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente (x:y:t) | x<=y = x:preCrescente (y:t)
                     | otherwise = [x]


{- 3. A amplitude de uma lista de inteiros define-se como a diferen¸ca entre o maior e o menor dos
elementos da lista (a amplitude de uma lista vazia ´e 0).
Defina a funcao amplitude :: [Int] -> Int que calcula a amplitude de uma lista (idealmente numa ´unica passagem pela lista). -}

amplitude:: [Int] -> Int
amplitude [] = 0
amplitude list = maximum list - minimum list

{- 4. Considere o sequinte tipo type Mat a = [[a]] para representar matrizes.
Defina a fun¸c˜ao soma:: Num a => Mat a -> Mat a -> Mat a que soma duas matrizes da mesma dimens˜ao -}

type Mat a = [[a]]

soma:: Num a => Mat a -> Mat a -> Mat a
soma [] _ = []
soma _ [] = []
soma (a:as) (b:bs) = (zipWith (+) a b):soma as bs

{- 5. Decidiu-se organizar uma agenda telefonica numa arvore binaria de procura (ordenada por ordem alfabetica de nomes).
Para isso, declararam-se os seguintes tipos de dados:
type Nome = String
type Telefone = Integer
data Agenda = Vazia | Nodo (Nome,[Telefone]) Agenda Agenda
Defina Agenda como instancia da classe Show de forma a que a visualizacao da arvore resulte
numa listagem da informacao ordenada por ordem alfabetica (com um registo por linha) e em
que os varios telefones associados a um nome se apresentem separados por / .-}

type Nome = String
type Telefone = Integer
data Agenda = Vazia | Nodo (Nome,[Telefone]) Agenda Agenda

t1:: Agenda
t1 = Nodo ("Joao",[911,922,933]) (Nodo ("Mariana",[133,122,144]) Vazia (Nodo ("Ana",[322,155]) Vazia Vazia))
            (Nodo ("Rui",[966]) Vazia Vazia)

instance Show Agenda where

    show Vazia = ""
    show (Nodo (nome,tlfs) e d) = show e ++ "Nome: "++ show nome ++ "\n" ++ "Lista Telefonica: " ++ "\n" ++ showaux tlfs ++ "\n\n" ++ show d

        where showaux:: [Telefone] -> String
              showaux [] = ""
              showaux [x] = show x
              showaux (h:t) = show h++"/"++showaux t

{- 6. Defina uma fun¸c˜ao randomSel :: Int -> [a] -> IO [a] que dado um inteiro n e uma lista
l, produz uma lista com n elementos seleccionados aleatoriamente de l. Um elemento n˜ao pode
aparecer na lista produzida mais vezes do que a parece na lista argumento. Se n for maior do
que o comprimento da lista a fun¸c˜ao dever´a retornar uma permuta¸c˜ao da lista argumento. Por
exemplo, a invoca¸c˜ao de randomSel 3 [1,3,1,4,2,8,9,5]] poderia produzir qualquer uma
das listas [1,4,2], [5,2,8] ou [1,9,1], mas nunca [2,3,2]. -}

randomSel:: Int -> [a] -> IO [a]
randomSel 0 _ = return []
randomSel n l = do
    randomIndex <- randomRIO(0,length l - 1)
    let randomElem = l !! randomIndex
    recur <- randomSel (n-1) (take randomIndex l ++ drop (randomIndex + 1) l)
    return (randomElem:recur)

{- 7. Defina uma fun¸c˜ao organiza :: Eq a => [a] -> [(a,[Int])] que, dada uma lista constr´oi
uma lista em que, para cada elemento da lista original se guarda a lista dos ´ındices onde esse elemento ocorre.
Por exemplo, organiza "abracadabra" corresponde a [(’a’,[0,3,5,7,10]), (’b’,[1,8]), (’r’,[2,9]),(c,[4])]. -}

organiza:: Eq a => [a] -> [(a,[Int])]
organiza [] = []
organiza l = organizaAux (repetidos l) l

organizaAux:: Eq a => [a] -> [a] -> [(a,[Int])]
organizaAux [] _ = []
organizaAux (h:t) l = (h, indices h l 0):organizaAux t l

indices:: Eq a => a -> [a] -> Int -> [Int]
indices _ [] _ = []
indices x (h:t) n | x == h = n:indices x t (n+1)
                  | otherwise = indices x t (n+1)

repetidos:: Eq a => [a] -> [a]
repetidos [] = []
repetidos (x:xs) | x `elem` xs = repetidos xs
                 | otherwise = x:repetidos xs


{- 8. Apresente uma defini¸c˜ao alternativa da fun¸c˜ao func, usando recursividade expl´ıcita em vez de
funcoes de ordem superior e fazendo uma ´unica travessia da lista.

    func :: [[Int]] -> [Int]
    func l = concat (filter (\x -> sum x >10) l)

 -}

func:: [[Int]] -> [Int]
func [] = []
func (h:t) | sum h < 10 = h++func t
           | otherwise = func t

{- 9. -}

data RTree a = R a [RTree a] deriving Show
type Dictionary = [ RTree (Char, Maybe String) ]
    

d1 = [R ('c',Nothing) [R ('a',Nothing) [R ('r',Nothing) [R ('a',Just "...") [
    R ('s',Just "...") [] ],
    R ('o',Just "...") [],
    R ('r',Nothing) [
    R ('o',Just "...") [] ]
    ]] ] ]

insere :: String -> String -> Dictionary -> Dictionary 
insere [x] desc dict = insereFim x desc dict
insere (h:t) desc [] = [R (h,Nothing) (insere t desc [])]
insere (h:t) desc (R (a,b) l:d) | h == a = R (a,b) (insere t desc l):d
                                | otherwise = R (a,b) l : insere (h:t) desc d

insereFim:: Char -> String -> Dictionary -> Dictionary
insereFim x desc [] = [R (x,Just desc) []]
insereFim x desc (R (a,b) l:t) | x == a = R (a,Just desc) l:t
                               | otherwise = R (a,b) l : insereFim x desc t



