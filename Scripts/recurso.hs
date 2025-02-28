module Recurso where

-- Ficha 5 -> Matrizes e Polinomios

type Mat a = [[a]]

mat1 = [[1,2,3],
        [4,5,6],
        [7,8,9]]

mat2 = [[1,1,1],
        [2,2,2],
        [3,3,3]]

mat3 = [[1,2,3],
        [0,4,5],
        [0,0,6]]

dimOK:: Mat a -> Bool
dimOK m = let l = length (head m)
            in and (map (\x -> length x == l) m)

dimMat:: Mat a -> (Int,Int)
dimMat l = (m,n)
    where (m,n) = (length l,length (head l))
          
addMat:: Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat (x:xs) (y:ys) = zipWith (+) x y:addMat xs ys

auxTranspose:: Mat a -> Mat a
auxTranspose ([]:_) = []
auxTranspose l = firstCol l:auxTranspose(otherCols l)

firstCol:: Mat a -> [a]
firstCol [] = []
firstCol l = map head l

otherCols:: Mat a -> Mat a
otherCols [] = []
otherCols l = map tail l

triSup :: (Num a,Eq a) => Mat a -> Bool
triSup m = and [ all (== 0) [ m !! i !! j | j <- [0..i-1] ] | i <- [0..length m - 1]]

rotateLeft:: Mat a -> Mat a
rotateLeft ([]:_) = []
rotateLeft l = lastMatrix l:rotateLeft(otherMatrix l)

lastMatrix:: Mat a -> [a]
lastMatrix l = map last l

otherMatrix:: Mat a -> Mat a
otherMatrix l = map init l

rotateRight:: Mat a -> Mat a
rotateRight l = map reverse (auxTranspose l)

type Polinomio = [Monomio]
type Monomio = (Float,Int)

selgrau :: Int -> Polinomio -> Polinomio
selgrau n l = filter(\(x,y) -> y==n ) l

conta :: Int -> Polinomio -> Int
conta n l = length $ selgrau n l

grau :: Polinomio -> Int
grau l = maximum $ map snd l

deriv :: Polinomio -> Polinomio
deriv l = map(\(x,y) -> (x*fromIntegral y,y-1)) $ filter(\(x,y) -> y /= 0) l

calcula :: Float -> Polinomio -> Float
calcula n l = foldl(\acc (x,y) -> acc + x * n ^ y) 0 l

simp :: Polinomio -> Polinomio 
simp l = filter(\(x,y) -> x /= 0) l

data BTree a = Empty
                | Node a (BTree a) (BTree a)
        deriving Show

minimo :: Ord a => BTree a -> a
minimo (Node x Empty _) = x
minimo (Node x e d) = minimo e

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node x Empty d) = d
semMinimo (Node x e d) = (Node x (semMinimo e) d)



type Matrix = [[Int]]
-- Função fornecida no enunciado
stringToMatrix :: String -> Matrix
stringToMatrix s = map stringToVector (lines s)

-- a)
-- Função que recebe uma string e devolve uma lista de inteiros
-- Separamos a string pelas vírgulas e depois convertemos cada elemento da lista de strings para inteiro
stringToVector :: String -> [Int]
stringToVector s = read $ "[" ++ s ++ "]"

-- Exercício 3

data Lista a = Esq a (Lista a) | Dir (Lista a) a | Nula deriving (Show)

-- a)
-- Função que recebe uma lista e devolve a mesma lista sem o último elemento
-- A estratégia para perceber este problema e assumir que temos uma lista e vamos adicionando elementos à esquerda e à direita
semUltimo :: Lista a -> Lista a
-- Casos base existe apenas 1 elemento na lista
semUltimo (Esq x Nula) = Nula
semUltimo (Dir Nula x) = Nula  
-- Casos "recursivos" 
-- Como o elemento está na esquerda, vamos tirar o elemento mais à direita
semUltimo (Esq x l) = Esq x (semUltimo l)
-- Como o elemento "x" está na direita, vamos tirar o elemento
semUltimo (Dir l x) = l

-- b)
-- Em vez de estarmos a complicar o exercicio, convertemos a "Lista a" para uma lista normal e depois aplicamos a função show
-- Função que recebe uma "Lista a" e devolve uma lista de a
toList :: Lista a -> [a]
toList Nula = []
toList (Esq x l) = x : toList l
toList (Dir l x) = toList l ++ [x]

-- Como o show para listas normais já está definido, podemos usar a função show para converter a lista para string
showListaA :: Show a => Lista a -> String
showListaA = show . toList

-- Exercício 4

-- Função fornecida no enunciado
inorder :: BTree a -> [a]
inorder Empty = []
inorder (Node r e d) = (inorder e) ++ (r:inorder d)

-- a)
-- Utilizando a sugetão do enunciado, vamos utilizar a função auxiliar numeraAux para percorrer a árvore e atribuir um número a cada nó
numera :: BTree a -> BTree (Int, a)
-- Como a função numeraAux devolve um par, vamos usar o snd para devolver apenas a árvore
numera t = snd $ numeraAux 1 t

numeraAux :: Int -> BTree a -> (Int, BTree (Int, a))
-- Caso base, a árvore está vazia
numeraAux n Empty = (n, Empty)
-- Caso recursivo, vamos percorrer a árvore e atribuir um número a cada nó
-- Como a travessia é inorder, vamos atribuir o número ao nó da esquerda, depois ao nó raiz e depois ao nó da direita
numeraAux n (Node r e d) = (n2, Node (n1,r) e' d')
    where
        -- Atribuir o número ao nó da esquerda, n1 é o número do nó raiz
        (n1,e') = numeraAux n e
        -- Atribuir o número ao nó da direita
        (n2,d') = numeraAux (n1+1) d

-- b)
-- unInorder find all the possible trees that can be built from a given inorder traversal
-- Não vou mentir este exercicio é meio puxadito e é o tipo de exercicio para nao deixar um aluno tirar 20
unInorder :: [a] -> [BTree a]
-- Caso base, a lista está vazia
unInorder [] = [Empty]
-- Básicamente o que se está a fazer é percorrer a lista e para cada elemento da lista, vamos criar uma árvore com esse elemento como raiz
-- isso é o que se está a fazer com o c <- [0..length l-1]
-- Depois vamos criar as sub-árvores da esquerda e da direita
-- Para isso vamos usar a função unInorder e passar como argumento a lista até ao elemento c e depois a lista depois do elemento c
-- Para finalizar, temos que ter em consideração que o elemento c é um indice e nao um elemento da lista
-- Por isso temos que usar o operador (!!) para obter o elemento da lista
unInorder l = [Node ((!!) l c) e d | c <- [0..length l-1], e <- unInorder (take c l), d <- unInorder (drop (c+1) l)]