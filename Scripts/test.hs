module Coisas where

import Data.Char

digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (h:t) | isDigit h = (h:ar,br)
                 | isAlpha h = (ar,h:br)
                 | otherwise = (ar,br)
    where (ar,br) = digitAlpha t

nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) | h > 0     = (a+1,b,c)
          | h == 0    = (a,b+1,c)
          | otherwise = (a,b,c+1)
    where (a,b,c) = nzp t

divMod' :: Integral a => a -> a -> (a, a)
divMod' n x | n - x < 0 = (0,n)
           | otherwise = (q+1,r)
    where (q,r) = divMod' (n-x) x

anyone:: (a -> Bool) -> [a] -> Bool
anyone _ [] = False
anyone f (h:t) = f h || anyone f t

zipWi:: (a->b->c) -> [a] -> [b] -> [c]
zipWi f [] _ = []
zipWi f _ [] = []
zipWi f (x:xs) (y:ys) = f x y:zipWi f xs ys

takeWi :: (a->Bool) -> [a] -> [a]
takeWi f [] = []
takeWi f (h:t) | f h = h:takeWi f t
               | otherwise = []

dropWi :: (a->Bool) -> [a] -> [a]
dropWi f [] = []
dropWi f (h:t) | f h = dropWi f t
               | otherwise = (h:t)

spam :: (a-> Bool) -> [a] -> ([a],[a])
spam _ [] = ([],[])
spam f (h:t) | f h = let (taken, dropped) = span f t in (h:taken, dropped)             
             | otherwise = ([], h:t)

deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy _ _ [] = []
deleteBy f n (h:t) | f n h = t
                   | otherwise = h:deleteBy f n t

sortOff :: Ord b => (a -> b) -> [a] -> [a]
sortOff _ [] = []
sortOff f (h:t) = insere f h (sortOff f t)

insere:: Ord b => (a -> b) -> a -> [a] -> [a]
insere _ _ [] = []
insere f n (h:t) | f n >= f h = h:insere f n t
                 | otherwise = n:h:t

type Mat a = [[a]]

m1:: Mat Int
m1 = [ [1,2,3],
       [0,4,5],
       [0,0,6]
     ]

m2:: Mat Int
m2 = [ [3,1,2],
       [0,2,3],
       [0,0,7]
     ]

dimOK :: Mat a -> Bool
dimOK m = let d = length (head m)
          in and (map(\l -> length l == d) m)

dimMat :: Mat a -> (Int,Int)
dimMat l = (m,n)
    where m = length l
          n = length (head l) 
{-
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat (x:xs) (y:ys) = zipWith (+) x y:addMat xs ys
-}

-- Soma de duas matrizes através de funções de ordem superior

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat = zipWMat (+)

-- Soma de duas matrizes através de recursividade

addMatRecur:: Num a => Mat a -> Mat a -> Mat a
addMatRecur [] [] = []
addMatRecur (x:xs) (y:ys) = addLine x y:addMatRecur xs ys 

-- Soma de 2 linhas de matrizes

addLine:: Num a => [a] -> [a] -> [a]
addLine [] [] = []
addLine (x:xs) (y:ys) = x+y:addLine xs ys

-- Transposta de uma matriz através de funções de ordem superior

transpose':: Mat a -> Mat a
transpose' ([]:_) = []
transpose' m1 = firstCol m1:transpose'(otherCols m1)

firstCol:: Mat a -> [a]
firstCol m = map head m

otherCols:: Mat a -> Mat a
otherCols m = map tail m

-- Função de ordem superior para a soma de duas matrizes

zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat = zipWith . zipWith

data BTree a = Empty 
            | Node a (BTree a) (BTree a) deriving Show

t1:: BTree Int
t1 = Node 5 (Node 3 Empty (Node 2 Empty Empty))
            (Node 4 Empty Empty)

t2:: BTree Int
t2 = Node 17 (Node 15 Empty Empty)
             (Node 13 (Node 10 Empty Empty)
                      (Node 9 Empty Empty))

altura :: BTree a -> Int
altura Empty = 0
altura (Node x e d) = 1 + max(altura e) (altura d)

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node x e d) = 1 + contaNodos e + contaNodos d

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node x Empty Empty) = 1
folhas (Node x e d) = folhas e + folhas d

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty
prune n (Node x e d) = Node x (prune (n-1) e) (prune (n-1) d)

path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node x _ _) = [x]
path (h:t) (Node x e d) | h = x:path t e
                        | otherwise = x:path t d

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node x e d) = Node x (mirror d) (mirror e)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node x e d) (Node x' e' d') = Node (f x x') (zipWithBT f e e') (zipWithBT f d d')
zipWithBT _ _ _ = Empty

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) e d) = (Node a e1 d1,Node b e2 d2,Node c e3 d3)
    where (e1,e2,e3) = unzipBT e 
          (d1,d2,d3) = unzipBT d

minimo :: Ord a => BTree a -> a
minimo (Node x Empty _) = x
minimo (Node x e d) = minimo e

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node x Empty d) = d
semMinimo (Node x e d) = Node x (semMinimo e) d

minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node x Empty d) = (x,d)
minSmin (Node x e d) = (min,Node x (semMin) d)
    where min    = minimo e 
          semMin = semMinimo e

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove n (Node x e d) | n > x = Node x e (remove n d) 
                      | n < x = Node x (remove n e) d
                      | otherwise = case d of
                                            Empty -> e
                                            _     -> let (g,h) = minSmin d in Node g e h

data RTree a = R a [RTree a]

rt:: RTree Int
rt = R 2 [R 3[ R 8 [],
               R 1 []],
               
          R 7 [],
          R 4 [R 14 []]
          ]

somas :: Num a => RTree a -> a
somas (R x []) = x
somas (R x l) = x + sum (map somas l)

alturas :: RTree a -> Int
alturas (R x []) = 0
alturas (R x l) = 1 + maximum (map alturas l)

prunes :: Int -> RTree a -> RTree a
prunes 0 (R x l) = (R x [])
prunes n (R x l) = R x (map(prunes (n-1)) l)

mirrors :: RTree a -> RTree a
mirrors (R x []) = (R x [])
mirrors (R x l) = R x (map mirrors (reverse l))

postOrder:: RTree a -> [a]
postOrder (R x []) = [x]
postOrder (R x l) = concatMap postOrder l ++ [x]

data LTree a = Tip a | Fork (LTree a) (LTree a)


-- Ex teste

transforma:: String -> String
transforma [] = []
transforma s = unlines(transposta (lines s))

transposta:: [String] -> [String]
transposta ([]:_) = []
transposta s = fstCol s:transposta(lstCol s)

fstCol:: [String] -> String
fstCol s = map head s

lstCol:: [String] -> [String]
lstCol s = map tail s

-- Recebe lista de ints e devolve lista de doubles

intToDouble:: [Int] -> [Double]
intToDouble [] = []
intToDouble l = map fromIntegral l

-- Recebe lista de ints e devolve o dobro de cada elemento da lista

doubleAll:: [Int] -> [Int]
doubleAll l = map (*2) l

-- Recebe lista de ints e devolve lista de string

intToString:: [Int] -> [String]
intToString l = map show l

-- Lista de Strings para uma lista com o tamanho de cada string

listSize:: [String] -> [Int]
listSize l = map length l

-- Lista de strings para uma lista de strings mas com mais uma coisa

stringTitle:: [String] -> [String]
stringTitle l = map (\x -> "Title: " ++ x) l

-- Lista de floats e devolve a taxa de cada elemento

taxRate:: [Float] -> [Float]
taxRate l = map (\x -> x+x*0.10) l

-- Lista de ints e devolve o incremento de cada elemento

incrementAll:: [Int] -> [Int]
incrementAll l = map (+1) l

