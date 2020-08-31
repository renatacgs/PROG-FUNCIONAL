-- ALUNA: RENATA CRISTINA GOMES DA SILVA --
-- MATRÍCULA 11721BCC012 --

-- Exercício 1 A --
{-
(||):: Bool -> Bool -> Bool
True || True  = True
False || True = True
True || False = True
False  || False   = False

(||):: Bool -> Bool -> Bool
True  || _ = True
False || y = y

(||):: Bool -> Bool -> Bool
False || False = False
_    || _    = True

-- Exercício 1 B --
(||):: (Bool->Bool) -> Bool
(x,y)
    |True  _ = True 
    |False  False = False
    | _  True = True


(||):: Bool -> Bool -> Bool
(x y)  = if True || _ then True else if False || y then y
-}

-- Exercício 2 --
distancia::(Float,Float)->(Float,Float)->Float
distancia (x,y)(x1,y1) = sqrt(((x1-x)^2)+((y1-y)^2))

-- Exercício 3 --
{-
1:[2,3,4] -- [1,2,3,4] --
'a':['b','c','d'] -- "abcd" --
head [1,2,3] -- 1 --
tail [1,2,3] -- [2,3] --
[1,5,2,3]!!1 -- 5 --
[1,5,2,3]!!3 -- 3 --
elem 2 [1,5,2,3] -- True --
take 2 [1,5,2,3,7] -- [1,5] --
drop 2 [1,5,2,3,7] -- [2,3,7] --
[1,2] ++ [3,4] -- [1,2,3,4] --
[1..10] -- [1,2,3,4,5,6,7,8,9,10] --
[7,6..3] -- [7,6,5,4,3] --
['b'..'g'] -- "bcdefg" --
take 5 [1,3..] -- [1,3,5,7,9] --
sum [1..10] -- 55 --
maximum [1,5,2,3,7] -- 7 --
minimum [1,5,2,3,7] -- 1 --
-}

-- Exercício 4 --

--Fatorial com guardas --
fatorialg f
    |f==0 = 1
    |otherwise = f*fatorialg(f-1)

--Fatorial com casamento de padrão --
fatorialp:: Int -> Int
fatorialp 0 = 1
fatorialp x = x * fatorialp(x-1)

-- Exercício 5 --
fibo:: Int->Int
fibo 0 = 0
fibo 1 = 1
fibo 2 = 1
fibo x = fibo(x-2) + fibo(x-1)

-- Exercício 6 --
n_tri :: Int->Int
n_tri 0 = 0
n_tri 1 = 1
n_tri x = x + n_tri(x-1)

-- Exercício 7 A --
fibo2 :: Int-> (Int, Int)
fibo2 x = (fibo x,fibo(x+1))

-- Exercício 7 B --
fibo3 :: Int -> Int -> ((Int,Int),(Int,Int))
fibo3 x m = ((fibo x,fibo m),(fibo m, fibo(x+m)))

-- Exercício 7 C --
fibo4 :: Int -> (Int,Int)
fibo4 x = fibo2 x

-- Exercício 7 D --
fibo5 :: Int -> ((Int,Int),Int)
fibo5 x = ((fibo4 x),fibo n)

-- Exercício 8 --
potencia2 :: Int -> Int
potencia2 0 = 1
potencia2 x = 2 * potencia2(x-1)

-- Exercício 9 A --
prodIntervalo :: Int -> Int -> Int
prodIntervalo m n
 |m > n = 1
 |otherwise = m * prodIntervalo(m+1) n

-- Exercício 9 B --
fatorialPIntervalo :: Int -> Int
fatorialPIntervalo 0 = 1
fatorialPIntervalo x = prodIntervalo 1 x

-- Exercício 11 --
resto_div :: Int -> Int -> Int
resto_div m n 
  |m<n=m
  |otherwise = resto_div (m - n)n

div_inteira :: Int -> Int -> Int
div_inteira m n
  |m < n = 0
  |otherwise = 1 + div_inteira (m - n) n

-- Exercício 12 --
mdc :: (Int,Int) -> Int
mdc (m,0) = m
mdc (m,n) = mdc (n, (mod m n))

mdcg :: (Int,Int) -> Int
mdcg (m,n)
 | n == 0 = m
 | otherwise = mdcg (n, (mod m n))

-- Exercício 13 --
-- Com guardas --
binomialg :: (Int,Int) -> Int
binomialg (n,0) = 1
binomialg (n,k)
 | k == 0 = 1
 | k == n = 1
 | otherwise = binomialg (n-1,k) + binomialg (n-1,k-1)

-- Com casamento de padrão --
binomial :: (Int,Int) -> Int
binomial (n,0) = 1
binomial (n,k) = if (k == n) then 1 else binomial (n-1,k) + binomial (n-1,k-1)

-- Exercício 14 --

A) reverse [1,2,3,4,5] || [5..1] -- [5,4,3,2,1] --
B) [a,c,e] -- Não foi possivel obter resultados já que para colocar char dentro de uma lista ([conteúdo]) cada caractere tem que estar em aspas simples.
C) [1,4,7,10,13,16] = [1,4..16] -- [1,4,7,10,13,16] --
D) zip [1, (-2)..(-11)] [1,5..17] -- [(1,1),(-2,5),(-5,9),(-8,13),(-11,17)] --


-- Exercício 15 A --
lista1 :: Int -> Int -> [Int]
lista1 a b
  |a==b = [a]
  |a>b = []
  |otherwise = [a+1..b-1]

-- Exercício 15 B --
lista2 :: Int -> Int -> [Int]
lista2 a b 
  |a == b || a > b = []
  |even a == True = [a,a+2..b]
  |otherwise = [a+1,a+2..b]

 
