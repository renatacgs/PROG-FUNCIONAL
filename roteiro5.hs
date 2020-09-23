--RENATA CRISTINA GOMES DA SILVA--
--11721BCC012--

-- Exercício 1 --
-- A --
type Data = (Int,Int,Int)

bissexto :: Int -> Bool
bissexto b = if mod b 4 == 0 && mod b 100 /= 0 then True else False

valida :: Data -> Bool
valida (x,y,z)
   |bissexto z == True && y == 2 && x >= 1 && x <= 29 = resultado
   |bissexto z == False && y == 2 && x >= 1 && x <= 28 = resultado
   |x >= 1 && x <= 30 && y == 4 || y == 6 || y == 9 || y == 11  = resultado
   |x >= 1 && x <= 31 && y == 1 || y == 3 || y == 5 || y == 7 || y == 8 || y == 10 || y == 12 = resultado
   |otherwise = resultado1
   where
     resultado = True
     resultado1 = False

-- B --
bissextos :: [Int]-> [Int]
bissextos b = y
    where
      y = [x |x<-b,bissexto x]

-- C --
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

precede :: Emprestimo -> Data -> Bool
precede (codigo,matricula,(dia,mes,ano),(a,b,c),situacao) (x,y,z)
   |c < z = True
   |b < y && c == z = True
   |a < x && b == y && c == z = True
   |otherwise = False

bdEmprestimo::Emprestimos
bdEmprestimo =
    [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
    ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
    ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados [] data1 = []
atrasados (x:xs) data1 = if (precede x data1 == True) then a else b
   where
    a = x:atrasados xs data1
    b = atrasados xs data1

-- D --
aux :: (Int, Int) -> (Int, Int)
aux (x, y) = z
  where
    z = (y, x + y)

fibo2 :: Int -> (Int, Int)
fibo2 0 = (0, 1)
fibo2 n = z
  where
    z = aux (fibo2 (n -1))

-- E --
prodIntervalo :: Int->Int->Int
prodIntervalo m n                   
    |m == 0 || n == 0 = 0
    |m < n || m == n = m * prodIntervalo(m+1) n
    |otherwise = 1

fatorial2 :: Int -> Int
fatorial2 0 = 1
fatorial2 n = fat
   where
    fat = prodIntervalo 1 n

-- Exercício 2 --
-- A --
valida1 :: Data -> Bool
valida1 (x,y,z) = let 
 a = True
 b = False
   in 
   if bissexto z == True && y == 2 && x >= 1 && x <= 29 then a
   else if bissexto z == False && y == 2 && x >= 1 && x <= 28 then a
   else if x >= 1 && x <= 30 && y == 4 || y == 6 || y == 9 || y == 11  then a
   else if x >= 1 && x <= 31 && y == 1 || y == 3 || y == 5 || y == 7 || y == 8 || y == 10 || y == 12 then a
   else b

-- B --
bissextos1 :: [Int]-> [Int]
bissextos1 listab = let
   a = [x | x <- listab, bissexto x]
   in 
   a

-- C --
atrasados1 :: Emprestimos -> Data -> Emprestimos
atrasados1 [] data1 = []
atrasados1 (x:xs) data1 = let
   a = precede x data1
   b = x:atrasados1 xs data1
   c = atrasados1 xs data1
   in
   if (a == True) then b else c

-- D --
aux1 :: (Int, Int) -> (Int, Int)
aux1 (x, y) =
  let a = (y, x + y)
   in a

fibo2x :: Int -> (Int, Int)
fibo2x 0 = (0, 1)
fibo2x n =
 let a = aux1 (fibo2x (n -1))
 in a

-- E --
fatorial2x :: Int -> Int
fatorial2x 0 = 1
fatorial2x n = let 
   a = prodIntervalo 1 n
   in
   a

-- Exercício 3 --
--1) (λx. 2*x + 1) 3 = 2*3+1 = 6+1 = 7

--2) (λxy. x-y) 5 7 = 5-7 = -2

--3) (λyx. x-y) 5 7 = 7-5 = 2

--4) (λxy. x-y) (λz. z/2) = (λxy. x-y) z/2 = z/2-z/2 = 0

--5) (λxy. x-y) ((λz. z/2)6) 1 = (λxy. x-y) (6/2)1 = (λxy. x-y) 3 1 = 3-1 = 2

--6) (λ x. λ y. – x y ) 9 4 = 9-4 = 5

   
--Exercício 4--
a = (\x -> x + 3) 5 --8--
b = (\x -> \y -> x * y + 5) 3 4 --17--
c = (\(x,y) -> x * y^2) (3,4) --48--
d = (\(x,y,_) -> x * y^2) (3,4,2) --48--
e = (\xs -> zip xs [1,2,3]) [4,5,6] --[(4,1),(5,2),(6,3)]

-- Exercício 5 --
--a) (λx λy. y)((λz. z)(λz. z))(λw. w) 5 = (\x-> \y-> y)((\z-> z)(\z-> z))(\w-> w) 5 = 5

--b) ((λf. (λx. f(f x))) (λy. (y * y))) 3 = ((λf. (λx. (λy. (y * y))((λy. (y * y)) x))) (λy. (y * y))) 3
--((\f-> (\x-> (\y-> (y * y))((\y-> (y * y)) x))) (\y-> (y * y))) 3 = 81

--c) ((λf. (λx. f(f x)))(λy.(+ y y))) 5 = ((\f-> (\x->f(f x)))(\y->(y+y))) 5 = 20

--d) ((λx. (λy. + x y) 5) ((λy. - y 3) 7)) = ((\x-> (\y->x+y) 5) ((\y->y-3) 7)) = 9

--e) (((λf. (λx. f(f(f x)))) (λy. (y * y))) 2) =  (((\f->(\x->f(f(f x)))) (\y->(y * y))) 2) = 256

--f) (λx. λy. + x ((λx. - x 3) y)) 5 6 =  (\x-> \y->x+((\x->x-3) y)) 5 6 = 8
