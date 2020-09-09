--ALUNA: RENATA CRISTINA GOMES DA SILVA--
--MATRÍCULA: 11721BCC012 --

-- Exercício 1 --
triangulo::(Int, Int, Int) -> String
triangulo (a,b,c) 
    | (a+b+c)>180 = "Nao eh triangulo"
    | a==b && b==c && a==c = "Triangulo Equilatero"
    | a==90 || b==90 || c==90 = "Triangulo Retangulo"
    | a>90 || b>90 || c>90 = "Triangulo Obtuso"
    | otherwise = "Triangulo Simples"


-- Exercício 2 --
equacao::(Float,Float,Float)->(Float,Float) 
equacao (a,b,c) = if a /= 0 then ( ((-b + sqrt(b*b - 4 * a*c))/2*a) , ((-b - sqrt(b*b - 4 * a*c))/2*a) ) 
    else ((-c / b), (a)) 


-- Exercício 3 --
type Data=(Int, Int, Int) 
passagem::Float->Data->Data->Float 
passagem p (d,m,a) (d1, m1, a1)
    |a1 - a < 2 = p-(p*0.15)
    |a1 - a <= 10 = p-(p*0.4)
    |a1 - a >= 70 = p-(p*0.5)
    |otherwise = p


-- Exercício 4 --
-- A --
gera1 = [x^2 | x <-[4..14], odd x] --[25,49,81,121,169]--
-- B --
gera2 = [(x, x*2) | x<-[1..4], x>1 && x<4] --[(2,4),(3,6)]--
-- C --
gera3 = [ y | x<-[10..15], y<-[1..x]] --[1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,11,1,2,3,4,5,6,7,8,9,10,11,12,1,2,3,4,5,6,7,8,9,10,11,12,13,1,2,3,4,5,6,7,8,9,10,11,12,13,14,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]--
-- D --
gera4 = [(x,y) | x<-[1..16], y<-[1..16], odd x, even y, x == y-1]--[(1,2),(3,4),(5,6),(7,8),(9,10),(11,12),(13,14),(15,16)]--
-- E --
gera5 = [(x+y) | x<-[1..16], y<-[1..16], odd x, even y, x == y-1]--[3,7,11,15,19,23,27,31]--



-- Exercício 5 --
-- A --
contaNegM2 a = length[x | x<-a, x < 0 && mod x 2 == 0] 
-- B --
listaNegM2 a = [x | x<-a, x < 0 && mod x 2 == 0] 


-- Exercício 6 --
distancias::[(Float, Float)] -> [Float] 
distancias x = [sqrt((x^2)+(y^2))| (x,y) <-x] 


-- Exercício 7 --
divisores::Int->[Int] 
divisores n = [x | x<-[1..n], (mod n x)==0] 

primos :: Int -> Int -> [Int] 
primos a b = [n | n <- [a .. b], (divisores n) == [1, n]] 


-- Exercício 8 --
mdc::(Int,Int) -> Int 
mdc (x,y)
 |y == 0 = x 
 |otherwise = mdc (y, (mod x y)) 

mmc2::Int->Int->Int
mmc2 n1 n2 =  div (n1*n2) (mdc (n1,n2)) 

mmc::Int->Int->Int->Int 
mmc n1 n2 n3 = mmc2 n1 (mmc2 n2 n3) 


-- Exercício 9 -- 
calculaserie::Float->Int->Float 
calculaserie x 1 = 1/x 
calculaserie x y 
    |(even y) = x / (fromIntegral y) + (calculaserie x(y-1)) 
    |(odd y) = (fromIntegral y) + (calculaserie x(y-1))


-- Exercício 10 --
fizzbuzz::Int->[String]
fizzbuzz 0 = [] 
fizzbuzz x
    | (mod x 3 == 0) && (mod x 5 == 0) = fizzbuzz(x-1) ++ ["Fizzbuzz"] 
    | (mod x 3 == 0) = fizzbuzz(x-1) ++ ["Fizz"] 
    | (mod x 5 == 0) = fizzbuzz(x-1) ++ ["Buzz"] 
    | otherwise = fizzbuzz(x-1) ++ ["No"] 

-- Exercício 11 -- 
conta__ocorrencias::Int->[Int]->Int
conta__ocorrencias y [] = 0 
conta__ocorrencias y (x:xs) 
  | x == y = 1 + (conta__ocorrencias y xs) 
  | otherwise = conta__ocorrencias y xs 

conta_ocorrencias::Int->Int->[Int]->(Int, Int) 
conta_ocorrencias v1 v2 l1 = (conta__ocorrencias v1 l1, conta__ocorrencias v2 l1) 


-- Exercício 12 --
unica_ocorrencia::Int->[Int]->Bool 
unica_ocorrencia n x = if(conta__ocorrencias n x==1)then True else False 


-- Exercício 13 --
intercala::[t]->[t]->[t]
intercala [] x = x
intercala x [] = x
intercala (t:xs) (b:ys) = t:b:intercala xs ys

-- Exercício 14 -- 
type Contato = (String, String, String, String) 
contatos :: [Contato] 
contatos = 
  [ ("Renata", "Rua 1", "11", "renata11@"),
    ("Cristina", "Rua 2", "22", "cristina22@"),
    ("Gomes", "Rua 3", "33", "gomes33@")
  ]
aux :: [Contato] -> String -> String 
aux [] email = "Email nao encontrado"
aux ((nome, _, _, emailInserido) : x) email 
  | emailInserido == email = nome 
  | otherwise = aux x email 
encontraContato :: String -> String 
encontraContato emailEncontrar = aux contatos emailEncontrar  


-- Exercício 15 --
type Pessoa = (String, Float, Int, Char)
pessoas::[Pessoa]
pessoas = 
  [ ("Rosa", 1.66, 27,'C'),
    ("João", 1.85, 26, 'C'),
    ("Maria", 1.55, 62, 'S'),
    ("Jose", 1.78, 42, 'C'),
    ("Paulo", 1.93, 25, 'S'),
    ("Clara", 1.70, 33, 'C'),
    ("Bob", 1.45, 21, 'C'),
    ("Rosana", 1.58,39, 'S'),
    ("Daniel", 1.74, 72, 'S'),
    ("Jocileide", 1.69, 18, 'S') 
  ]


-- Altura média --
tamanhoMedia::[Pessoa]->Float
tamanhoMedia t = (somaMedia t) / fromIntegral(length t)::Float 

somaMedia::[Pessoa]->Float
somaMedia [] = 0 
somaMedia ((_, altura, _, _):xs)= altura+somaMedia xs 


-- Idade pessoa mais nova --
lista::[Pessoa]->[Int]
lista [] = []
lista ((_,_,idade,_):xs) = [y | y<-idade:(lista xs)] 

idadeNova::[Pessoa]->Int
idadeNova x = minimum (lista x) 


-- Nome e estado civil da pessoa mais velha --
nomeEstado::[Pessoa]->[Pessoa]
nomeEstado [(nome,altura,idade,estado)] = [(nome,altura,idade,estado)]

nomeEstado ((nome,altura,idade,estado):(nome1,altura1,idade1,estado1):xs)
 |idade>idade1 = nomeEstado((nome,altura,idade,estado):xs)
 |otherwise = nomeEstado((nome1,altura1,idade1,estado1):xs)


--Dados de cada pessoa acima de 50 anos --
maisde50::[Pessoa]->[Pessoa]
maisde50 x = [(nome, altura, idade, sexo) | (nome, altura, idade,sexo)<-x, idade>=50]


--Numero de pessoas casadas com idade superior a i --

numeroDePessoas::[Pessoa]->Int->Int 
numeroDePessoas l1 n = length[(nome, altura, idade, sexo) | (nome, altura, idade, sexo)<-l1, sexo=='C', idade>n]

-- Exercício 16 --
insere_ord::Ord n => n -> [n] -> [n] 
insere_ord x [] = [x] 
insere_ord x (y:ys)
    | x<=y = (x:y:ys) 
    | otherwise = y:insere_ord x ys 


-- Exercício 17 --
reverte::[n]->[n] 
reverte [] = [] 
reverte (head:tail) = (reverte tail) ++ [head] 


-- Exercício 18 --
remove_repetidos:: Ord n => n -> [n] -> [n]
remove_repetidos x [] = [] 
remove_repetidos x (y:ys) 
    |x == y = remove_repetidos x ys 
    |otherwise = y:(remove_repetidos x ys) 

sem_repetidos :: Ord n => [n] -> [n] 
sem_repetidos [] = [] 
sem_repetidos (x:xs) = x:(sem_repetidos (remove_repetidos x xs)) 


-- Exercício 19 --
disponiveis :: [Int] 
disponiveis = [1, 2, 5, 10, 20, 50, 100] 
notasTroco :: Int -> [[Int]] 
notasTroco 0 = [[]]
notasTroco valor = [head : tail | head <- disponiveis, head <= valor, tail <- notasTroco (valor - head)] 
-- Exercício 20 --
