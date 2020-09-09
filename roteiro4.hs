--ALUNA: RENATA CRISTINA GOMES DA SILVA--
--MATRÍCULA: 11721BCC012 --

-- Exercício 1 --
lst1 = [x*2 | x <- [1..10], x*2 >= 12] --[12,14,16,18,20] --
lst2 = [ x | x <- [50..100], mod x 7 == 3] -- [52,59,66,73,80,87,94]--
lst3 = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19] --[10,11,12,14,16,17,18,20] --
lst4=[(x,y)| x <- [1..4], y <- [x..5]] --[(1,1),(1,2),(1,3),(1,4),(1,5),(2,2),(2,3),(2,4),(2,5),(3,3),(3,4),(3,5),(4,4),(4,5)] --

-- Exercício 2--
quadrados::Int->Int->[Int]
quadrados x y = [n^2 | n<-[x..y]]

-- Exercício 3 --
seleciona_impares::[Int]->[Int]
seleciona_impares lista = [x | x<-lista, odd x]

-- Exercício 4 --
tabuada::Int->[Int]
tabuada x = [n*x | n<-[1..10]]

-- Exercício 5 --
bissexto2::Int->Bool
bissexto2 ano
  |(mod ano 400 == 0) = True
  |(mod ano 4 == 0) && (mod ano 100 /=0 ) = True
  | otherwise = False

bissextos::[Int]->[Int]
bissextos n = [x | x<-n, bissexto2 x]

-- Exercício 6 -- 
sublistas::[[Int]]->[Int]
sublistas lista = [x | y<-lista, x<-y]

-- Exercício 7 --
type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

bdEmprestimo :: Emprestimos
bdEmprestimo =
  [ ("H123C9", "BSI945", (12, 9, 2009), (20, 09, 2009), "aberto"),
    ("L433C5", "BCC021", (01, 9, 2009), (10, 09, 2009), "encerrado"),
    ("M654C3", "BCC008", (04, 9, 2009), (15, 09, 2009), "aberto")
  ]

valida :: Data -> Bool
valida (dia, mes, ano)
  | dia >= 1 && dia <= 31 && (mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 12) = True
  | dia >= 1 && dia <= 30 && (mes == 4 || mes == 6 || mes == 9 || mes == 11) = True
  | dia >= 1 && dia <= 28 && mes == 2 && not (bissexto2 ano) = True
  | dia >= 1 && dia <= 29 && mes == 2 && (bissexto2 ano) = True
  | otherwise = False

procede :: Data -> Data -> Bool
procede (dia, mes, ano) (dia2, mes2, ano2)
  | not (valida (dia, mes, ano)) || not (valida (dia2, mes2, ano2)) = False
  | ano > ano2 = False
  | ano == ano2 && mes > mes2 = False
  | ano == ano2 && mes == mes && dia > dia2 = False
  | otherwise = True

emprestimoEmDia :: Data -> Emprestimo -> Bool
emprestimoEmDia dataAtual (codigoLivro, codigoAluno, dataEmprestimo, dataDevolucao, status) =
  procede dataAtual dataDevolucao

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados listaEmprestimos dataAtual = [x | x <- listaEmprestimos, not (emprestimoEmDia dataAtual x)]

-- Exercício 8 --
npares::[Int]->Int
npares lista = length[x | x<-lista, even x]

-- Exercício 9 --
produtorio::[Int]->Int
produtorio [] = 1
produtorio (x:y) = x * produtorio y

-- Exercício 10 --
comprime::[[n]]->[n]
comprime lista = [x | y<-lista, x<-y]

-- Exercício 11 --
tamanho::[n]->Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

-- Exercício 12 --
membro :: Eq t => t -> [t] -> Bool
membro aux [] = False
membro aux (x : xs)
  | aux == x = True
  | otherwise = membro aux xs

uniaoNRec :: [Int] -> [Int] -> [Int]
uniaoNRec l1 l2 = [aux | aux <- l1] ++ [y | y <- l2, not (membro y l1)]

-- Exercício 13 --
uniaoRec2 a [] = a
uniaoRec2 a (x:xs)
 |elem x a = uniaoRec2 a xs
 |otherwise = uniaoRec2 (a ++ [x]) xs