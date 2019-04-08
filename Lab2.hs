-- Задание 1 Список натуральных чисел.
n1 :: Integer -> [Integer]
n1 0 = []
n1 x = n1 (x-1) ++ [x]
-- Задание 2 Список нечетных натуральных чисел.
n2 :: Integer -> [Integer]
n2 0 = [ ]
n2 x = n2 (x-1)++[2*x-1]
-- Задание 3  Список четных натуральных чисел.
n3 :: Integer -> [Integer]
n3 0 = [ ]
n3 x = n3 (x-1)++[2*x]
-- Задание 4 Список квадратов натуральных чисел.
n4 :: Integer -> [Integer]
n4 0 = [ ]
n4 x = n4 (x-1)++[x*x]
-- Задание 5  Список факториалов.
n5 :: Integer -> [Integer]
fact 0 = 1
fact x = x*fact(x-1)
n5 0 = [ ]
n5 x = n5 (x-1)++[fact(x)]  
-- Задание 6 Список степеней двойки.
n6 :: Integer -> [Integer]
func 1 = 1
func x = 2*func(x-1)
n6 0 = [ ]
n6 x = n6 (x-1)++[func(x+1)] 
-- Задание 7  Список треугольных чисел.
n7 :: Integer -> [Integer]
t 1 = 1
t(x) = x+t(x-1)
n7 0 = [ ]
n7 x = n7 (x-1)++[t(x)]
-- Задание 8  Список пирамидальных чисел.
n8 :: Integer -> [Integer]
funct 1 = 1
funct(x) = x+funct(x-1)
p 1 = 1
p x = funct(x) + p(x-1)
n8 0 = [ ]
n8 x = n8 (x-1) ++ [p(x)]
--Вариант 8
--Задание 2.8 (11)
--Функция countTrue :: [Bool] -> Integer, возвращающая 
--количество элементов списка, равных True.
countTrue :: [Bool] -> Integer
countTrue [] = 0
countTrue [x] = if x then 1 else 0
countTrue (x:xs) = if x then 1 + (countTrue xs) else countTrue xs
--Задание 2.11 
--Функция substitute :: Char -> Char -> String -> String,
--которая заменяет в строке указанный символ на заданный. Пример: 
--substitute ’e’ ’i’ "eigenvalue" возвращает "iiginvalui"
substitute :: Char -> Char -> String -> String
substitute c1 c2 [] = ""
substitute c1 c2 (h:t) = if c1 == h
                then 
                    c2:substitute c1 c2 t
                else
                    h: (substitute c1 c2 t)
