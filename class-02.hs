-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.
sec2hms :: Int -> (Int, Int, Int)
sec2hms  n =(n `div` 3600, (n `mod` 3600) `div` 60,(n `mod` 3600) `mod` 60)

hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s) = h * 3600 + m * 60 + s

-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec'  a b c = hms2sec (a,b,c)

-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hms x)) [1,10..10000]

-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
-- б) периметр и площадь треугольника по координатам вершин.

type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2-y1)^2)

-- triangle :: ??? -> (Double, Double)
triangle (x1, y1) (x2, y2) (x3, y3) = (p,s)
  where
    p = (distance (x1, y1) (x2, y2)) + (distance (x3, y3) (x2, y2)) + (distance(x1, y1) (x3, y3))
    s = sqrt ( p*(p - distance (x1, y1) (x2, y2))* (p-distance (x3, y3) (x2, y2)) * (p-distance (x1, y1) (x3, y3)))

-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.

-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs)
  | x `mod` 2 ==0 = 1+nEven(xs)
  | otherwise =nEven(xs)  
  

-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
--   [1,2,3,4]
doubleElems :: Num a => [a] -> [a]
doubleElems[] = []
doubleElems (x:xs) = x*2 :doubleElems xs


-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs) 
   | x `mod` 2 ==1 = x:fltOdd xs
   | otherwise = fltOdd xs

-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;
-- б) увеличить элементы с чётными значениями в два раза;
-- в) переставить местами чётные и нечётные по порядку следования элементы
--    (для списков нечётной длины отбрасывать последний элемент).


flt_a :: Integral a => [a] -> [a]
flt_a [] = []
flt_a (x:xs) 
   | x >0 = x:flt_a xs
   | otherwise = flt_a xs
   
flt_b :: Integral a => [a] -> [a]
flt_b [] = []
flt_b (x:xs) 
   | x `mod` 2 == 1 =  x:flt_b xs
   | otherwise = 2*x :flt_b xs
   
   
swap_c :: Integral a => [a] -> [a]
swap_c [] = []
swap_c [x] = []
swap_c (x:xx:xs) = xx:x:swap_c xs
-- 2.5 
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих   элементов исходных списков. Предусмотреть ситуацию списков разной длины.
combine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) = x+y : combine_plus xs ys

-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.

task_2_6 [] ys = []
task_2_6 xs [] = []
task_2_6 (x:xs) (y:ys) = (x,y) : task_2_6 xs ys
-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;
-- б) в порядке возрастания.
natNfirst_up n = [1..n]


 
natNFirst_down n = abs_list [(-n)..(-1)]
  where 
    abs_list [] = []
    abs_list (x:xs) = (abs x) : abs_list (xs)


-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.
task_2_8  _ [] = []
task_2_8 a (x:xs) = x:a:task_2_8 a xs
-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).



--3



-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):
-- а) [a] -> Int -> a


find_ (x:xs) i
     |i>0 = find_ xs (i-1)
     |otherwise = x


-- б) Eq a => [a] -> a -> Bool

find_a [] _ = False
find_a (x:xs) a
     |a == x = True
     |otherwise = find_a xs a

-- в) [a] -> Int -> [a]

-- возвращение n первых элементов массива
ret (xs) ii = retu_rn xs 0
  where
   retu_rn (xs)  i 
     |i<=ii = (find_ (xs) (i)) : retu_rn (xs) (i+1) 
     | otherwise = []





-- г) a -> Int -> [a]
-- дублирование i раз эдемента а
dublicate a i 
  |i>0 = a:dublicate a (i-1)
  |otherwise = []
-- д)[a] -> [a] -> [a]

--сумма двух списков
--combine_plus [] ys = ys
--combine_plus xs [] = xs
--combine_plus (x:xs) (y:ys) = x+y : combine_plus xs ys

--разность двух списков
combine_min [] ys = []
combine_min xs [] = []
combine_min (x:xs) (y:ys) = (x-y) : combine_min xs ys



-- е) Eq a => [a] -> [[a]]


-- ж) [a] -> [(Int, a)]

-- инжексирование элементов массива
index_mass mass = numb_ 0 mass
   where
       numb_ _ [] = []
       numb_ i (y:ys) = (i,y): numb_ (i+1) (ys)

-- з) Eq a => [a] -> [a]
-- сравнение двух массивов на равенство
equal_mas [] [] = True
equal_mas (x:xs) (y:ys) 
  |x ==y = equal_mas (xs) (ys)
  |otherwise = False  