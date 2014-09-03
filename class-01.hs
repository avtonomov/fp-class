-- IT314. Функциональное программирование
-- Занятие 1

-- 1) Функция без параметров (= константа)

hello :: String -- типовая аннотация (сигнатура)
hello = "Hello, world"

{-
  Запустите интерпретатор ghci (из каталога, в котором находится этот файл),
  загрузите этот файл:

> :load class-01

  и вызовите функцию hello:

> hello

-}

-- 2) Объявление функций

-- тип функции: Два параметра типа Double и результат того же типа
avg :: Double -> Double -> Double
avg a b = (a + b)/2

{-

  Пример вызова (передача параметров через пробел,
  пробел -- это операция вызова функции (применение)!):

> avg 5 9
7.0

  Функция может вызываться инфиксно:

> 5 `avg` 9
7.0

  а) Вычислите в ghci среднее арифметическое следующих пар чисел: 332 и 723, 34.34 и 93.27.
     Впишите ответы:527.5, 63.805

  б) Напишите функцию avg3, вычисляющую среднее арифметическое трёх заданных чисел.
     Проверьте результаты её работы на двух тройках чисел.

-}

avg3 :: Double -> Double -> Double -> Double
avg3 a b c = (a + b + c)/3

{-
   Результаты проверки:
    (3+5+7)/3 = 5
    (3+10+3)/3 = 5.333

   ???

-}

-- 3) Выражения

{-
   Вычислите и сохраните в этом файле значения следующих выражений,
   обращая внимание на обозначения и приоритеты операций, стандартные функции,
   расстановку скобок:

    2 + 3 = 5
    mod 10 4  = 2 
    10 `mod` 4 = 2 
    True && 5 < 10 = true
    5 < 7 || 10 > 3=true
    sqrt (-2)= nan 
    sqrt (sqrt 16)= 2.0
    let x = 4 in (sin x)^2 + (cos x)^2= 1.0
    x= <interactive>:18:1: Not in scope: `x'

    7^(-1)= *** Exception: Negative exponent
    error "AAAA!!!!"= *** Exception: AAAA!!!!

    12345^54321=большое число
    2 < 3 || 9999954321^99912345 > 12345^54321= true

-}

-- 4) Типы

{-
  Тип выражения можно узнать, воспользовавшись командой интерпретатора :t, например:

> :t 'a'
'a' :: Char
> :t 1
1 :: Num a => a

  Запись "1 :: Num a => a" означает, что выражение "1" имеет тип "a", где "a" принадлежит
  классу типов Num (имеет экземпляр класса типов Num, является числовым типом).

  Определите и сохраните в этом файле типы следующих выражений:
   5 :: Num a => a

   5.0 :: Fractional a => a

   sqrt 4 :: Floating a => a

   sqrt 4.0 :: Floating a => a

   2+3 :: Num a => a
   5 < 7 :: Bool

   if 2 > 3 then 7 else 5 :: Num a => a
   5 > 6 && False :: Bool


   Команда ":set +t" включает режим, при котором печатается тип каждого вычисляемого выражения.
   Команда ":set +s" включает режим, при котором печатается время вычисления каждого выражения.

-}

-- 5) Объявление функций (2)

-- а) Удвоение значения заданного числа
-- (объясните смысл типовой аннотации: ???)

--типовая аннотоция означает, что выражение double(т.е выходной параметр функции) имеет тип "a", где уже "a" 
  -- принадлежит классу типов Num

double :: Num a => a -> a
double a = a * 2

-- б) Утроение заданного числа
--    (типовую аннотацию и образцы параметров следует написать самостоятельно)
triple :: Num a => a -> a
triple a = a * 3

-- в) Определение наибольшего из трёх заданных целых чисел (можно воспользоваться стандартной
--    двухаргументной функцией max).
--max3 :: Ord a => a -> a -> a -> a
max3 :: Ord a => a -> a -> a -> a
max3 a b c = a `max` (max b c)

{-
  Проверка:
> max3 87 34 209  = 209
???
> max3 22 28 30 = 30
???
> max3 12 25 (-7) = 25
???

-}

-- г) Функция, возвращающая True тогда и только тогда, когда оба ее аргумента равны True
-- (пользоваться стандартными логическими операциями не следует, обратите внимание на
--  образцы параметров функции, последняя строка -- "во всех остальных случаях").
bothTrue :: Bool -> Bool -> Bool
bothTrue True True = True
bothTrue _  _ = False


-- д) Функция, возвращающая True, если только один из её аргументов равен True,
-- и False в противном случае (пользоваться стандартными логическими операциями не следует).
oneTrue :: Bool -> Bool -> Bool
oneTrue True False = True
oneTrue False True = True
oneTrue _ _ = False

-- е) Дана температура в градусах Фаренгейта. Вычислить соответствующую температуру
-- в градусах Цельсия.
f2c :: Double -> Double
f2c a = -32 * 5/9

{-
   ж) Найти наибольший общий делитель двух целых чисел, пользуясь
      алгоритмом Евклида (псевдокод):
      НОД(a, b) = НОД(b, a mod b), если b ≠ 0; 
      НОД(a, 0) = a.
-}
gcd' :: Integer -> Integer->Integer
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

-- з) Функция, возвращающая название дня недели по его номеру (от 1 до 7),
--    если номер неправильный, генерируется исключение (функция error).
dayOfWeek :: Int -> String
dayOfWeek 1 = "Понедельник"
dayOfWeek 2 = "Вторник"
dayOfWeek 3 = "Среда"
dayOfWeek 4 = "Четверг"
dayOfWeek 5 = "Пятница"
dayOfWeek 6 = "Суббота"
dayOfWeek 7 = "Воскресенье"
dayOfWeek _ = error "нет такого"


-- Далее типовые аннотации, если их нет, следует писать самостоятельно.

-- 6) Условное определение функции

-- Пример.
-- Определение знака числа (-1, 0, 1). Класс типов Ord определяет операции сравнения.
sign :: (Num a, Ord a) => a -> Int
sign a
   | a < 0 = -1
   | a == 0 = 0
   | otherwise = 1

{-
   а) Найти значение функции f(x), вычисляемое по правилу:
          −x,   если x ≤ 0,
	  x^2,  если 0 < x < 2,
          4,    если x ≥ 2.
-}

eval_f :: Integer-> Integer
eval_f a
   | a <= 0 = -1 * a
   | a > 0 && a < 2 = a^2
   | otherwise = 4



-- б) Написать функцию, возвращающую текстовую характеристику ("hot", "warm", "cool", "cold")
-- по заданному значению температуры в градусах Цельсия.
describeTemperature :: Double -> String
describeTemperature a
   | a < (-20) = "cold" 
   | a < 0 && a >(-20)  = "cool"
   | a > 0 && a < 20 = "warm"
   | otherwise = "hot"

{- 
   в) (*) Дан список температур в градусах Фаренгейта. Вывести для каждого значения
    соответствующую текстовую характеристику.

  Решение:
> map (describeTemperature . f2c) [82, 94, 50, 65, 34]
["cool","cool","cool","cool","cool"]

  В этом решении с помощью операции (.) строится композиция (суперпозиция) функций
  и получившаяся функция применяется функцией map к каждому элементу списка.
-}

-- 7) Рекурсия

-- Пример. Вычислить сумму всех целых чисел от 1 до n (где n >= 1):
sum_n 1 = 1
sum_n n
  | n > 1 = n + sum_n (n-1)
  | otherwise = error "n should be >= 1"

-- а) Вычислить сумму всех целых чисел от a до b включительно

sum_ab  a b
  | a < b = n + sum_ab(a+1)
  | otherwise = n
{-
   б) Числовая последовательность определяется следующим образом:
      a1 = 1, a2 = 2, a3 = 3, a_k = a_{k−1} + a_{k−2} − 2*a_{k−3}, k = 4, 5, ...
      Вычислить её n-й элемент.
-}
eval_a_n = undefined

-- в) Вычислить, пользуясь рекурсией, n-ю степень числа a (n - целое):
pow = undefined

-- г) Пользуясь ранее написанной функцией pow, вычислить сумму: 1^k + 2^k + ... + n^k.
sum_nk = undefined

-- д) Сумма факториалов чисел от 1 до n.
sum_fact 1 = 1
sum_fact n = undefined
  where
    fact n = undefined

-- е) Количество цифр целого числа
number_digits = undefined

-- ж) Проверить, является ли заданное число простым.
isPrime = undefined

-- 8) Разное

{-
   а) Дан номер года (положительное целое число). Определить количество дней в этом году,
  учитывая, что обычный год насчитывает 365 дней, а високосный — 366 дней. Високосным
  считается год, делящийся на 4, за исключением тех годов, которые делятся на 100 и
  не делятся на 400 (например, годы 300, 1300 и 1900 не являются високосными,
  а 1200 и 2000 — являются).
-}

nDays year = undefined
  where
    isLeap = undefined
