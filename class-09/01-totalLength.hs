import System.Environment
import System.Random
import System.IO
import System.Directory
import Data.Char
{-
  Написать функцию, которая по заданному списку строк возвращает сумму длин всех строк.
-}

totalLength :: [String] -> Int
totalLength (xs) = foldl (\acc x ->  acc + length x) 0 (xs)

{-
  Написать функцию, которая по заданному символу и целому числу n строит список строк,
  содержащих 1, 2, ..., n повторений символа. Функция должна возвращать Nothing, если n=0.
-}

build1 :: Char -> Int -> Maybe [String]
build1 ch 0 = Nothing
build1 ch n = Just([replicate acc ch | acc <- [1..n]])

 


{-
  Написать функцию, аналогичную по возможностям функции build1, но возвращающую при этом
  значение Either String [String], в котором значение слева должно свидетельствовать об
  одной из следующих особых ситуаций: 
  (*) n=0;
  (*) n > 100;
  (*) Роспотребнадзор запрещает создавать строки из символа 'x'.
-}

build2 :: Char -> Int -> Either String [String]
build2 ch n
     |n == 0 = Left ("n=0")
     |n >100 = Left ("n>0")
     |ch =='x' = Left ("Роспотребнадзор запрещает создавать строки из символа 'x'")
     |otherwise = Right([replicate acc ch | acc <- [1..n]])

{-
  Параметрами командной строки являются имя файла, символ, целое число.
  1) Пользуясь функцией totalLength и возможностями IO, как функтора, подсчитать и
     вывести общую длину строк, переданных программе в качестве аргументов командной строки.
  2) Пользуясь функцией totalLength и возможностями IO, как функтора, подсчитать и вывести общую
     длину строк, содержащихся в заданном текстовом файле (результат readFile должен быть
     предварительно преобразован к списку строк).
  3) Пользуясь функцией totalLength, подсчитать общую длину строк для значений в контекстах,
     сформированных функциями build1 и build2 (в решении следует пользоваться возможностями
     Maybe и Either String как функторов).
-}
{-
[file, ch,n] <- getArgs
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  print (totalLength (lines contents))
  hClose handle
-}


main = do
  [file, ch,n] <- getArgs
  let num = (read n :: Int)
  task1 <- fmap totalLength getArgs
  task2 <- fmap (totalLength.lines) (readFile file)
  let task3 = fmap (totalLength) (build1 (head ch) num)
  let task31 = fmap (totalLength) (build2 (head ch) num)
  print task1
  print task2
  print task3
  print task31