{-1. Написать программу, работа которой управляется конфигурационным файлом, содержащим строки следующего формата:
имя поля=значение
Возможными именами полей являются summand (слагаемое), multiplier (множитель), divisor (делитель). Все значения
являются целыми числами. В качестве параметров командной строки программе подаются имя конфигурационного файла
и имя текстового файла с целочисленными данными. Над каждым целым числом из второго файла выполняются операции,
указанные в конфигурационном файле, то есть число складывается, умножается и делится соответственно.
Если какое-либо поле отсутствует, то действие не выполняется. Результаты вычислений выводятся на консоль.
Организовать доступ к параметрам конфигурационного файла средствами монады Reader.
-}

import Control.Monad.Reader
import System.Environment


read_operation s = do
  let (field_name,value) = span (/= '=') s
  do_this field_name  $ (\x -> read x) . tail $ value

do_this "summand" num = (+ num)
do_this "multiplier" num = (* num)
do_this "divisor"   num = (`div` num)


main = do
  [fname_operation, fname_numbers] <- getArgs
  numbers <- fmap (map (\x -> read x) . lines)  $ readFile fname_numbers
  operation <- fmap (map read_operation . lines) $ readFile fname_operation
  print $ map (\x -> (runReader (temp x)) operation) numbers

temp n = do
  operation <- ask
  return $ foldl (flip ($)) n operation 