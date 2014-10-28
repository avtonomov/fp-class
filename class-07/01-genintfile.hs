{-
  Создать текстовый файл, содержащий случайные целые числа, разделённые пробелами
  и символами перевода строки. В командной строке должны задаваться следующие параметры:
  1) имя создаваемого файла;
  2) диапазон генерируемых случайных чисел: от и до;
  3) количество чисел в строке;
  4) количество строк в файле.
-}


import System.Random
import System.IO
import System.Environment
import System.Directory
import Data.Char
import Control.Monad(when)

gen_rand rand first_num last_num n =take n $ (randomRs (first_num, last_num) rand :: [Int])


--createStr n  = unwords  

temp file first_num last_num n_file n_str = do
  if n_file > 0
     then do
     rand <- newStdGen
     let num = map show (gen_rand rand first_num last_num n_str)
     appendFile file ((unwords $ (num))++"\n")
     temp file first_num last_num (n_file-1) n_str
  else
     return ()

createFile file first_num last_num n_file n_str = do
  writeFile file ("")
  temp file first_num last_num n_file n_str

main = do
 [fname, first_num, last_num, n_file, n_str] <- getArgs  
 createFile fname (read first_num :: Int) (read last_num :: Int) (read n_file :: Int) (read n_str :: Int)



