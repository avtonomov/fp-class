{-
  Разработайте утилиту со следующими возможностями:
  1) подсчёт количества строк в заданном текстовом файле;
  2) добавление заданной строки в начало (конец) заданного файла;
  3) преобразование всех буквенных символов заданного файла к верхнему
     регистру (результат выводится на консоль);
  4) построчное слияние двух заданных файлов (каждая строка первого файла
     соединяется с соответствующей строкой второго файла);
  5) генерация случайного текстового файла (случайность должна ограничиваться
     максимальным количеством строк в файле и символов в строке).

  Все входные данные программы должны передаваться с помощью параметров
  командной строки.
-}

import System.Random
import System.IO
import System.Environment
import System.Directory
import Data.Char

task_1 file = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  print (length (lines contents))
  hClose handle


task_2_end file str = appendFile file str

task_2_first file str = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  writeFile "temp_file.txt"  $ unlines $ str:(lines contents)
  hClose handle
  renameFile "temp_file.txt" file



task_3 file  = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  putStr $ map toUpper contents
  hClose handle

task_4 file_1 file_2 = do
  handle_1 <- openFile file_1 ReadMode
  contents_1 <- hGetContents handle_1
  handle_2 <- openFile file_2 ReadMode
  contents_2 <- hGetContents handle_2
  writeFile "temp_file.txt"  (merger (lines contents_1 ) (lines contents_2))
  hClose handle_1
  hClose handle_2
 
merger (x:[]) (y:[]) = x++y
merger (x:xs) (y:ys) = x++y++merger (xs) (ys)



 
task_5 file len count  = do
  if len > 0
     then do
     rand <- getStdGen
     str <- return $ take count $ randomRs ('a','z') rand
     appendFile file (str++"\n")
     task_5 file (len - 1) count



main = undefined
