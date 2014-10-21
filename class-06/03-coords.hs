{-
  Написать программу, которая в зависимости от параметров командной строки
  а) генерирует случайный текстовый файл содержащий декартовы координаты точек на плоскости
     (по одной точке в каждой строке);
  б) определяет по заданному файлу в указанном ранее формате количество точек в каждой
     из четвертей;
  в) отыскивает наиболее удалённую от начала координат точку.
-}

import System.Random
import System.IO
import System.Environment
import System.Directory
import Data.Char

task_1 file n = do
	rand_1 <- newStdGen
	rand_2 <- newStdGen
	writeFile file ( unlines ( map show ( take n (zipWith (\x y -> (x, y)) (randomRs (-100, 100) rand_1 :: [Int]) (randomRs (-100, 100) rand_2 :: [Int])))))

task_2 file  = do
     handle <- openFile file ReadMode
     contents <- hGetContents handle
     let (a1, a2, a3, a4) = foldl (\acc x -> temp acc (read x)) (0, 0, 0, 0) (lines contents)
     print (a1, a2, a3, a4)
     hClose handle

temp (a1, a2, a3, a4) (x, y)
	| x > 0 && y > 0 = (a1 + 1, a2, a3, a4)
	| x < 0 && y > 0 = (a1, a2 + 1, a3, a4)
	| x < 0 && y < 0 = (a1, a2, a3 + 1, a4)
	| otherwise = (a1, a2, a3, a4 + 1)




max_ (x1,y1) (x2,y2) = if ( max ((x1)^2 +(y1)^2) ((x2)^2 +(y2)^2) == (x1)^2 +(y1)^2) then (x1,y1) else (x2,y2)
	
task_3 file  = do
     handle <- openFile file ReadMode
     contents <- hGetContents handle
     let (a1, a2) = foldl (\acc x -> max_ acc (read x)) (0, 0) (lines contents)
     print (a1, a2)
     hClose handle

main = do
	 [task,file] <- getArgs
 	 case task of
          "1" -> task_1 file 10
          "2" -> task_2 file
          "3" -> task_3 file