{-
   Дан текстовый файл, содержащий данные о нескольких студентах в следующем формате: три последовательные
   строки соответствуют имени, возрасту и номеру группы (например, 4.8). Определить соответствующий тип
   данных (data) и организовать чтение файла в список значений этого типа.

   Для двух данных файлов объединить загруженные списки в один список, упорядоченный по имени и сохранить
   результат в новый файл того же формата. Указание: всюду следует использовать монадический синтаксис
   (операции >>= и >>, функции ap, liftM и другие функции монадической обработки данных, использование
   блока do не допускается).
-}

import System.Environment
import Control.Monad
import Data.List
import Data.Ord


data Students = Students {name::String, age::Int, number_group::String}
                                               deriving (Show,Ord,Eq)

read_to_student_array [] = []
read_to_student_array (n:a:number:xs) = [Students n (read a) number]:read_to_student_array (xs)


read_from_file fname = readFile fname >>= return . read_to_student_array . lines

write_to_file fname xs = writeFile fname $ unlines $ map show xs

union_lists  fname1 fname2 = (++) `liftM` (read_from_file fname1) `ap` (read_from_file fname2)  >>= write_to_file "new_list.txt" .sort
main =  union_lists "list1.txt" "list2.txt"
