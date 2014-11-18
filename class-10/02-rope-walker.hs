import Control.Monad 
{-
  Модифицируйте имеющуюся реализацию задачи о канатоходце (лекция 9) следующим образом:
  1) реализуйте загрузку входных данных из файла следующего вида:
       R 2
       L 3
       R -1
       B
       L 1
     и вычисление соответствующего им результата (в решении может пригодиться 
     функция foldr (<=<) return — проверьте её тип для получения подсказки);
  2) замените монаду Maybe на Either String так, чтобы в случае падения канатоходца
     можно было получить информацию о его причинах (нарушение баланса и в какую
     сторону или банан на канате);
  3) реализуйте операцию landBoth, поддерживающую одновременное (атомарное) приземление
     птиц на оба конца шеста, и внесите соответствующие изменения в другие функции;
  5) реализуйте операцию unlandAll (одновременный вылет всех птиц с шеста) и внесите
     соответствующие изменения в другие функции;
  4) организуйте масштабное тестирование.
-}

type Birds = Int

type Pole = (Birds, Birds)

balance = 3


updatePole :: Pole -> Either String Pole
updatePole (l,r)
  |(unbalanced (l,r)) && l>r = Left ("Left")
  |(unbalanced (l,r)) && l<r = Left ("Right")
  |otherwise = Right(l,r)

unbalanced (l, r) = abs (l - r) > balance

landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left, right) = updatePole (left + n, right)

landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right) = updatePole (left, right + n)

landBoth :: Birds -> Pole -> Either String Pole
landBoth n (left, right) = updatePole (left+n, right + n)


unlandAll  = const (Right (0, 0))


banana = const $ Left "banana!!!!"



read_file fname = (map (read_item . words) . lines) `liftM` (readFile "02.txt")


read_item [x,y]
 | x == "M" = landBoth $ read y
 | x == "L" = landLeft $ read y
 | x == "R" = landRight $ read y
 | x == "B" = banana
 | x == "U" = unlandAll


run fname = read_file  fname 




tests = all test [1..3]
  where
    test 1 = (return (0, 0) >>= landLeft 1 >>= landRight 4 
              >>= landLeft (-1) >>= landRight (-2)) == Left "Right"
    test 2 = (return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2) == Right (2, 4)
    test 3 = (return (0, 0) >>= landLeft 1 >>= banana >>= landRight 1) == Left "banana!!!!"


