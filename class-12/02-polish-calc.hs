{-
   Представленный на лекции вычислитель выражений в обратной польской нотации
   не проверяет корректность заданного выражения, выдавая, к примеру, следующие
   очевидно неправильные ответы:

   ghci> evalRPN "* 1"
   1
   ghci> evalRPN "+ * 2 4"
   4
   ghci> evalRPN "* * *"
   *** Exception: 01-polish-calc.hs:10:15-43: Non-exhaustive patterns in lambda

   1. Переработайте реализацию вычислителя таким образом, чтобы в случае ошибки ответ
   не выводился. Воспользуйтесь в решении монадой Maybe, совместив её с монадой State
   с помощью преобразователя монад.

   2. Добавьте к вычислителю поддержку переменных. Значения переменных должны
   задаваться в командной строке, а доступ к ним должен осуществляться средствами
   монады Reader.

   3. Добавьте к вычислителю подсчёт количества операций со стеком (монада Writer
   с журналом типа Sum Int).
-}


import System.Environment
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
 

type Stack = [Int]
 
push x = tell (Sum 1) >> get >>= put . (x:)

pop = tell (Sum 1) >> get >>= \xs -> do 
                              guard (not $ null xs)
                              put $ tail xs
                              return $ head xs



step "+" = processTops (+)
step "*" = processTops (*)
step  n  = push (read n)
processTops op = op `liftM` pop `ap` pop >>= push
 
evalRPN :: String -> (Maybe Int, Int)
evalRPN xs = (if val == Nothing then Nothing else Just (head res), getSum sum)
 where
   ((val, sum), res) = runState (runWriterT (runMaybeT (mapM step $ words xs))) []
