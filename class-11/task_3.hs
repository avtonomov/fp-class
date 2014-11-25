{- Пользуясь монадой State, реализовать функции для работы с очередью: enqueue и dequeue.-}

import Control.Monad.State

enqueue x = do
  xs <- get
  put $ xs ++ [x]


dequeue = do
  (x:xs) <- get
  put xs
  return x


simple_test = do
  dequeue
  enqueue 4
  a <- dequeue
  enqueue 4
  enqueue a


begin_Test = execState simple_test [1, 2, 3] ==[3, 4, 4,2]