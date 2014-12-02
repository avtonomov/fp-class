{-
   Модифицируйте представленное на лекции решение задачи о запросе пароля,
   удовлетворяющего требованиям по стойкости, следующим образом:
   - в командной строке задаются ограничения (минимальная длина, наличие букв,
     наличие цифр, наличие знаков пунктуации);
   - к стеку монад добавляется монада Reader, и с её помощью организуется
     доступ к ограничениям в функции isValid;
   - все попытки ввода пароля протоколируются средствами монады Writer.
-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Writer
import System.Environment

import Data.Char
type Args = [Int]



parse_args arg = map (read) arg


isValid s  = do
          (len:letter:numbers:punctuation:_) <- ask
          return $ length s >= len 
                 && if (letter==1) then any isAlpha s else True
                 && if (numbers==1) then any isNumber s else True
                 && if (punctuation==1) then any isPunctuation s else True


getValidPassword = do
  liftIO $ putStrLn "Введите новый пароль:"
  s <- liftIO getLine
  tell [s]
  args <- lift ask
  guard (runReader (isValid s) args)
  return s
 

askPassword = do
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Сохранение в базе данных..."

main = do
 args <- getArgs
 result <- runWriterT (runReaderT (runMaybeT askPassword) (parse_args args))
 return $ result