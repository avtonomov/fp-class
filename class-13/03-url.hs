import Parser
import SimpleParsers
import Control.Applicative hiding (many, optional)
import Control.Monad
import Data.List
import Data.Maybe

{-
   Определите тип данных, представляющий адрес URL следующего вида:

     <схема>://<логин>:<пароль>@<хост>:<порт>/<URL‐путь>?<параметры>#<якорь>

   Реализуйте соответствующий парсер, считая, что обязательными компонентами
   URL являются только схема с символами "://" и имя хоста, все остальные
   компоненты могут отсутствовать.
-}


data Scheme = FTP | HTTP | HTTPS | Unk String
              deriving Show
type Login = String
type Password = String
type Host = String
type Port = String
type Path = String
type Params = String
type Anchor = String

data URL = URL Scheme Login Password Host Port Path Params Anchor
            deriving Show

scheme = (string "https" >> return HTTPS) <|>
         (string "http" >> return HTTP) <|>
         (string "ftp" >> return FTP) <|>
         Unk `liftM` lowers
		 
login = do
	login <- many1 (sat (/= ':'))
	char ':'
	return  login
password = do
    pass <- many1 (sat (/= '@'))
	char '@'	
    return pass

host = do
	host_ <- manyl (sat (/= ':'))
	char ':'
	return host_

port = do
	port_ret <- manyl (sat (/= '/'))
	char '/'
	return port_ret
	
path = do
	path_ret <- manyl (sat (/= '?'))
	char '?'
	return path_ret
	
params = do
	params_ret <- manyl (sat (/= '#'))
	char '#'
	return params_ret
	
anchor = do
	anchor_ret <-many (sat $ const True)
	return anchor_ret
url = URL <$>
       scheme <*>
      (string "://" >> (temp "" login)) <*>
	  (temp) <*>
	  (host) <*>
	  (temp "" port) <*>
	  (temp "" path) <*>
	  (temp "" params) <*>
	  (temp "" anchor)
	  
  