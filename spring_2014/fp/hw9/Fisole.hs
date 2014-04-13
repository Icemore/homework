-- (3 балла)

-- Список экспорта менять нельзя!
module Fisole
    ( Fisole, runFisole
    , abortF, putCharF, getCharF
    ) where

import System.IO
import System.IO.Error
import qualified Control.Exception as CE
import Data.Typeable

data Fisole a = Fisole (Handle -> IO a)

-- Второй аргумент - имя файла с которым будут работать функции putCharF и getCharF.
-- Если произошли какие-то ошибки ввода-вывода (т.е. исключения бросались), нужно их поймать и вернуть Left с каким-нибудь сообщением об ошибке.
runFisole :: Fisole a -> String -> IO (Either String a)
runFisole (Fisole f) fileName = eval `CE.catch` abort
    where 
        eval = do     
            h <- openFile fileName ReadWriteMode
            res <- f h `CE.finally` hClose h
            return $ Right res
        abort ex = return $ Left $ ioeGetErrorString $ (ex :: CE.IOException)

instance Functor Fisole where
    fmap f c = do
        r <- c
        return (f r)

instance Monad Fisole where
    return x = Fisole (\h -> return x)
    (Fisole f1 ) >>= k = Fisole c
        where 
            c h = do
                x <- f1 h
                (unpack $ k x) h
            unpack (Fisole f) = f


-- abortF s завершает вычисление, s - сообщение об ошибке.
abortF :: String -> Fisole a
abortF s = Fisole (\h -> CE.throw $ userError s)

putCharF :: Char -> Fisole ()
putCharF c = Fisole (\h -> hPutChar h c)

getCharF :: Fisole Char
getCharF = Fisole (\h -> hGetChar h)
