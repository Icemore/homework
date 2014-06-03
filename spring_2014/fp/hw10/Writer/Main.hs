-- (0.5 балла)
import Control.Monad.Writer
import Test.HUnit

fac :: Int -> Writer String Int
fac 0 = fac 1
fac 1 = do
    tell "1"
    return 1
fac n = do 
    x <- censor wrap (fac (n-1))
    tell (" * " ++ (show n))
    return (n * x)
    where
        wrap str = "(" ++ str ++ ")"


main = fmap (const ()) $ runTestTT $ test
    [ runWriter (fac 0) ~?= (1,"1")
    , runWriter (fac 1) ~?= (1,"1")
    , runWriter (fac 2) ~?= (2,"(1) * 2")
    , runWriter (fac 3) ~?= (6,"((1) * 2) * 3")
    , runWriter (fac 4) ~?= (24,"(((1) * 2) * 3) * 4")
    , runWriter (fac 5) ~?= (120,"((((1) * 2) * 3) * 4) * 5")
    ]
