module Main where
import System.Environment
import System.IO
import Control.Monad.State
import Control.Applicative
import Data.Char (chr, ord, isNumber)
import System.FilePath
import Control.Arrow

data BrainFuck = PtrInc | PtrDec | ValInc | ValDec | Put | Get | Loop [BrainFuck]
    deriving (Show)

ctx :: String
ctx = " ========execution=====\n\n\n"
main :: IO ()
main = do
    args <- getArgs 
    case args of
        []  -> return ()
        [x] ->  ( readFile x ) >>= readProcessing >>= (\ src -> run $ reverse $ execState (parse src)  [] ) >> putChar '\n'
            where readProcessing src = putStrLn src >>  putStrLn ctx >> return src
        _ -> return ()





parse :: String -> State  [BrainFuck] String
parse  [] = return []
parse src =  case src of
    ('+':xs) -> get >>= (\s -> put (ValInc:s) ) >> (parse xs)
    ('-':xs) -> get >>= (\s -> put (ValDec:s) )  >> parse xs
    ('>':xs) -> get >>= (\s -> put (PtrInc:s) ) >> parse xs
    ('<':xs) -> get >>= (\s -> put (PtrDec:s) ) >> parse xs
    ('.':xs) -> get >>= (\s -> put (Put:s) ) >> parse xs
    (',':xs) -> get >>= (\s -> put (Get:s) ) >> parse xs
    ('[':xs) -> let (y,ys) = runState  (parse xs) []
                in get >>= (\s -> put ((Loop  ys):s) ) >> parse y
    (']':xs) ->  return []
    ('\n':xs) -> parse xs
    _:xs -> parse []

run :: [BrainFuck] -> IO ()
run s =   evalStateT ( run' s) (take 100 [0,0..],take 100 [0,0..])  >> return ()

run' :: [BrainFuck] -> StateT ([Int],[Int]) IO [BrainFuck]
run' s = case s of
    (ValInc:xs)  -> get >>= (\ ((p:ps),q) -> put ((p+1):ps,q)) >> run' xs
    (ValDec:xs)  -> get >>= (\ ((p:ps),q) -> put ((p-1):ps,q)) >> run' xs
    (PtrInc:xs)  -> get >>= (\ (ps,q:qs) -> put (q:ps,qs)  ) >> run' xs 
    (PtrDec:xs)  -> get >>=  (\ ((p:ps) ,qs) -> put  (ps,p:qs))  >> run' xs 
    (Put:xs)     -> get >>= ( \ (p:ps,q) -> (liftIO  $ putChar $ chr p) ) >> run' xs
    (Get:xs)     -> do
        (p:ps,q) <- get
        t        <- liftIO $ hGetLine stdin
        if isNum t
        then do
            put ((read t):ps, q) 
            run' xs
        else run' xs
            where
                isNum :: String -> Bool
                isNum s = let b = foldl1 (&&) $ map isNumber s
                          in b
    all@((Loop bf):xs) -> run' bf >> get >>= (\(p:_,s) -> case p of
                                                            0 -> run' xs
                                                            n -> run' all)
    [] -> return []

