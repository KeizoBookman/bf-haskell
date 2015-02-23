module Main where
import System.Environment
import Control.Monad.State
import Control.Applicative
import Data.Char (chr, ord, isNumber)
import System.FilePath
import Control.Arrow

data BrainFuck = PtrInc | PtrDec | ValInc | ValDec | Put | Get | Loop [BrainFuck]
    deriving (Show)

ctx :: String
ctx = " ========Compile Time execution=====\n\n\n"
main :: IO ()
main = do
    args <- getArgs 
    case args of
        []  -> return ()
        [x] ->  ( readFile x ) >>= readProcessing >>= (\ src -> run $ execState(parse src)  [] ) >> return ()
            where readProcessing src = putStrLn src >>  putStrLn ctx >> return src
        _ -> return ()





parse :: String -> State  [BrainFuck] String
parse  [] = return []
parse src =  case src of
    ('>':xs) -> get >>= (\s -> put (PtrInc:s) ) >> parse xs
    ('<':xs) -> get >>= (\s -> put (PtrDec:s) ) >> parse xs
    ('+':xs) -> get >>= (\s -> put (ValInc:s) ) >> (parse xs)
    ('-':xs) -> get >>= (\s -> put (ValDec:s) )  >> parse xs
    (',':xs) -> get >>= (\s -> put (Put:s) ) >> parse xs
    ('.':xs) -> get >>= (\s -> put (Get:s) ) >> parse xs
    ('[':xs) -> let (ys,y) = runState  (parse xs) []
                in get >>= (\s -> put ((Loop  y):s) ) >> parse ys
    (']':xs) ->  return []
    _:xs -> parse xs

run :: [BrainFuck] -> IO ()
run s =   evalStateT ( run' s)  tupElem >> return ()
    where 
        lis :: [Int]
        lis  = replicate 15000 0
        tupElem :: ([Int],[Int])
        tupElem = (,) lis lis

run' :: [BrainFuck] -> StateT ([Int],[Int]) IO [BrainFuck]
run' s =case s of
    (ValInc:xs)  -> get >>= (\((p:ps),q) -> put ((p+1):ps,q)) >> run' xs
    (ValDec:xs)  -> get >>= (\((p:ps),q) -> put ((p-1):ps,q)) >> run' xs
    (PtrInc:xs)  -> get >>= (\(ps,q:qs) -> put $ (inc q ps,qs)  ) >> run' xs 
        where inc [] ps = 0:ps
              inc p  ps = p:ps
    (PtrDec:xs)  -> get >>=  (\ ((p:ps) ,qs) -> put  (ps,dec p qs))  >> run' xs 
        where dec [] ps = 0:ps
              dec p  ps = p:ps
    (Put:xs)     -> get >>= ( \(p:ps,q) -> (liftIO $ putChar $ chr p) ) >> run' xs
    (Get:xs)     -> get >>= ( \(p:ps,q) ->   liftIO getLine    >>= (\ x -> put (x:ps,q) ) )  >> run' xs
        -- where digits  xs = mkIntValue . (foldl (&&) True $ map isNumber xs) 
    all@((Loop bf):xs) -> run' xs >>= get >>= ( \ (p,q) -> case p of
                                                    (0:ps) -> run' xs
                                                    (n:ns) -> run' all )
    [] -> return []

