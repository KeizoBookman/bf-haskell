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
run' s = case s of
    (ValInc:xs)  -> get >>= (\ ((p:ps),q) -> put ((p+1):ps,q)) >> run' xs
    (ValDec:xs)  -> get >>= (\ ((p:ps),q) -> put ((p-1):ps,q)) >> run' xs
    (PtrInc:xs)  -> get >>= (\ (ps,q:qs) -> put (q:ps,qs)  ) >> run' xs 
    (PtrDec:xs)  -> get >>=  (\ ((p:ps) ,qs) -> put  (ps,p:qs))  >> run' xs 
    (Put:xs)     -> get >>= ( \ (p:ps,q) -> (liftIO $ putChar $ chr p) ) >> run' xs
    (Get:xs)     -> do
        (p:ps,q) <- get
        t        <- liftIO getLine
        if isNum t
        then do
            put ((read t):ps, q) 
            run' xs
        else run' xs
            where
                isNum :: String -> Bool
                isNum s = let b = foldl1 (&&) $ map isNumber s
                          in b
    all@((Loop bf):xs) -> do
        run' bf 
        (p,q) <-get 
        case p of
          (0:ns) -> run' xs
          (n:ns) -> run' all 
    [] -> return []

