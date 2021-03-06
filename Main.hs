module Main where
import System.Environment (getArgs)
import System.IO (readFile)
import Data.Char (chr, ord)
import Control.Applicative (many, (<$), (<|>), (<*>), (<*), (*>), (<$>) )
import Control.Monad (foldM, void)
import Text.ParserCombinators.ReadP hiding (many)

data BrainFuck = PtrInc | PtrDec | ValInc | ValDec | Put | Get | Loop [BrainFuck]
    deriving (Show)

main :: IO ()
main = do
    args <- getArgs 
    case args of
        []  -> return ()
        [path] -> do
          src <- readFile path
          run $ parse $ filter (`elem` "+-><.,[]") src
          putChar '\n'

parse :: String -> [BrainFuck]
parse = fst . last . readP_to_S (many bf) where
    bf = ValInc <$ char '+'
     <|> ValDec <$ char '-'
     <|> PtrInc <$ char '>' 
     <|> PtrDec <$ char '<' 
     <|> Put <$ char '.'
     <|> Get <$ char ','
     <|> char '[' *> (Loop <$> many bf) <* char ']'
     <|> pfail

type Tape = ([Int], Int, [Int])

next :: Tape -> Tape
next (x:xs, y, ys) = (xs, x, y:ys)

prev :: Tape -> Tape
prev (xs, x, y:ys) = (x:xs, y, ys)

adjust :: (Int -> Int) -> Tape -> Tape
adjust f (xs, x, ys) = let y = f x in seq y (xs, y, ys)

run :: [BrainFuck] -> IO ()
run = void . foldM run' (repeat 0, 0, repeat 0) where
    run' :: Tape -> BrainFuck -> IO Tape
    run' tape@(_, x, _) inst = case inst of
        ValInc -> return $ adjust succ tape
        ValDec -> return $ adjust pred tape
        PtrInc -> return $ next tape
        PtrDec -> return $ prev tape
        Put    -> tape <$ putChar (chr $ (`mod` 128) $ abs x)
        Get    -> getChar >>= \ c -> return $ adjust (const $ ord c) tape
        loop@(Loop insts)
            | x == 0 -> return tape
            | otherwise -> foldM run' tape insts >>= flip run' loop
