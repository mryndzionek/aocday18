{-# LANGUAGE TemplateHaskell #-}

module Main where
import Data.Void
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.State.Plus
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map

import System.Environment

import Text.Megaparsec hiding (State, parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lx
import Control.Lens hiding (Const)

type Reg = Char
type Val = Integer
data Var = Reg Reg | Const Val deriving (Show)

data Cmd = Snd Reg |
           Set Reg Var |
           Add Reg Var |
           Mul Reg Var |
           Mod Reg Var |
           Rcv Reg |
           Jgz Var Var deriving (Show)

type Parser = Parsec Void String

type Env = Map.Map Reg Val
type Program = [Cmd]

data St = St {
  _env  :: Env,
  _pc   :: Int,
  _inp  :: Seq.Seq Integer,
  _out  :: Seq.Seq Integer,
  _sc   :: Integer
} deriving (Show)

makeLenses ''St

-- Program parser -------------------------------------
lexeme :: Parser a -> Parser a
lexeme = Lx.lexeme space

symbol :: String -> Parser String
symbol = Lx.symbol space1

letter :: Parser Char
letter = lexeme letterChar

integer :: Parser Integer
integer = lexeme Lx.decimal

signedInteger :: Parser Integer
signedInteger = Lx.signed space integer

var :: Parser Var
var = reg <|> cnst
  where
    reg = letter >>= \r -> return $ Reg r
    cnst = signedInteger >>= \c -> return $ Const c

cmd :: Parser Cmd
cmd = singleArg <|> twoArgs <|> jgZ
  where
    singleArg = foldl1 (<|>) $ map si [("rcv", Rcv), ("snd", Snd)]
    si :: (String, Reg -> Cmd) -> Parser Cmd
    si (s, t) = do
      _ <- symbol s
      r <- letter
      return $ t r
    twoArgs = foldl1 (<|>) $ map ta [("set", Set), ("add", Add), ("mul", Mul), ("mod", Mod)]
    ta :: (String, Reg -> Var -> Cmd) -> Parser Cmd
    ta (s, t) = do
      _ <- symbol s
      r <- letter
      v <- var
      return $ t r v
    jgZ = do
      _ <- symbol "jgz"
      v' <- var
      v  <- var
      return $ Jgz v' v 

parse :: String -> Either (ParseError Char Void) [Cmd]
parse input = mapM (runParser cmd "") (lines input)
-------------------------------------------------------

safeLu :: Int -> [a] -> Maybe a
safeLu i a
  | (i >= 0) && (length a > i) = Just (a !! i)
  | otherwise = Nothing

eval_ :: Var -> StatePlusT St Identity Val
eval_ v = case v of
  Const c -> return c
  Reg   r -> (fromMaybe 0 . Map.lookup r . view env) <$> get

set_ :: Reg -> Var -> StatePlusT St Identity ()
set_ r v = eval_ v >>= modify . over env . Map.insert r

process :: Cmd -> StatePlusT St Identity ()
process c = case c of
  Set r v  -> set_ r v
  Add r v  -> op_ r v (+)
  Mul r v  -> op_ r v (*)
  Mod r v  -> op_ r v (flip rem)
  Jgz v' v -> eval_ v' >>= (\a -> when (a > 0) $ jmp v)
  Snd r    -> eval_ (Reg r) >>= (\a -> modify $ (out %~ (a Seq.<|)) . (sc %~ (+1)))
  Rcv r    -> do
    s <- get
    if Seq.null (view inp s) then mzero
    else
      let (inp_ Seq.:|> v) = s ^. inp
      in set_ r (Const v) >> modify (inp .~ inp_)
  where
    op_ r v f = eval_ v >>= (\a -> modify $ over env (adj (f a) r))
    jmp v = eval_ v >>= (\a -> modify $ over pc ((fromIntegral a - 1) +))
    adj f' k m = case Map.lookup k m of
      Nothing -> Map.insert k (f' 0) m
      Just _  -> Map.adjust f' k m

interpret :: Program -> StatePlusT St Identity ()
interpret p = intpr
  where
    intpr = do
      s <- get
      case safeLu (_pc s) p of
        Nothing  -> mzero
        Just c -> do
          process c
          modify (pc %~ (+ 1)) >> intpr

runPrgm :: Program -> St -> Seq.Seq Integer -> Seq.Seq Integer
runPrgm p s i = let Identity s' = runStatePlusT (interpret p) (set inp i s)
                in snd s' ^. out

runPrgms :: Program -> St -> St -> (St, Seq.Seq Integer, St, Seq.Seq Integer)
runPrgms p s1 s2 = until (\(_, ia, _, ib) -> Seq.null ia && Seq.null ib) run (s1', i1', s2', i2')
  where
    (s1', i1', s2', i2') = run (s1, Seq.empty, s2, Seq.empty)
    run (sa, ia, sb, ib) = let Identity sa' = runStatePlusT (interpret p) (sa & inp .~ ia & out .~ Seq.empty)
                               Identity sb' = runStatePlusT (interpret p) (sb & inp .~ ib & out .~ Seq.empty)
                           in (snd sa', sb' ^. (_2 . out), snd sb', sa' ^. (_2 . out))
      
main :: IO ()
main = do
  args <- getArgs
  i <- readFile $ head args
  let prgm = parse i
  case prgm of
    Right p -> do
      let op1 = runPrgm p initS Seq.empty
      let (v Seq.:<| _) = op1
      putStrLn $ "Part 1: " ++ show v
      let op2 = runPrgms p (initS & env .~ Map.singleton 'p' 0)
                           (initS & env .~ Map.singleton 'p' 1)
      putStrLn $ "Part 2: " ++ show (op2 ^. _3 ^. sc)
    Left e -> putStrLn $ "Error: " ++ show e
    where
      initS = St Map.empty 0 Seq.empty Seq.empty 0
