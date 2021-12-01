module Eval(runEval) where

import Syntax
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Writer

data Value 
    = VInt Int
    | VBool Bool 
    | VClosure String Expr Eval.Scope

instance Show Value where
    show (VInt x) = show x
    show (VBool b) = show b
    show VClosure {} = "<<closure>>"

data EvalState = EvalState { depth :: Int } deriving (Show)

inc :: Eval a -> Eval a
inc m = do
    modify $ \s -> s { depth = depth s + 1 }
    out <- m
    modify $ \s -> s { depth = depth s -1 }
    return out

red :: Expr -> Eval ()
red x = do
    d <- gets depth
    tell [(d, x)]
    return ()

type Step = (Int, Expr)

-- newtype WriterT w m a :: * -> (* -> *) -> * -> *
-- Here w =[Step] represents the type of the output to accumulate (which has to be a monoid, which allow us to accumulate this value),
-- m=State EvalState is the inner monad,
-- and a the type of the computation.
type Eval a = WriterT [Step] (State EvalState) a

type Scope = Map.Map String Value


eval :: Scope -> Expr -> Eval Value
eval env expr = case expr of

    ELit (LInt x) -> do
        return $ VInt (fromIntegral x)

    ELit (LBool x) -> do
        return $ VBool x

    EVar x -> do
        red expr
        return $ env Map.! x

    ELam x body -> inc $ do
        return (VClosure x body env)

    EApp a b -> inc $ do
        x <- eval env a
        red a
        y <- eval env b
        red b
        apply x y

extend :: Scope -> String -> Value -> Scope
extend env v t = Map.insert v t env

apply :: Value -> Value -> Eval Value
apply (VClosure n e clo) ex = do
  eval (extend clo n ex) e
apply _ _  = error "Tried to apply non-closure"

emptyScope :: Scope
emptyScope = Map.empty

runEval :: Expr -> (Value, [Step])
runEval x = evalState (runWriterT (eval emptyScope x)) (EvalState 0)
