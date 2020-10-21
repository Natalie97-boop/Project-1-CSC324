{-|
Module: P1 
Description: Project 1: A Spreadsheet Application with DeerLang
Copyright: (c) University of Toronto Mississagua, 2020
               CSC324 Principles of Programming Languages, Fall 2020
-}
-- This lists what this module exports. Don't change this!
module P1
  (
    evalDeer,
    computeSpreadsheet
  )
where

-- You *may not* add imports from Data.Map 
import P1Types(Spreadsheet(..), Definition(..), Column(..),
               Expr(..), Value(..),
               Env, exampleSpreadsheet)
import Prelude hiding (lookup)
import qualified Data.Map (lookup, insert, empty, union)

-------------------------------------------------------------------------------
-- Main Functions: 
-- | These are the functions that we will be directly testing.
-- | Do not change the type signatures of these functions.
-------------------------------------------------------------------------------

evalDeer :: Expr -> Env -> Value
evalDeer (Literal value) env = value
evalDeer (Id id) env = case (Data.Map.lookup id env) of
                           Just value -> value
                           Nothing    -> Error
evalDeer (Builtin string listExpr) env = builtinHelper string listExpr env
evalDeer (Lambda params body) env = (VClosure params body env)
evalDeer (Apply fnexpr argexpressions) env = case (evalDeer fnexpr env) of (VClosure param fnbody fnenv) -> 
                                                                              let argVals = map (\x -> (evalDeer x env)) argexpressions
                                                                                  extendedEnv = Data.Map.union (mapParamArgValueHelper param argVals Data.Map.empty) fnenv
                                                                              in evalDeer fnbody extendedEnv
                                                                    




  
   -- work on this later, figure out how to apply statements together with nested params
                                                           

-- evalDeer (Literal v) env = Error
-- ...

computeSpreadsheet :: Spreadsheet -> [Column]
computeSpreadsheet (Spreadsheet defs columns) = undefined
--  let defEnv    = ...build an environment with the definitions...
--      valueCols = ...just the value columns...
--      dataEnvs  = ...list of environment, one for each spreadsheet row...
--      ...
--  in ...


-------------------------------------------------------------------------------
-- Helper Functions
-- | You may add, remove, or modify any helper functions here.
-------------------------------------------------------------------------------

mapParamArgValueHelper :: [String] -> [Value] -> Env -> Env
mapParamArgValueHelper strings values env = 
  case (strings, values) of ([],[]) -> env
                            ((x:xs),(y:ys)) -> Data.Map.union (Data.Map.insert x y env) (mapParamArgValueHelper xs ys env)  



builtinHelper :: String -> [Expr] -> Env -> Value
builtinHelper operation arguments env = 
  case (operation, arguments) of ("+", [e1,e2]) -> builtinApplyArithmetic e1 e2 env (+)
                                 ("-", [e1,e2]) -> builtinApplyArithmetic e1 e2 env (-)
                                 ("*", [e1,e2]) -> builtinApplyArithmetic e1 e2 env (*)
                                 ("/", [e1,e2]) -> builtinApplyArithmetic e1 e2 env (/)
                                 (">", [e1,e2]) -> builtinApplyComparator e1 e2 env (>)
                                 ("=", [e1,e2]) -> builtinApplyComparator e1 e2 env (==)
                                 (">=", [e1,e2]) -> builtinApplyComparator e1 e2 env (>=)
                                 ("++", [e1,e2]) -> builtinApplyStrings e1 e2 env (++)
                                 ("!", [Literal (VBool bool)]) -> VBool(not bool) -- Remember to test this later
                                 _ -> Error

builtinApplyArithmetic :: Expr -> Expr -> Env -> (Float -> Float -> Float) -> Value
builtinApplyArithmetic e1 e2 env op =
    case ((evalDeer e1 env), (evalDeer e2 env)) of
        ((VNum a), (VNum b)) -> VNum (op a b)
        _                        -> Error

builtinApplyComparator :: Expr -> Expr -> Env -> (Float -> Float -> Bool) -> Value
builtinApplyComparator e1 e2 env op =
    case ((evalDeer e1 env), (evalDeer e2 env)) of
        ((VNum a), (VNum b)) -> VBool (op a b)
        _                        -> Error

builtinApplyStrings :: Expr -> Expr -> Env -> (String -> String -> String) -> Value
builtinApplyStrings e1 e2 env op =
    case ((evalDeer e1 env), (evalDeer e2 env)) of
        ((VStr a), (VStr b)) -> VStr (op a b)
        _                        -> Error

-- Return an environment with the appropriate identifier-to-value bindings.
getEnvironment:: Definition -> Env
getEnvironment def = 
  let new_map = Data.Map.empty
  in case def of (Def string expr) -> (Data.Map.insert string (evalDeer expr Data.Map.empty) new_map) 
  --Might be wrong? Will have to check this later...

  
-- Return a list of environments, one corresponding to each row in the data.
-- Each environment consists of bindings from the value columns, along with
-- the environment.

buildDataEnvs :: [Column] -> Env -> [Env]
buildDataEnvs columns env = case (columns) of [] -> undefined


-------------------------------------------------------------------------------
-- The example from the handout
-------------------------------------------------------------------------------

result = computeSpreadsheet exampleSpreadsheet
