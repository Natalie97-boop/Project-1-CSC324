{-|
Module: P1
Description: Project 1: A Spreadsheet Application with DeerLang
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
import qualified Data.Map (empty, insert, lookup, union)
import           P1Types  (Column (..), Definition (..), Env, Expr (..),
                           Spreadsheet (..), Value (..), exampleSpreadsheet)
import           Prelude  hiding (lookup)

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
evalDeer (Apply fnexpr argexpressions) env = case (evalDeer fnexpr env)
  of (VClosure param fnbody fnenv) ->
      let argVals = map (\x -> (evalDeer x env)) argexpressions
          extendedEnv = Data.Map.union (mapParamArgValueHelper param argVals Data.Map.empty) (Data.Map.union fnenv env) 
      in evalDeer fnbody extendedEnv
     _ -> Error


computeSpreadsheet :: Spreadsheet -> [Column]
computeSpreadsheet (Spreadsheet defs columns) =
    let defEnv = definitionEnvHelper defs
        valueCols = filter (isValueColumn) columns
        dataEnvs = buildDataEnvs valueCols defEnv []
        computedCols = mapComputedColumnsHelper (filter (not.isValueColumn) columns) dataEnvs []
    in reorderColumns columns valueCols computedCols []


-------------------------------------------------------------------------------
-- Helper Functions
-- | You may add, remove, or modify any helper functions here.
-------------------------------------------------------------------------------

-- Strings both the evaluated ValCols and ComputedCols back to the original order, back
-- when they were in the original list (originalOrder). This helper takes the original column list,
-- both the evaluated column lists, and an accumulator variable and returns the combined list in the 
-- correct order

reorderColumns :: [Column] -> [Column] -> [Column] -> [Column] -> [Column]
reorderColumns originalOrder valueColumns computedColumns acc =
  case (originalOrder, valueColumns, computedColumns) of
    ([],[],[]) -> acc
    ((x:xs),(y:ys),(z:zs)) ->
      case x of (ValCol string values) -> reorderColumns xs ys computedColumns (rightcons acc y)
                (ComputedCol string expr) -> reorderColumns xs valueColumns zs (rightcons acc z)
    ((x:xs),[],(z:zs)) -> reorderColumns xs valueColumns zs (rightcons acc z)
    ((x:xs),(y:ys),[]) -> reorderColumns xs ys computedColumns (rightcons acc y)

-- Checks whether the given column is a ValCol, returns a Bool

isValueColumn :: Column -> Bool
isValueColumn (ValCol string values) = True
isValueColumn column = False 
 
{- Helper functions that implement the ComputedCol evaluation -}

-- Maps the each ComputedCol in the first variable to each corresponding environment to produce a list of
-- values, then proceeds to update the environment with the newly evaluated Values corresponding to each 
-- ComputedCol. This is done so that if another ComputedCol is dependent on the value of the prior, 
-- it can do an environment lookup to evaluate the expression.

mapComputedColumnsHelper :: [Column] -> [Env] -> [Column] -> [Column]
mapComputedColumnsHelper computedCol listEnv acc = case computedCol of
  [] -> acc
  (first:rest) -> case first of
    (ComputedCol string expr) ->
      let computedvalues = evaluateComputedColHelper first listEnv
          newEnv = map (\x -> computedColValueToEnv x string) computedvalues
      in mapComputedColumnsHelper rest (compileListEnv listEnv newEnv []) (rightcons acc (ValCol string computedvalues))

-- Maps a String to a Value to create a new Env.

computedColValueToEnv :: Value -> String -> Env
computedColValueToEnv value string = Data.Map.insert string value Data.Map.empty

-- Evaluates a ComputedCol to a list of Values, corresponding to the list of Env given.

evaluateComputedColHelper :: Column -> [Env] -> [Value]
evaluateComputedColHelper computedCol listEnv = case computedCol of (ComputedCol binding expr) ->  map (\x -> evalDeer expr x) listEnv
                                                                    _ -> [Error]

------------------------------------------------------------------------------------------------------------------------------

{- Helper functions that implement the builtins -}

-- Invoked by evalDeer to pattern-match the correct operation to the corresponding helper functions that apply
-- the operation with the proper operand types and return type

builtinHelper :: String -> [Expr] -> Env -> Value
builtinHelper operation arguments env =
  case (operation, arguments) of 
    ("+", [e1,e2]) -> builtinApplyArithmetic e1 e2 env (+)
    ("-", [e1,e2]) -> builtinApplyArithmetic e1 e2 env (-)
    ("*", [e1,e2]) -> builtinApplyArithmetic e1 e2 env (*)
    ("/", [e1,e2]) -> builtinApplyDivision e1 e2 env
    (">", [e1,e2]) -> builtinApplyComparator e1 e2 env (>)
    ("=", [e1,e2]) -> builtinApplyComparator e1 e2 env (==)
    (">=", [e1,e2]) -> builtinApplyComparator e1 e2 env (>=)
    ("++", [e1,e2]) -> builtinApplyStrings e1 e2 env (++)
    ("!", [Literal (VBool bool)]) -> VBool(not bool) 
    _ -> Error

-- Implements the division by zero error, and regular divison

builtinApplyDivision :: Expr -> Expr -> Env  -> Value
builtinApplyDivision e1 e2 env =
    case ((evalDeer e1 env), (evalDeer e2 env)) of
        ((_), (VNum 0))      -> Error
        ((VNum a), (VNum b)) -> VNum (a / b)
        _                    -> Error

--  Implements all other builtin operations that operate on VNum, other than division

builtinApplyArithmetic :: Expr -> Expr -> Env -> (Float -> Float -> Float) -> Value
builtinApplyArithmetic e1 e2 env op =
    case ((evalDeer e1 env), (evalDeer e2 env)) of
        ((VNum a), (VNum b)) -> VNum (op a b)
        _                    -> Error

-- Implements the logical comparators that work on VNum

builtinApplyComparator :: Expr -> Expr -> Env -> (Float -> Float -> Bool) -> Value
builtinApplyComparator e1 e2 env op =
    case ((evalDeer e1 env), (evalDeer e2 env)) of
        ((VNum a), (VNum b)) -> VBool (op a b)
        _                    -> Error

-- Evaluates each expression and adds the strings

builtinApplyStrings :: Expr -> Expr -> Env -> (String -> String -> String) -> Value
builtinApplyStrings e1 e2 env op =
    case ((evalDeer e1 env), (evalDeer e2 env)) of
        ((VStr a), (VStr b)) -> VStr (op a b)
        _                    -> Error

------------------------------------------------------------------------------------------------------------------------------

{- Helper functions that operate on the Environment -}

-- Returns the appropriate environment binding for the defintion given

getEnvironment:: Definition -> Env
getEnvironment def =
  let new_map = Data.Map.empty
  in case def of (Def string expr) -> (Data.Map.insert string (evalDeer expr Data.Map.empty) new_map)

-- For a list of definitions, creates the union of all definitions, with shadowing for definitions

definitionEnvHelper :: [Definition] -> Env
definitionEnvHelper listDef = let environments = reverseList $ map (\x -> getEnvironment x) listDef 
                                  newMap = Data.Map.empty
                              in foldl (\x -> Data.Map.union x) newMap environments

-- Returns a list of environments corresponding to only the value columns in the list, it is important
-- to note that the list of environment retains the original order that the value columns were in

buildDataEnvs :: [Column] -> Env -> [Env]-> [Env]
buildDataEnvs columns env acc = case (columns) of
  [] -> addInitialEnvironment env acc
  (firstColumn:rest) -> case firstColumn of
    (ValCol id values) ->  let localListEnv = map (\x -> getEnvironment (Def id (Literal $ x))) values
                           in buildDataEnvs rest env (compileListEnv localListEnv acc [])
    (ComputedCol string exp) -> buildDataEnvs rest env acc

-- Returns a list of environments with another env unioned to every member environment of the given list

addInitialEnvironment :: Env -> [Env] -> [Env]
addInitialEnvironment env listEnv = map (\x -> Data.Map.union x env) listEnv

--Combines every member environment of both lists together. 

compileListEnv :: [Env] -> [Env] -> [Env] -> [Env]
compileListEnv intialLists listsToAdd acc = case (intialLists, listsToAdd) of
  ([],[])         -> acc
  (_,[])          -> intialLists
  ((x:xs),(y:ys)) -> (compileListEnv xs ys (rightcons acc (Data.Map.union x y)))

-- Maps each argument (Value) to a corresponding parameter in a lambda. This makes it possible to later
-- evaluate the lambda expression

mapParamArgValueHelper :: [String] -> [Value] -> Env -> Env
mapParamArgValueHelper strings values env =
  case (strings, values) of ([],[]) -> env
                            ((x:xs),(y:ys)) -> Data.Map.union (Data.Map.insert x y env) (mapParamArgValueHelper xs ys env)

-- Appends a new member to a list, or "it cons to the right"
rightcons :: [a] -> a -> [a]
rightcons xs x = xs ++ [x]

-- Reverses a list, needed for definition shadowing
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

result = computeSpreadsheet exampleSpreadsheet
