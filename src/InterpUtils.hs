module InterpUtils where

import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map

import AbsGrammar

data Value = Int Integer | Double Double | Bool Bool | Void | Undef
    deriving (Show, Ord, Eq)

instance Num Value where
    -- a + b --
   (Int a) + (Int b) = Int $ a + b
   (Double a) + (Int b) = Double $ a + (fromIntegral b) -- for inc and dec
   (Double a) + (Double b) = Double $ a + b
   (Bool a) + (Bool b) = Bool $ a || b
    -- a * b --
   (Int a) * (Int b) = Int $ a * b
   (Double a) * (Double b) = Double $ a * b
   (Bool a) * (Bool b) = Bool $ a && b
    -- a - b --
   (Int a) - (Int b) = Int $ a - b
   (Double a) - (Int b) = Double $ a - (fromIntegral b) -- for inc and dec
   (Double a) - (Double b) = Double $ a - b
   (Bool a) - (Bool b) = Bool $ a && (not b)
   --abs
   --signum
   fromInteger int = Int int



vdiv:: Value -> Value -> Value
vdiv (Int a) (Int b) = Int (a `div` b)
vdiv (Double a) (Double b) = Double (a / b)

vand:: Value -> Value -> Value
vand (Int int1) (Int int2) = Int (int1 .&. int2)
vand (Bool bool1) (Bool bool2) = Bool $ bool1 && bool2

-- HE'LL STEAL YOUR CUCUMBER!!!
vor:: Value -> Value -> Value
vor (Int int1) (Int int2) = Int (int1 .|. int2)
vor (Bool bool1) (Bool bool2) = Bool $ bool1 || bool2

type IEnv = (Map Id Fun, CallStack)
type CallStack = [ScopeStack]
type ScopeStack = [Map Id Value]
type Fun = ([Arg], [Stm])


--Value look (Ident x, Env env)
lookIVar:: IEnv -> Id -> Maybe Value
lookIVar (_, (scopeStack:_)) id = scopeTraverse scopeStack id
    where
    scopeTraverse [] _ = Nothing
    scopeTraverse (h:tail) id = let res = Map.lookup id h in
        case res of
            Nothing -> scopeTraverse tail id
            (Just val) -> Just val

--Fun look (Ident x, Env env)
lookIFun:: IEnv -> Id -> Maybe Fun
lookIFun (fs, _) id = Map.lookup id fs 

--Env extend (Env env, Ident x, Value v)
extendVar:: IEnv -> Id -> Value -> IEnv
extendVar (funcs, ((mapka:scopes):calls)) id val = (funcs, ((new_map:scopes):calls))
    where
    new_map = Map.insert id val mapka


--Env extend (Env env, Definition d)
extendFun:: IEnv -> Def -> IEnv
extendFun (funcs, vars) (DefFun _ id args stms) = (new_funcs, vars)
    where
    new_funcs = Map.insert id (args, stms) funcs

--Env callPush (Env env)
callPush:: IEnv -> [Arg] -> [Value] -> IEnv
callPush (fs, calls) args vals = let mapka = extendArgs args vals in
    (fs, ([mapka]:calls))
    where
    extendArgs:: [Arg] -> [Value] -> Map Id Value
    extendArgs [] [] = Map.empty
    extendArgs ((ArgDecl _ id):tail1) (val:tail2) = Map.insert id val (extendArgs tail1 tail2)

--Env newScope (Env env)
newScope:: IEnv -> IEnv
newScope (fs, (scopes:calls)) = (fs, ((Map.empty:scopes):calls))


--Env empty ()
emptyIEnv:: IEnv
emptyIEnv = (Map.empty, [[Map.empty]])