module InterpUtils where

import Data.Map (Map)
import qualified Data.Map as Map

import AbsGrammar

data Value = Int Int | Double Double | Bool Bool | Void
    deriving Show

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
callPush:: IEnv -> IEnv
callPush (fs, calls) = (fs, ([Map.empty]:calls))

--Env newScope (Env env)
newScope:: IEnv -> IEnv
newScope (fs, (scopes:calls)) = (fs, ((Map.empty:scopes):calls))


--Env empty ()
emptyIEnv:: IEnv
emptyIEnv = (Map.empty, [[Map.empty]])