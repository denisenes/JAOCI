module CheckUtils where

import Data.Map (Map)
import qualified Data.Map as Map

import AbsGrammar

type Env = (FunSign,[Context])        -- function signatures and context stack
type FunSign = Map Id ([Type],Type)   -- function signature (Ident ---> [Args_types], Return_type)
type Context = Map Id Type            -- variables with their types

-- look for a variable type in the symbol tables
-- (it starts look at the top of the stack and then goes down to a more global context)
type Baby = Type
lookVar:: Env -> Id -> Maybe Baby 
lookVar (_, stack) id = lookFrame stack id where
        lookFrame:: [Map Id Type] -> Id -> Maybe Baby
        lookFrame [] _ = Nothing
        lookFrame (frame:tail) id = let res = Map.lookup id frame in 
                                   if res /= Nothing then res
                                   else lookFrame tail id

-- look variable only in the top frame
lookVar2:: Env -> Id -> Maybe Type
lookVar2 (_, top:_) id = Map.lookup id top


-- look for a function signature in the environment
lookFun:: Env -> Id -> Maybe ([Type],Type)
lookFun (fnSign, _) id = Map.lookup id fnSign

-- update symbol table on the top of the context stack
updateVar:: Env -> Id -> Type -> Env
updateVar (fnSign, (table:stackTail)) id t = (fnSign, (newTable:stackTail))
        where newTable = Map.insert id t table

-- update list of function signatures
updateFun:: Env -> Id -> ([Type],Type) -> Env
updateFun (fnSign, stack) id signature = (newFnSign, stack)
        where newFnSign = Map.insert id signature fnSign

-- make new empty symbol table on the top of the stack
newBlock:: Env -> Env
newBlock (fnSign, stack) = (fnSign, (Map.empty:stack))


emptyEnv :: Env
emptyEnv = (Map.empty, [Map.empty])