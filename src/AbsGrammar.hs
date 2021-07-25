module AbsGrammar where

newtype Id = Id String deriving (Eq, Ord, Show, Read)

data Program = PDefs [Def]
  deriving (Eq, Ord, Show, Read)

data Def = DefFun Type Id [Arg] [Stm]
  deriving (Eq, Ord, Show, Read)

data Arg = ArgDecl Type Id
  deriving (Eq, Ord, Show, Read)

data Stm
    = StmExp Exp
    | StmDecls Type [Id]
    | StmInit Type Id Exp
    | StmReturn Exp
    | StmWhile Exp Stm
    | StmBlock [Stm]
    | StmIfElse Exp Stm Stm
  deriving (Eq, Ord, Show, Read)

data Exp
    = ExpBool BoolLiteral
    | ExpInt Integer
    | ExpDouble Double
    | ExpId Id
    | ExpApp Id [Exp]
    | ExpPost Id IncDecOp
    | ExpPre IncDecOp Id
    | ExpMul Exp MulOp Exp
    | ExpAdd Exp AddOp Exp
    | ExpCmp Exp CmpOp Exp
    | ExpAnd Exp Exp
    | ExpOr Exp Exp
    | ExpAssg Id Exp
  deriving (Eq, Ord, Show, Read)

data IncDecOp = OpInc | OpDec
  deriving (Eq, Ord, Show, Read)

data MulOp = OpTimes | OpDiv
  deriving (Eq, Ord, Show, Read)

data AddOp = OpPlus | OpMinus
  deriving (Eq, Ord, Show, Read)

data CmpOp = OpLt | OpGt | OpLtEq | OpGtEq | OpEq | OpNEq
  deriving (Eq, Ord, Show, Read)

data BoolLiteral = LTrue | LFalse
  deriving (Eq, Ord, Show, Read)

data Type = Type_bool | Type_int | Type_double | Type_void
  deriving (Eq, Ord, Show, Read)

