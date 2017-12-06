module NStack.Comms.TypeRepresentation where

import qualified Data.Map as Map
import qualified Data.Text as T
import GHC.Generics
import Data.Serialize (Serialize(..))
import Data.Serialize.Text ()

-- | Simplified version of `ClosedTypeExpr` for sending to the clients
data TypeRepresentation
  = TextType
  | IntegerType
  | DoubleType
  | BoolType
  | JsonType
  | ByteArrayType
  | VoidType
  | MapType (Map.Map T.Text TypeRepresentation)
  | TupleType [TypeRepresentation]
  | ListType TypeRepresentation
  | VectType Integer TypeRepresentation
  | MatrixType Integer Integer TypeRepresentation
  | OptionalType TypeRepresentation
  | SumType (Map.Map T.Text TypeRepresentation)
  deriving (Show, Eq, Generic)

-- | Type representation of a monomorphic nstack method
data MTypeRepresentation = MTypeRepresentation {
  _inType :: TypeRepresentation,
  _outType :: TypeRepresentation
  } deriving (Show, Eq, Generic)

instance Serialize TypeRepresentation
instance Serialize MTypeRepresentation

isSink :: MTypeRepresentation -> Bool
isSink (MTypeRepresentation _ VoidType) = True
isSink _ = False

isSource :: MTypeRepresentation -> Bool
isSource (MTypeRepresentation VoidType _) = True
isSource _ = False
