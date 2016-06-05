module Language.WebIDL.AST where

import Prelude hiding (Enum)

-- Definition
data Definition a = DefInterface (Interface a)
                  | DefPartial (Partial a)
                  | DefDictionary (Dictionary a)
                  | DefException (Exception a)
                  | DefEnum (Enum a)
                  | DefTypedef (Typedef a)
                  | DefImplementsStatement (ImplementsStatement a)
                  deriving (Show, Eq)

-- Interface
data Interface a = Interface a Ident (Maybe Ident) [InterfaceMember a] deriving (Show, Eq)

-- Partial
data Partial a = PartialInterface a Ident [InterfaceMember a]
               | PartialDictionary a Ident [DictionaryMember a]
               deriving (Show, Eq)

-- Dictionary
data Dictionary a = Dictionary a Ident (Maybe Ident) [DictionaryMember a] deriving (Show, Eq)

-- Exception
data Exception a = Exception a Ident (Maybe Ident) [ExceptionMember a] deriving (Show, Eq)

-- Enum
data Enum a = Enum a Ident [EnumValue] deriving (Show, Eq)

-- Typedef
data Typedef a = Typedef a Type Ident deriving (Show, Eq)

-- Implements
data ImplementsStatement a = ImplementsStatement a Ident Ident deriving (Show, Eq)

-- Fields
data InterfaceMember a = IMemConst (Const a)
                       | IMemAttribute (Attribute a)
                       | IMemOperation (Operation a)
                       deriving (Show, Eq)

data DictionaryMember a = DictionaryMember a Type Ident (Maybe Default) deriving (Show, Eq)

data ExceptionMember a = ExConst a (Const a)
                       | ExField a Type Ident
                       deriving (Show, Eq)

data Attribute a = Attribute a (Maybe Inherit) (Maybe ReadOnly) Type Ident deriving (Show, Eq)

data Operation a = Operation a (Maybe Qualifier) ReturnType (Maybe Ident) [Argument] deriving (Show, Eq)

data Argument = ArgOptional Type ArgumentName Default
              | ArgNonOpt Type (Maybe Ellipsis) ArgumentName
              deriving (Show, Eq)

newtype EnumValue = EnumValue String deriving (Show, Eq)

data ArgumentName = ArgKey ArgumentNameKeyword
                  | ArgIdent Ident
                  deriving (Show, Eq)

-- Values
data Const a = Const a ConstType Ident ConstValue deriving (Show, Eq)

data Default = DefaultValue ConstValue
             | DefaultString String
             deriving (Show, Eq)

data ConstValue = ConstBooleanLiteral Bool
                | ConstFloatLiteral Double
                | ConstInteger Integer
                | ConstNull
                deriving (Show, Eq)


-- Qualifers
data Qualifier = QuaStatic
               | QSpecials [Special]
               deriving (Show, Eq)

data Special = Getter 
             | Setter 
             | Ccreator 
             | Deleter 
             | Legacycaller
             deriving (Show, Eq)

data ArgumentNameKeyword = ArgAttribute    | ArgCallback    | ArgConst        | ArgCreator
                         | ArgDeleter      | ArgDictionary  | ArgEnum         | ArgException
                         | ArgGetter       | ArgImplements  | ArgInherit      | ArgInterface
                         | ArgLegacycaller | ArgPartial     | ArgSetter       | ArgStatic
                         | ArgStringifier  | ArgTypedef     | ArgUnrestricted
                         deriving (Show, Eq)

-- Types
data Type = TySingleType SingleType | TyUnionType UnionType TypeSuffix deriving (Show, Eq)

data SingleType = STyNonAny NonAnyType
                | STyAny TypeSuffix
                deriving (Show, Eq)

data NonAnyType = TyPrim PrimitiveType TypeSuffix
                | TyDOMString TypeSuffix
                | TyIdent Ident TypeSuffix
                | TySequence Type (Maybe Null)
                | TyObject TypeSuffix
                | TyDate TypeSuffix
                deriving (Show, Eq)


data PrimitiveType = PrimIntegerType IntegerType
                   | PrimFloatType FloatType
                   | Boolean
                   | Byte
                   | Octet
                   deriving (Show, Eq)

data IntegerType = IntegerType (Maybe Unsigned) IntegerWidth deriving (Show, Eq)

data IntegerWidth = Short | Long Int deriving (Show, Eq)

data TypeSuffix = TypeSuffixArray
                | TypeSuffixNullable
                | TypeSuffixNone
                deriving (Show, Eq)

data FloatType = TyFloat (Maybe Unrestricted)
               | TyDouble (Maybe Unrestricted)
               deriving (Show, Eq)

type UnionType = [UnionMemberType]

data UnionMemberType = UnionTy UnionType TypeSuffix 
                     | UnionTyNonAny NonAnyType
                     | UnionTyAny TypeSuffix
                     deriving (Show, Eq)

data ReturnType = RetType Type
                | RetVoid
                deriving (Show, Eq)


data ConstType = ConstPrim PrimitiveType (Maybe Null)
               | ConstIdent Ident (Maybe Null)
               deriving (Show, Eq)

-- Markers

data Ellipsis = Ellipsis deriving (Show, Eq)
data ReadOnly = ReadOnly deriving (Show, Eq)
data Inherit = Inherit deriving (Show, Eq)
data Unrestricted = Unrestricted deriving (Show, Eq)
data Null = Null deriving (Show, Eq)
data Unsigned = Unsigned deriving (Show, Eq)

-- Ident
newtype Ident = Ident String deriving (Show, Eq, Ord)
