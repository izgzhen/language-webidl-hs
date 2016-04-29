module AST where

import Prelude hiding (Enum)

-- Definition
data Definition a = DefInterface (Interface a)
                  | DefPartial (Partial a)
                  | DefDictionary (Dictionary a)
                  | DefException (Exception a)
                  | DefEnum (Enum a)
                  | DefTypedef (Typedef a)
                  | DefImplementsStatement (ImplementsStatement a)
                  deriving (Show)

-- Interface
data Interface a = Interface a Ident (Maybe Ident) [InterfaceMember a] deriving (Show)

-- Partial
data Partial a = PartialInterface a Ident [InterfaceMember a]
               | PartialDictionary a Ident [DictionaryMember a]
               deriving (Show)

-- Dictionary
data Dictionary a = Dictionary a Ident (Maybe Ident) [DictionaryMember a] deriving (Show)

-- Exception
data Exception a = Exception a Ident (Maybe Ident) [ExceptionMember a] deriving (Show)

-- Enum
data Enum a = Enum a Ident [EnumValue] deriving (Show)

-- Typedef
data Typedef a = Typedef a Type Ident deriving (Show)

-- Implements
data ImplementsStatement a = ImplementsStatement a Ident Ident deriving (Show)

-- Fields
data InterfaceMember a = IMemConst (Const a)
                       | IMemAttribute (Attribute a)
                       | IMemOperation (Operation a)
                       deriving (Show)

data DictionaryMember a = DictionaryMember a Type Ident Default deriving (Show)

data ExceptionMember a = ExConst a (Const a)
                       | ExField a Type Ident
                       deriving (Show)

data Attribute a = Attribute a (Maybe Inherit) (Maybe ReadOnly) Type Ident deriving (Show)

data Operation a = Operation a Qualifier ReturnType (Maybe Ident) [Argument] deriving (Show)

data Argument = ArgOptional Type ArgumentName Default
              | ArgNonOpt Type (Maybe Ellipsis) ArgumentName
              deriving (Show)

newtype EnumValue = EnumValue String deriving (Show)

data ArgumentName = ArgKey ArgumentNameKeyword
                  | ArgIdent Ident
                  deriving (Show)

-- Values
data Const a = Const a ConstType Ident ConstValue deriving (Show)

data Default = DefaultValue ConstValue
             | DefaultString String
             deriving (Show)

data ConstValue = ConstBooleanLiteral Bool
                | ConstFloatLiteral Double
                | ConstInteger Integer
                | ConstNull
                deriving (Show)


-- Qualifers
data Qualifier = QuaStatic
               | QSpecials [Special]
               deriving (Show)

data Special = Getter 
             | Setter 
             | Ccreator 
             | Deleter 
             | Legacycaller
             deriving (Show)

data ArgumentNameKeyword = ArgAttribute    | ArgCallback    | ArgConst        | ArgCreator
                         | ArgDeleter      | ArgDictionary  | ArgEnum         | ArgException
                         | ArgGetter       | ArgImplements  | ArgInherit      | ArgInterface
                         | ArgLegacycaller | ArgPartial     | ArgSetter       | ArgStatic
                         | ArgStringifier  | ArgTypedef     | ArgUnrestricted
                         deriving (Show)

-- Types
data Type = TySingleType SingleType | TyUnionType UnionType TypeSuffix deriving (Show)

data SingleType = STyNonAny NonAnyType
                | STyAny TypeSuffix
                deriving (Show)

data NonAnyType = TyPrim PrimitiveType TypeSuffix
                | TyDOMString TypeSuffix
                | TyIdent Ident TypeSuffix
                | TySequence Type (Maybe Null)
                | TyObject TypeSuffix
                | TyDate TypeSuffix
                deriving (Show)


data PrimitiveType = PrimIntegerType IntegerType
                   | PrimFloatType FloatType
                   | Boolean
                   | Byte
                   | Octet
                   deriving (Show)

data IntegerType = IntegerType (Maybe Unsigned) IntegerWidth deriving (Show)

data IntegerWidth = Short | Long Int deriving (Show)

data TypeSuffix = TypeSuffixArray TypeSuffix
                | TypeSuffixListNullable TypeSuffix
                | TypeSuffixNone
                deriving (Show)

data FloatType = TyFloat (Maybe Unrestricted)
               | TyDouble (Maybe Unrestricted)
               deriving (Show)

type UnionType = [UnionMemberType]

data UnionMemberType = UnionTy UnionType TypeSuffix 
                     | UnionTyNonAny NonAnyType
                     | UnionTyAny TypeSuffix
                     deriving (Show)

data ReturnType = RetType Type
                | RetVoid
                deriving (Show)


data ConstType = ConstPrim PrimitiveType (Maybe Null)
               | ConstIdent Ident (Maybe Null)
               deriving (Show)

-- Markers

data Ellipsis = Ellipsis deriving (Show)
data ReadOnly = ReadOnly deriving (Show)
data Inherit = Inherit deriving (Show)
data Unrestricted = Unrestricted deriving (Show)
data Null = Null deriving (Show)
data Unsigned = Unsigned deriving (Show)

-- Ident
newtype Ident = Ident String deriving (Show)
