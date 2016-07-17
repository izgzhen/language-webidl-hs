{-|
  Module:      Language.WebIDL.AST
  Description: Abstract Syntax Tree of WebIDL
-}

module Language.WebIDL.AST where

import Prelude hiding (Enum)

-- | Definition
data Definition a = DefInterface (Interface a)
                  | DefPartial (Partial a)
                  | DefDictionary (Dictionary a)
                  | DefException (Exception a)
                  | DefEnum (Enum a)
                  | DefTypedef (Typedef a)
                  | DefImplementsStatement (ImplementsStatement a)
                  deriving (Show, Eq)

-- | @interface@
data Interface a = Interface a Ident (Maybe Ident) [InterfaceMember a] deriving (Show, Eq)

-- | Partial Definition
data Partial a = PartialInterface a Ident [InterfaceMember a]
               | PartialDictionary a Ident [DictionaryMember a]
               deriving (Show, Eq)

-- | @dictionary@
data Dictionary a = Dictionary a Ident (Maybe Ident) [DictionaryMember a] deriving (Show, Eq)

-- | @exception@
data Exception a = Exception a Ident (Maybe Ident) [ExceptionMember a] deriving (Show, Eq)

-- | @enum@
data Enum a = Enum a Ident [EnumValue] deriving (Show, Eq)

-- | @typedef@
data Typedef a = Typedef a Type Ident deriving (Show, Eq)

-- | @implements@ statement
data ImplementsStatement a = ImplementsStatement a Ident Ident deriving (Show, Eq)

-- | Member of interface definition
data InterfaceMember a = IMemConst (Const a)
                       | IMemAttribute (Attribute a)
                       | IMemOperation (Operation a)
                       deriving (Show, Eq)

-- | Member of dictionary
data DictionaryMember a = DictionaryMember a Type Ident (Maybe Default) deriving (Show, Eq)

-- | Member of exception definition
data ExceptionMember a = ExConst a (Const a)
                       | ExField a Type Ident
                       deriving (Show, Eq)

-- | Attribute member of interface
data Attribute a = Attribute a (Maybe Inherit) (Maybe ReadOnly) Type Ident deriving (Show, Eq)

-- | Operation member of interface
data Operation a = Operation a (Maybe Qualifier) ReturnType (Maybe Ident) [Argument] deriving (Show, Eq)

-- | Argument of operation signature
data Argument = ArgOptional Type ArgumentName Default
              | ArgNonOpt Type (Maybe Ellipsis) ArgumentName
              deriving (Show, Eq)

-- | Value of a @enum@
newtype EnumValue = EnumValue String deriving (Show, Eq)

-- | Name of argument
data ArgumentName = ArgKey ArgumentNameKeyword
                  | ArgIdent Ident
                  deriving (Show, Eq)
-- | @const@
data Const a = Const a ConstType Ident ConstValue deriving (Show, Eq)

-- | @default@ specification
data Default = DefaultValue ConstValue
             | DefaultString String
             deriving (Show, Eq)

-- | Constant value
data ConstValue = ConstBooleanLiteral Bool
                | ConstFloatLiteral Double
                | ConstInteger Integer
                | ConstNull
                deriving (Show, Eq)

-- | Qualifers
data Qualifier = QuaStatic
               | QSpecials [Special]
               deriving (Show, Eq)

-- | Special qualifier
data Special = Getter 
             | Setter 
             | Ccreator 
             | Deleter 
             | Legacycaller
             deriving (Show, Eq)

-- | Argument name keyword
data ArgumentNameKeyword = ArgAttribute    | ArgCallback    | ArgConst        | ArgCreator
                         | ArgDeleter      | ArgDictionary  | ArgEnum         | ArgException
                         | ArgGetter       | ArgImplements  | ArgInherit      | ArgInterface
                         | ArgLegacycaller | ArgPartial     | ArgSetter       | ArgStatic
                         | ArgStringifier  | ArgTypedef     | ArgUnrestricted
                         deriving (Show, Eq)

-- | Types
data Type = TySingleType SingleType | TyUnionType UnionType TypeSuffix deriving (Show, Eq)

-- | Single type
data SingleType = STyNonAny NonAnyType
                | STyAny TypeSuffix
                deriving (Show, Eq)

-- | Types that is not @any@
data NonAnyType = TyPrim PrimitiveType TypeSuffix
                | TyDOMString TypeSuffix
                | TyIdent Ident TypeSuffix
                | TySequence Type (Maybe Null)
                | TyObject TypeSuffix
                | TyDate TypeSuffix
                deriving (Show, Eq)

-- | Primitive type
data PrimitiveType = PrimIntegerType IntegerType
                   | PrimFloatType FloatType
                   | Boolean
                   | Byte
                   | Octet
                   deriving (Show, Eq)

-- | Integer type
data IntegerType = IntegerType (Maybe Unsigned) IntegerWidth deriving (Show, Eq)

-- | Integer width, @short@, @long@ etc.
data IntegerWidth = Short | Long Int deriving (Show, Eq)

-- | @unsigned@ modifier
data Unsigned = Unsigned deriving (Show, Eq)

-- | Suffix of type
data TypeSuffix = TypeSuffixArray
                | TypeSuffixNullable
                | TypeSuffixNone
                deriving (Show, Eq)

-- | Float type
data FloatType = TyFloat (Maybe Unrestricted)
               | TyDouble (Maybe Unrestricted)
               deriving (Show, Eq)

-- | Union of several types
type UnionType = [UnionMemberType]

-- | Union member type
data UnionMemberType = UnionTy UnionType TypeSuffix 
                     | UnionTyNonAny NonAnyType
                     | UnionTyAny TypeSuffix
                     deriving (Show, Eq)

-- | Return value's type
data ReturnType = RetType Type
                | RetVoid
                deriving (Show, Eq)

-- | Constant's type
data ConstType = ConstPrim PrimitiveType (Maybe Null)
               | ConstIdent Ident (Maybe Null)
               deriving (Show, Eq)

-- | @null@ keyword
data Null = Null deriving (Show, Eq)

-- | @ellipsis@ marker
data Ellipsis = Ellipsis deriving (Show, Eq)

-- | @readonly@ marker
data ReadOnly = ReadOnly deriving (Show, Eq)

-- | @inherit@ marker
data Inherit = Inherit deriving (Show, Eq)

-- | @unrestricted@ marker
data Unrestricted = Unrestricted deriving (Show, Eq)

-- | Identifier
newtype Ident = Ident String deriving (Show, Eq, Ord)