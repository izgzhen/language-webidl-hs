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

-- Interface
data Interface a = Interface a Ident (Maybe Ident) [InterfaceMember a]

-- Partial
data Partial a = PartialInterface a Ident [InterfaceMember a]
               | PartialDictionary a Ident [DictionaryMember a]

-- Dictionary
data Dictionary a = Dictionary a Ident (Maybe Ident) [DictionaryMember a]

-- Exception
data Exception a = Exception a Ident (Maybe Ident) [ExceptionMember a]

-- Enum
data Enum a = Enum a Ident [EnumValue]

-- Typedef
data Typedef a = Typedef Type Ident

-- Implements
data ImplementsStatement a = ImplementsStatement a Ident Ident

-- Fields
data InterfaceMember a = IMemConst (Const a)
                       | IMemAttribute (Attribute a)
                       | IMemOperation (Operation a)

data DictionaryMember a = DictionaryMember a Type Ident Default

data ExceptionMember a = ExConst a (Const a)
                       | ExField a Type Ident

data Attribute a = Attribute a (Maybe Inherit) (Maybe ReadOnly) Type Ident

data Operation a = Operation a Qualifier ReturnType (Maybe Ident) [Argument]

data Argument = ArgOptional Type ArgumentName Default
              | ArgNonOpt Type (Maybe Ellipsis) ArgumentName

newtype EnumValue = EnumValue String

data ArgumentName = ArgKey ArgumentNameKeyword
                  | ArgIdent Ident

-- Values
data Const a = Const a ConstType Ident ConstValue

data Default = DefaultValue ConstValue
             | DefaultString String

data ConstValue = ConstBooleanLiteral Bool
                | ConstFloatLiteral Float
                | ConstInteger Int
                | ConstNull


-- Qualifers
data Qualifier = QuaStatic
               | QSpecials [Special]

data Special = Getter 
             | Setter 
             | Ccreator 
             | Deleter 
             | Legacycaller

data ArgumentNameKeyword = ArgAttribute    | ArgCallback    | ArgConst        | ArgCreator
                         | ArgDeleter      | ArgDictionary  | ArgEnum         | ArgException
                         | ArgGetter       | ArgImplements  | ArgInherit      | ArgInterface
                         | ArgLegacycaller | ArgPartial     | ArgSetter       | ArgStatic
                         | ArgStringifier  | ArgTypedef     | ArgUnrestricted

-- Types
data Type = TySingleType SingleType | TyUnionType UnionType TypeSuffix

data SingleType = NonAnyType 
                | AnyType TypeSuffix

data NonAnyType = TyPrim PrimitiveType TypeSuffix 
                | TyDOMString TypeSuffix 
                | TyIdent Ident TypeSuffix 
                | TySequence Type (Maybe Null)
                | TyObject TypeSuffix
                | TyDate TypeSuffix


data PrimitiveType = PrimIntegerType IntegerType
                   | PrimFloatType FloatType
                   | Boolean
                   | Byte
                   | Octet

data IntegerType = IntegerType (Maybe Unsigned) IntegerWidth

data IntegerWidth = Short | Long Int

data TypeSuffix = TypeSuffixArray TypeSuffix
                | TypeSuffixListNullable TypeSuffix

data FloatType = TyFloat (Maybe Unrestricted)
               | TyDouble (Maybe Unrestricted)

type UnionType = [UnionMemberType]

data UnionMemberType = UnionTy UnionType TypeSuffix 
                     | UnionTyNonAny NonAnyType
                     | UnionTyAny TypeSuffix

data ReturnType = RetType Type
                | RetVoid


data ConstType = ConstPrim PrimitiveType (Maybe Null)
               | ConstIdent Ident (Maybe Null)

-- Markers

data Ellipsis = Ellipsis
data ReadOnly = ReadOnly
data Inherit = Inherit
data Unrestricted = Unrestricted
data Null = Null
data Unsigned = Unsigned

-- Ident
newtype Ident = Ident String

