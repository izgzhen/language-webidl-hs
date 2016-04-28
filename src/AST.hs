module AST where

type Definitions a = [(ExtendedAttributeList a, Definition a)]

data Definition a = DefInterface (Interface a)
                  -- | DefPartial (Partial a)
                  -- | DefDictionary (Dictionary a)
                  -- | DefException (Exception a)
                  -- | DefEnum (Enum a)
                  -- | DefTypedef (Typedef a)
                  -- | DefImplementsStatement (ImplementsStatement a)

data ExtendedAttributeList a = ExtendedAttributeList -- Unimplemented yet

data Interface a = Interface a Ident (Maybe Ident) [InterfaceMember a]

data InterfaceMember a = IMemConst (Const a)
                       | IMemAttribute (Attribute a)
                       | IMemOperation (Operation a)

data Const a = Const a ConstType Ident ConstValue

data Attribute a = Attribute a (Maybe Inherit) (Maybe ReadOnly) Type Ident


data Operation a = Operation a Qualifier ReturnType (Maybe Ident) [Argument]


data Qualifier = QuaStatic
               | QSpecials [Special]

data Special = Getter 
             | Setter 
             | Ccreator 
             | Deleter 
             | Legacycaller

data Argument = ArgOptional Type ArgumentName Default
              | ArgNonOpt Type (Maybe Ellipsis) ArgumentName



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

data Unsigned = Unsigned

data TypeSuffix = TypeSuffixArray TypeSuffix
                | TypeSuffixListNullable TypeSuffix

data Null = Null

newtype Ident = Ident String


data FloatType = TyFloat (Maybe Unrestricted)
               | TyDouble (Maybe Unrestricted)

data Unrestricted = Unrestricted


type UnionType = [UnionMemberType]

data UnionMemberType = UnionTy UnionType TypeSuffix 
                     | UnionTyNonAny NonAnyType
                     | UnionTyAny TypeSuffix


data ArgumentName = ArgKey ArgumentNameKeyword
                  | ArgIdent Ident


data ArgumentNameKeyword = ArgAttribute | ArgCallback  | ArgConst  | ArgCreator
                         | ArgDeleter  | ArgDictionary  | ArgEnum  | ArgException
                         | ArgGetter  | ArgImplements  | ArgInherit  | ArgInterface
                         | ArgLegacycaller  | ArgPartial  | ArgSetter  | ArgStatic
                         | ArgStringifier  | ArgTypedef  | ArgUnrestricted

data Ellipsis = Ellipsis


            
data Default = DefaultValue ConstValue
             | DefaultString String

data ConstValue = ConstBooleanLiteral Bool
                | ConstFloatLiteral Float
                | ConstInteger Int
                | ConstNull

data ReturnType = RetType Type
                | RetVoid

data ReadOnly = ReadOnly
data Inherit = Inherit


data ConstType = ConstPrim PrimitiveType (Maybe Null)
               | ConstIdent Ident (Maybe Null)

