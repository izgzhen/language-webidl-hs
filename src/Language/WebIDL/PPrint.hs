module Language.WebIDL.PPrint where

import Language.WebIDL.AST
import Prelude hiding (Enum)
import Text.PrettyPrint.Leijen

printDef :: Definition a -> String
printDef = show . pretty

instance Pretty (Definition a) where
    pretty (DefInterface x) = pretty x
    pretty (DefPartial x) = pretty x
    pretty (DefDictionary x) = pretty x
    pretty (DefException x) = pretty x
    pretty (DefEnum x) = pretty x
    pretty (DefTypedef x) = pretty x
    pretty (DefImplementsStatement x) = pretty x

instance Pretty (Interface a) where
    pretty (Interface _ x mInherit members) =
        text "interface" <+> pretty x <+> prettyInherit mInherit <+> scope members <> semi

prettyMaybe Nothing  _ = empty
prettyMaybe (Just x) f = f x

prettyInherit x = prettyMaybe x (\e -> colon <+> pretty e)

instance Pretty (Exception a) where
    pretty (Exception _ x mInherit members) =
        text "exception" <+> pretty x <+> prettyInherit mInherit <+> scope members <> semi

instance Pretty (Partial a) where
    pretty (PartialInterface _ x members) =
        text "interface" <+> pretty x <+> scope members <> semi
    pretty (PartialDictionary _ x members) =
        text "dictionary" <+> pretty x <+> braces (vsep $ map pretty members) <> semi

instance Pretty (Enum a) where
    pretty (Enum _ x enums) = text "enum" <+> pretty x <+> scope enums <> semi

instance Pretty (Dictionary a) where
    pretty (Dictionary _ x mInherit members) =
        text "dictionary" <+> pretty x <+> prettyInherit mInherit
                          <+> scope members <> semi

scope members = braces (line <> vsep (map (indent 4 . pretty) members) <> line)

instance Pretty (Typedef a) where
    pretty (Typedef _ ty ident) = text "typedef" <+> pretty ty <+> pretty ident <> semi

instance Pretty (ImplementsStatement a) where
    pretty (ImplementsStatement _ i1 i2) = pretty i1 <+> text "implements" <+> pretty i2 <> semi

instance Pretty Ident where
    pretty (Ident x) = text x

instance Pretty Type where
    pretty (TySingleType s) = pretty s
    pretty (TyUnionType ut suffix) = pretty ut <> pretty suffix

instance Pretty SingleType where
    pretty (STyNonAny t) = pretty t
    pretty (STyAny suffix) = text "any" <> pretty suffix

instance Pretty NonAnyType where
    pretty (TyPrim t suffix) = pretty t <> pretty suffix
    pretty (TyDOMString suffix) = text "DOMString" <> pretty suffix
    pretty (TyIdent ident suffix) = pretty ident <> pretty suffix
    pretty (TySequence t mNull) = text "sequence" <> angles (pretty t) <> prettyMaybe mNull pretty
    pretty (TyObject suffix) = text "object" <> pretty suffix
    pretty (TyDate suffix) = text "Date" <> pretty suffix


instance Pretty (DictionaryMember a) where
    pretty (DictionaryMember _ t ident mDefault) =
        pretty t <+> pretty ident <> prettyMaybe mDefault prettyDefault <> semi

instance Pretty Default where
    pretty (DefaultValue cval) = pretty cval
    pretty (DefaultString s)   = text (show s)

instance Pretty ConstValue where
    pretty (ConstBooleanLiteral b) = if b then text "true" else text "false"
    pretty (ConstFloatLiteral d)   = pretty d
    pretty (ConstInteger i)        = pretty i
    pretty ConstNull               = text "null"

instance Pretty Null where
    pretty Null = text "?"

instance Pretty TypeSuffix where
    pretty TypeSuffixArray = text "[]"
    pretty TypeSuffixNullable = text "?"
    pretty TypeSuffixNone = empty

instance Pretty PrimitiveType where
    pretty (PrimIntegerType t) = pretty t
    pretty (PrimFloatType t)   = pretty t
    pretty Boolean             = text "boolean"
    pretty Byte                = text "byte"
    pretty Octet               = text "octet"

instance Pretty FloatType where
    pretty (TyFloat mUnres)  = prettyMaybe mUnres (\e -> pretty e <> space) <> text "float"
    pretty (TyDouble mUnres) = prettyMaybe mUnres (\e -> pretty e <> space) <> text "double"

instance Pretty Unrestricted where
    pretty Unrestricted = text "unrestricted"

instance Pretty IntegerType where
    pretty (IntegerType mUns width) = prettyMaybe mUns pretty <> pretty width

instance Pretty IntegerWidth where
    pretty Short = text "short"
    pretty (Long i) = hsep $ take i (repeat (text "long"))

instance Pretty Unsigned where
    pretty Unsigned = text "unsigned"

instance Pretty UnionMemberType where
    pretty (UnionTy ut suffix) = pretty ut <> pretty suffix
    pretty (UnionTyNonAny t) = pretty t
    pretty (UnionTyAny suffix) = text "any" <> pretty suffix

instance Pretty EnumValue where
    pretty (EnumValue s) = text s

instance Pretty (InterfaceMember a) where
    pretty (IMemConst c) = pretty c
    pretty (IMemAttribute attr) = pretty attr
    pretty (IMemOperation op) = pretty op

instance Pretty (Operation a) where
    pretty (Operation _ mQ retty mIdent args) =
        pretty mQ <> pretty retty <+> prettyMaybe mIdent (\e -> pretty e)
                  <> parens (hcat (punctuate (comma <> space) (map pretty args))) <> semi


instance Pretty Argument where
    pretty (ArgOptional t name def) = text "optional" <+> pretty t <+> pretty name <> prettyDefault def
    pretty (ArgNonOpt t mElli name) = pretty t <> prettyMaybe mElli (\_ -> text "...") <+> pretty name

prettyDefault def = space <> equals <+> pretty def

instance Pretty ArgumentName where
    pretty (ArgKey key) = pretty key
    pretty (ArgIdent i) = pretty i

instance Pretty ArgumentNameKeyword where
    pretty ArgAttribute = text "attribute"
    pretty ArgCallback = text "callback"
    pretty ArgConst = text "const"
    pretty ArgCreator = text "creator"
    pretty ArgDeleter = text "deleter"
    pretty ArgDictionary = text "dictionary"
    pretty ArgEnum = text "enum"
    pretty ArgException   = text "exception"
    pretty ArgGetter = text "getter"
    pretty ArgImplements = text "implements"
    pretty ArgInherit = text "inherit"
    pretty ArgInterface   = text "interface"
    pretty ArgLegacycaller = text "legacycaller"
    pretty ArgPartial = text "partial"
    pretty ArgSetter = text "setter"
    pretty ArgStatic  = text "static"
    pretty ArgStringifier = text "stringifier"
    pretty ArgTypedef = text "typedef"
    pretty ArgUnrestricted = text "unrestricted"


instance Pretty ReturnType where
    pretty (RetType t) = pretty t
    pretty RetVoid     = text "void"

instance Pretty Qualifier where
    pretty QuaStatic = text "static"
    pretty (QSpecials specials) = hsep $ map pretty specials

instance Pretty Special where
    pretty Getter = text "getter"
    pretty Setter = text "setter"
    pretty Ccreator = text "ccreator"
    pretty Deleter = text "deleter"
    pretty Legacycaller = text "legacycaller"


instance Pretty (Attribute a) where
    pretty (Attribute _ mInherit mReadOnly t i) =
        prettyInherit mInherit <> prettyMaybe mReadOnly pretty
                               <> text "attribute" <+> pretty t <+> pretty i <> semi

instance Pretty ReadOnly where
    pretty ReadOnly = text "readonly" <> space

instance Pretty Inherit where
    pretty Inherit = text "inherit" <> space

instance Pretty (Const a) where
    pretty (Const _ t i v) = text "const" <+> pretty t <+> pretty i <+> equals <+> pretty v <> semi

instance Pretty ConstType where
    pretty (ConstPrim ty mNull) = pretty ty <> prettyMaybe mNull pretty
    pretty (ConstIdent i mNull) = pretty i <> prettyMaybe mNull pretty

instance Pretty (ExceptionMember a) where
    pretty (ExConst _ c) = pretty c
    pretty (ExField _ t i) = pretty t <+> pretty i <> semi
