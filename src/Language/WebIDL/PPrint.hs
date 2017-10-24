{-|
  Module:      Language.WebIDL.PPrint
  Description: Pretty printer of WebIDL AST
-}

module Language.WebIDL.PPrint (
  printDef
) where

import Language.WebIDL.AST
import Prelude hiding (Enum)
import Text.PrettyPrint.Leijen

-- | print definition
printDef :: Definition a -> String
printDef = show . pretty

instance Pretty (Definition a) where
    pretty (DefInterface x) = pretty x
    pretty (DefPartial x) = pretty x
    pretty (DefDictionary x) = pretty x
    pretty (DefEnum x) = pretty x
    pretty (DefTypedef x) = pretty x
    pretty (DefImplementsStatement x) = pretty x
    pretty (DefCallback x) = pretty x

instance Pretty (Interface a) where
    pretty (Interface _ extAttrs x mInherit members) =
        prettyExtAttrs extAttrs line <> text "interface" <+> pretty x <+> prettyInherit mInherit <+> scope members <> semi

instance Pretty (Callback a) where
    pretty (Callback _ f retty args) = text "callback" <+> pretty f <+>
                                       equals <+> pretty retty <+> prettyParenList args <> semi

prettyExtAttrs :: [ExtendedAttribute a] -> Doc -> Doc
prettyExtAttrs [] _ = empty
prettyExtAttrs attrs delimiter = brackets (hcat (punctuate (comma <> space) (map pretty attrs))) <> delimiter

instance Pretty (ExtendedAttribute a) where
    pretty (ExtendedAttributeNoArgs _ x) = pretty x
    pretty (ExtendedAttributeArgList _ x args) = pretty x <> prettyParenList args
    pretty (ExtendedAttributeIdent _ x y) = pretty x <+> equals <+> pretty y
    pretty (ExtendedAttributeIdentList _ x ids) = pretty x <+> equals <+> prettyParenList ids
    pretty (ExtendedAttributeNamedArgList _ x f args) = pretty x <+> equals <+> pretty f <> prettyParenList args

prettyMaybe Nothing  _ = empty
prettyMaybe (Just x) f = f x

prettyInherit x = prettyMaybe x (\e -> colon <+> pretty e)

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
    pretty (TySingleType s mNull) = pretty s <> prettyMaybe mNull pretty
    pretty (TyUnionType ut mNull) = prettyUnionType ut <> prettyMaybe mNull pretty

prettyUnionType ut = parens (hcat (punctuate (space <> text "or" <> space) (map pretty ut)))

instance Pretty SingleType where
    pretty (STyNonAny t) = pretty t
    pretty STyAny = text "any"

instance Pretty NonAnyType where
    pretty (TyPrim t) = pretty t
    pretty TyDOMString = text "DOMString"
    pretty TyUSVString = text "USVString"
    pretty TyByteString = text "ByteString"
    pretty TyError = text "Error"
    pretty (TyIdent ident) = pretty ident
    pretty (TySequence t) = text "sequence" <> angles (pretty t)
    pretty TyObject = text "object"
    pretty TyDate = text "Date"

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
    pretty (IntegerType mUns width_) = prettyMaybe mUns pretty <> pretty width_

instance Pretty IntegerWidth where
    pretty Short = text "short"
    pretty (Long i) = hsep $ take i (repeat (text "long"))

instance Pretty Unsigned where
    pretty Unsigned = text "unsigned"

instance Pretty UnionMemberType where
    pretty (UnionTy ut mNull) = pretty ut <> prettyMaybe mNull pretty
    pretty (UnionTyNonAny t mNull) = pretty t <> prettyMaybe mNull pretty

instance Pretty EnumValue where
    pretty (EnumValue s) = text s

instance Pretty (InterfaceMember a) where
    pretty (IMemConst c) = pretty c
    pretty (IMemAttribute attr) = pretty attr
    pretty (IMemOperation op) = pretty op

instance Pretty (Operation a) where
    pretty (Operation _ extAttrs mQ retty mIdent args) =
        prettyExtAttrs extAttrs space <> pretty mQ <> pretty retty
            <+> prettyMaybe mIdent pretty <> prettyParenList args <> semi

prettyParenList :: Pretty a => [a] -> Doc
prettyParenList args = parens (hcat (punctuate (comma <> space) (map pretty args)))

instance Pretty (Argument a) where
    pretty (ArgOptional extAttrs t name def) =
        prettyExtAttrs extAttrs space <> text "optional" <+> pretty t <+> pretty name <> prettyMaybe def prettyDefault
    pretty (ArgNonOpt extAttrs t mElli name) =
        prettyExtAttrs extAttrs space <> pretty t <> prettyMaybe mElli (\_ -> text "...") <+> pretty name

prettyDefault def = space <> equals <+> pretty def

instance Pretty ArgumentName where
    pretty (ArgKey key) = pretty key
    pretty (ArgIdent i) = pretty i

instance Pretty ArgumentNameKeyword where
    pretty ArgAttribute = text "attribute"
    pretty ArgCallback = text "callback"
    pretty ArgConst = text "const"
    pretty ArgDeleter = text "deleter"
    pretty ArgDictionary = text "dictionary"
    pretty ArgEnum = text "enum"
    pretty ArgIterable = text "iterable"
    pretty ArgGetter = text "getter"
    pretty ArgImplements = text "implements"
    pretty ArgInherit = text "inherit"
    pretty ArgInterface   = text "interface"
    pretty ArgLegacyCaller = text "legacycaller"
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
    pretty Deleter = text "deleter"
    pretty LegacyCaller = text "legacycaller"


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
