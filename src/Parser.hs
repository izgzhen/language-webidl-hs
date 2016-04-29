module Parser where

import AST
import Prelude hiding (Enum)
import Text.ParserCombinators.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok

type MyParser = CharParser ()

testParse :: MyParser a -> String -> Either ParseError a
testParse p = parse p ""

parseIDL :: String -> Either ParseError [Definition ()]
parseIDL = parse (many (pDef <* spaces)) ""

pDef :: MyParser (Definition ())
pDef = DefInterface <$> pInterface
   <|> DefPartial <$> pPartial
   <|> DefDictionary <$> pDictionary
   <|> DefException <$> pException
   <|> DefEnum <$> pEnum
   <|> DefTypedef <$> pTypedef
   <|> DefImplementsStatement <$> pImplementsStatement

pPartial :: MyParser (Partial ())
pPartial = string "partial" *> spaces *> p
  where
    p =     PartialInterface () <$> (string "interface" *> spaces *> pIdent)
                                <*> braces (many pInterfaceMember) <* semi
        <|> PartialDictionary () <$> (string "dictionary" *> spaces *> pIdent)
                                 <*> braces (many pDictionaryMember) <* semi

pDictionary :: MyParser (Dictionary ())
pDictionary = Dictionary () <$> (string "dictionary" *> spaces *> pIdent)
                            <*> pMaybeIdent <*> braces (many pDictionaryMember) <* semi

pInterface :: MyParser (Interface ())
pInterface = Interface () <$> (string "interface" *> spaces *> pIdent)
                          <*> pMaybeIdent <*> braces (many pInterfaceMember)

pException :: MyParser (Exception ())
pException = Exception () <$> (string "exception" *> spaces *> pIdent)
                          <*> pMaybeIdent <*> braces (many pExceptionMember)

pEnum :: MyParser (Enum ())
pEnum = Enum () <$> (string "enum" *> spaces *> pIdent) <*> braces pEnumValues <* semi

pEnumValues :: MyParser [EnumValue]
pEnumValues = sepBy1 (EnumValue <$> stringLit) (char ',')


pTypedef :: MyParser (Typedef ())
pTypedef = Typedef () <$> (string "typedef" *> spaces *> pType) <*> pIdent <* semi

pImplementsStatement :: MyParser (ImplementsStatement ())
pImplementsStatement = ImplementsStatement () <$> pIdent <* spaces
                                              <*> (string "implements" *> spaces *> pIdent <* semi)

pDictionaryMember :: MyParser (DictionaryMember ())
pDictionaryMember = DictionaryMember () <$> pType <*> pIdent <*> pDefault <* semi

pExceptionMember :: MyParser (ExceptionMember ())
pExceptionMember =  ExConst () <$> pConst
                <|> ExField () <$> pType <*> pIdent <* semi

pMaybeIdent :: MyParser (Maybe Ident)
pMaybeIdent = pMaybeIdent

pInterfaceMember :: MyParser (InterfaceMember ())
pInterfaceMember =  IMemConst <$> pConst
                <|> IMemAttribute <$> pAttribute
                <|> IMemOperation <$> pOperation

pConst :: MyParser (Const ())
pConst = Const () <$> (string "const" *> pConstType) <*> (pIdent <* pEq) <*> (pConstValue <* char ';')

pConstType :: MyParser ConstType
pConstType =  ConstPrim <$> pPrimTy <*> pNull
          <|> ConstIdent <$> pIdent <*> pNull

pAttribute :: MyParser (Attribute ())
pAttribute = Attribute () <$> pInherit <*> pReadOnly <*> (string "attribute" *> pType) <*> (pIdent <* semi)

pInherit :: MyParser (Maybe Inherit)
pInherit = optionMaybe (string "inherit" *> return Inherit)

pReadOnly :: MyParser (Maybe ReadOnly)
pReadOnly = optionMaybe (string "readonly" *> return ReadOnly)

pOperation :: MyParser (Operation ())
pOperation = Operation () <$> pQualifier <*> pReturnType <*> pMaybeIdent <*> parens (many pArg) <* semi

pArg :: MyParser Argument
pArg =  ArgOptional <$> (string "optional" *> pType) <*> pArgumentName <*> pDefault
    <|> ArgNonOpt   <$> pType <*> pEllipsis <*> pArgumentName

pArgumentName :: MyParser ArgumentName
pArgumentName = ArgKey <$> pArgumentNameKeyword <|> ArgIdent <$> pIdent

pArgumentNameKeyword :: MyParser ArgumentNameKeyword
pArgumentNameKeyword =  string "attribute" *> return ArgAttribute
                    <|> string "callback" *> return ArgCallback
                    <|> string "const" *> return ArgConst
                    <|> string "creator" *> return ArgCreator
                    <|> string "deleter" *> return ArgDeleter
                    <|> string "dictionary" *> return ArgDictionary
                    <|> string "enum" *> return ArgEnum
                    <|> string "exception" *> return ArgException  
                    <|> string "getter" *> return ArgGetter
                    <|> string "implements" *> return ArgImplements
                    <|> string "inherit" *> return ArgInherit
                    <|> string "interface" *> return ArgInterface  
                    <|> string "legacycaller" *> return ArgLegacycaller
                    <|> string "partial" *> return ArgPartial
                    <|> string "setter" *> return ArgSetter
                    <|> string "static" *> return ArgStatic 
                    <|> string "stringifier" *> return ArgStringifier
                    <|> string "typedef" *> return ArgTypedef
                    <|> string "unrestricted" *> return ArgUnrestricted

pDefault :: MyParser Default
pDefault =  DefaultValue <$> pConstValue
        <|> DefaultString <$> stringLit


pQualifier :: MyParser Qualifier
pQualifier =  string "static" *> return QuaStatic
          <|> QSpecials <$> many pSpecial

pSpecial :: MyParser Special
pSpecial = string "getter" *> return Getter
       <|> string "setter" *> return Setter
       <|> string "ccreator" *> return Ccreator
       <|> string "deleter" *> return Deleter
       <|> string "legacycaller" *> return Legacycaller

pReturnType :: MyParser ReturnType
pReturnType = string "void" *> return RetVoid
          <|> RetType <$> pType

pConstValue :: MyParser ConstValue
pConstValue =  ConstBooleanLiteral <$> pBool
           <|> try (ConstFloatLiteral <$> pFloat)
           <|> ConstInteger <$> pInt
           <|> string "null" *> return ConstNull

pBool :: MyParser Bool
pBool =  string "true" *> return True
     <|> string "false" *> return False


pNull :: MyParser (Maybe Null)
pNull = optionMaybe (char '?' *> return Null)

pEllipsis :: MyParser (Maybe Ellipsis)
pEllipsis = optionMaybe (string "..." *> return Ellipsis)

pPrimTy :: MyParser PrimitiveType
pPrimTy = try (string "boolean" *> return Boolean)
      <|> try (string "byte" *> return Byte)
      <|> try (string "octet" *> return Octet)
      <|> PrimIntegerType <$> pIntegerType
      <|> PrimFloatType <$> pFloatType

pIntegerType :: MyParser IntegerType
pIntegerType = IntegerType <$> pUnsigned <* spaces <*> pIntegerWidth

pUnsigned :: MyParser (Maybe Unsigned)
pUnsigned = optionMaybe (string "unsigned" *> return Unsigned)

pIntegerWidth = string "short" *> return Short
             <|> Long . length <$> many1 (string "long" <* spaces)

pFloatType :: MyParser FloatType
pFloatType =  TyFloat <$> (string "float" *> spaces *> pUnrestricted)
          <|> TyDouble <$> (string "double" *> spaces *> pUnrestricted)

pUnrestricted :: MyParser (Maybe Unrestricted)
pUnrestricted = optionMaybe (string "unrestricted" *> return Unrestricted)

pType :: MyParser Type
pType =  TySingleType <$> pSingleType
     <|> TyUnionType <$> pUnionType <*> pTypeSuffix

pSingleType :: MyParser SingleType
pSingleType =  STyAny <$> (string "any" *> pTypeSuffix)
           <|> STyNonAny <$> pNonAnyType

pNonAnyType :: MyParser NonAnyType
pNonAnyType =  TyPrim <$> pPrimTy <*> pTypeSuffix
           <|> TySequence <$> (string "sequence" *> spaces *> angles pType) <*> pNull
           <|> TyObject <$> (string "object" *> pTypeSuffix)
           <|> try (TyDOMString <$> (string "DOMString" *> pTypeSuffix))
           <|> TyDate <$> (string "Date" *> pTypeSuffix)
           <|> TyIdent <$> pIdent <*> pTypeSuffix

pTypeSuffix :: MyParser TypeSuffix
pTypeSuffix =  option TypeSuffixNone $ TypeSuffixArray <$> (spaces *> string "[]" *> pTypeSuffix)
           <|> TypeSuffixListNullable <$> (spaces *> string "?" *> pTypeSuffix)

pUnionType :: MyParser UnionType
pUnionType = parens (sepBy1 pUnionMemberType (string "or"))

pUnionMemberType :: MyParser UnionMemberType
pUnionMemberType =  UnionTy <$> pUnionType <*> pTypeSuffix
                <|> UnionTyNonAny <$> pNonAnyType
                <|> UnionTyAny <$> (string "any []" *> pTypeSuffix)

lexer = Tok.makeTokenParser emptyDef

parens     = Tok.parens lexer
braces     = Tok.braces lexer
angles     = Tok.angles lexer
reserved   = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
whiteSpace = Tok.whiteSpace lexer
pIdent     = Ident <$> Tok.identifier lexer
pInt       = Tok.integer lexer
pFloat     = Tok.float lexer
semi       = Tok.semi lexer
stringLit  = Tok.stringLiteral lexer
pEq        = char '='
