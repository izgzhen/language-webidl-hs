module Parser where

import AST

import Text.ParserCombinators.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok

type MyParser = CharParser ()

testParse :: MyParser a -> String -> Either ParseError a
testParse p = parse p ""

parseDef :: String -> Either ParseError (Definition ())
parseDef = parse pDef ""

pDef :: MyParser (Definition ())
pDef = DefInterface <$> pInterface


pInterface :: MyParser (Interface ())
pInterface = Interface () <$> (string "interface" *> pIdent) <*> pMaybeIdent <*> braces (many pInterfaceMember)

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
pReturnType = RetType <$> pType
          <|> string "void" *> return RetVoid

pConstValue :: MyParser ConstValue
pConstValue =  ConstBooleanLiteral <$> pBool
           <|> ConstFloatLiteral <$> pFloat
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
pPrimTy = PrimIntegerType <$> pIntegerType
      <|> PrimFloatType <$> pFloatType
      <|> string "boolean" *> return Boolean
      <|> string "byte" *> return Byte
      <|> string "octet" *> return Octet

pIntegerType :: MyParser IntegerType
pIntegerType = IntegerType <$> pUnsigned <*> pIntegerWidth

pUnsigned :: MyParser (Maybe Unsigned)
pUnsigned = optionMaybe (string "unsigned" *> return Unsigned)

pIntegerWidth = string "short" *> return Short
             <|> Long . length <$> many (string "long")

pFloatType :: MyParser FloatType
pFloatType =  TyFloat <$> (string "float" *> pUnrestricted)
          <|> TyDouble <$> (string "double" *> pUnrestricted)

pUnrestricted :: MyParser (Maybe Unrestricted)
pUnrestricted = optionMaybe (string "unrestricted" *> return Unrestricted)

pType :: MyParser Type
pType =  TySingleType <$> pSingleType
     <|> TyUnionType <$> pUnionType <*> pTypeSuffix

pSingleType :: MyParser SingleType
pSingleType =  STyNonAny <$> pNonAnyType 
           <|> STyAny <$> (string "any" *> pTypeSuffix)

pNonAnyType :: MyParser NonAnyType
pNonAnyType =  TyPrim <$> pPrimTy <*> pTypeSuffix
           <|> TyDOMString <$> (string "DOMString" *> pTypeSuffix)
           <|> TyIdent <$> pIdent <*> pTypeSuffix
           <|> TySequence <$> (string "sequence" *> angles pType) <*> pNull
           <|> TyObject <$> (string "object" *> pTypeSuffix)
           <|> TyDate <$> (string "Date" *> pTypeSuffix)

pTypeSuffix :: MyParser TypeSuffix
pTypeSuffix =  TypeSuffixArray <$> (string "[]" *> pTypeSuffix)
           <|> TypeSuffixListNullable <$> (string "?" *> pTypeSuffix)


pUnionType :: MyParser UnionType
pUnionType = parens (sepBy1 pUnionMemberType (string "or"))

pUnionMemberType :: MyParser UnionMemberType
pUnionMemberType =  UnionTy <$> pUnionType <*> pTypeSuffix 
                <|> UnionTyNonAny <$> pNonAnyType
                <|> UnionTyAny <$> (string "any [ ]" *> pTypeSuffix)

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
