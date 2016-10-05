{-|
  Module:      Language.WebIDL.Parser
  Description: Parser of WebIDL source
-}

module Language.WebIDL.Parser (
  Tag(..), MyParser, ParserState, Comment, parseIDL, tryParse, pDef, pExtAttrs, pExtAttr, pPartial, pDictionary,
  pInterface, pException, pInheritance, pEnum, pEnumValues, pTypedef, pImplementsStatement,
  pDictionaryMember, pExceptionMember, pMaybeIdent, pInterfaceMember, pConst, pConstType,
  pAttribute, pOperation, pArg, pArgumentName, pArgumentNameKeyword, pDefault, pQualifier,
  pSpecial, pReturnType, pConstValue, pBool, pNull, pPrimTy, pIntegerType, pUnsigned, pFloatType,
  pType, pSingleType, pNonAnyType, pTypeSuffix, pUnionType, pUnionMemberType,
  pIdent, spaces, pParenComma, pString, pStringEnds, string, parens) where

import Language.WebIDL.AST
import Prelude hiding (Enum)
import Text.ParserCombinators.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec (modifyState, SourcePos, getPosition, getState, putState, sourceLine)
import qualified Text.Parsec.Token as Tok

data Comment = LineComment String | BlockComment String

data ParserState = ParserState {
  _comments' :: [Comment]
}

-- | Tag of source
data Tag = Tag {
  _comments  :: [Comment],
  _sourcePos :: SourcePos
}

instance Eq Tag where
  (==) _ _ = True

instance Show Comment where
  show (LineComment l) = show l
  show (BlockComment s) = show (take 5 s) ++ "..."

instance Show Tag where
    show (Tag comment pos) = show comment ++ "(" ++ show (sourceLine pos) ++ ")"

initState :: ParserState
initState = ParserState []

type MyParser = CharParser ParserState

tryParse :: MyParser a -> String -> Either ParseError a
tryParse p = runParser p initState "webidl"

-- | parse IDL source
parseIDL :: String -> Either ParseError [Definition Tag]
parseIDL = tryParse (pSpaces *> many1 (pDef <* pSpaces))

pDef :: MyParser (Definition Tag)
pDef = try (DefInterface <$> pInterface)
   <|> DefCallback <$> pCallback
   <|> DefPartial <$> pPartial
   <|> DefDictionary <$> pDictionary
   <|> DefException <$> pException
   <|> DefEnum <$> pEnum
   <|> DefTypedef <$> pTypedef
   <|> DefImplementsStatement <$> pImplementsStatement

pCallback :: MyParser (Callback Tag)
pCallback = Callback <$> (string "callback" *> pSpaces *> getTag)
                     <*> pIdent
                     <*> (pEq *> pReturnType <* pSpaces)
                     <*> pParenComma pArg <* semi

pExtAttrs :: MyParser [ExtendedAttribute Tag]
pExtAttrs = try (brackets (pSpaces *> sepBy (pExtAttr <* pSpaces) (char ',' <* pSpaces)))
        <|> return []

pExtAttr :: MyParser (ExtendedAttribute Tag)
pExtAttr = try (ExtendedAttributeNamedArgList <$> getTag <*> (pIdent <* pEq) <*> pIdent <*> pParenComma pArg)
       <|> try (ExtendedAttributeArgList <$> getTag <*> pIdent <*> pParenComma pArg)
       <|> try (ExtendedAttributeIdent <$> getTag <*> (pIdent <* pEq) <*> pIdent)
       <|> try (ExtendedAttributeIdentList <$> getTag <*> (pIdent <* pEq) <*> pParenComma pIdent)
       <|> ExtendedAttributeNoArgs <$> getTag <*> pIdent

pPartial :: MyParser (Partial Tag)
pPartial = string "partial" *> pSpaces *> p
  where
    p =   PartialInterface <$> getTag <*> (string "interface" *> pSpaces *> pIdent)
                              <*> braces (many pInterfaceMember) <* semi
      <|> PartialDictionary <$> getTag <*> (string "dictionary" *> pSpaces *> pIdent)
                               <*> braces (many pDictionaryMember) <* semi

pDictionary :: MyParser (Dictionary Tag)
pDictionary = Dictionary <$> getTag <*> (string "dictionary" *> pSpaces *> pIdent)
                         <*> pInheritance <*> braces (many pDictionaryMember) <* semi

pInterface :: MyParser (Interface Tag)
pInterface = Interface <$> getTag <*> pExtAttrs <*> (string "interface" *> pSpaces *> pIdent)
                       <*> pInheritance <*> braces (pSpaces *> many (pInterfaceMember <* pSpaces)) <* semi

pException :: MyParser (Exception Tag)
pException = Exception <$> getTag <*> (string "exception" *> pSpaces *> pIdent)
                          <*> pInheritance <*> braces (many pExceptionMember)

pInheritance :: MyParser (Maybe Ident)
pInheritance = optionMaybe (spaces *> char ':'  *> spaces *> pIdent)

pEnum :: MyParser (Enum Tag)
pEnum = Enum <$> getTag <*> (string "enum" *> pSpaces *> pIdent) <*> braces pEnumValues <* semi

pEnumValues :: MyParser [EnumValue]
pEnumValues = sepBy1 (EnumValue <$> stringLit) (char ',')

pTypedef :: MyParser (Typedef Tag)
pTypedef = do
  tag <- getTag
  _ <- string "typedef"
  pSpaces
  ty <- try pType
  pSpaces
  ident <- pIdent
  _ <- semi
  return (Typedef tag ty ident)

pImplementsStatement :: MyParser (ImplementsStatement Tag)
pImplementsStatement = ImplementsStatement <$> getTag <*> pIdent <* pSpaces
                                              <*> (string "implements" *> pSpaces *> pIdent <* semi)

pDictionaryMember :: MyParser (DictionaryMember Tag)
pDictionaryMember = DictionaryMember <$> getTag <*> pType <* pSpaces
                                     <*> pIdent <*> pDefault <* semi

pExceptionMember :: MyParser (ExceptionMember Tag)
pExceptionMember =  ExConst <$> getTag <*> pConst
                <|> ExField <$> getTag <*> pType <*> pIdent <* semi

pMaybeIdent :: MyParser (Maybe Ident)
pMaybeIdent = optionMaybe pIdent

pInterfaceMember :: MyParser (InterfaceMember Tag)
pInterfaceMember =  try (IMemConst <$> pConst)
                <|> try (IMemAttribute <$> pAttribute)
                <|> IMemOperation <$> pOperation

pConst :: MyParser (Const Tag)
pConst = Const <$> getTag <*> (string "const" *> pSpaces *> pConstType <* pSpaces)
               <*> (pIdent <* pEq) <*> (pSpaces *> pConstValue <* semi)

pConstType :: MyParser ConstType
pConstType =  ConstPrim <$> pPrimTy <*> pNull
          <|> ConstIdent <$> pIdent <*> pNull

pAttribute :: MyParser (Attribute Tag)
pAttribute = Attribute <$> getTag <*> pModifier Inherit "inherit"
                       <*> pModifier ReadOnly "readonly"
                       <*> (string "attribute" *> pSpaces *> pType)
                       <*> (pSpaces *> pIdent <* semi)

pModifier :: a -> String -> MyParser (Maybe a)
pModifier m s = optionMaybe (string s *> pSpaces *> return m)

pOperation :: MyParser (Operation Tag)
pOperation = Operation <$> getTag <*> pExtAttrs <*> pQualifier <* spaces
                       <*> pReturnType <* pSpaces
                       <*> pMaybeIdent <* pSpaces
                       <*> pParenComma pArg <* semi

pArg :: MyParser (Argument Tag)
pArg =  try (ArgOptional <$> pExtAttrs <*> (string "optional" *> spaces *> pType <* pSpaces) <*> pArgumentName <*> pDefault)
    <|> ArgNonOpt <$> pExtAttrs <*> (pType <* pSpaces) <*> (pModifier Ellipsis "...") <*> (pSpaces *> pArgumentName)

pArgumentName :: MyParser ArgumentName
pArgumentName = try (ArgKey <$> pArgumentNameKeyword)
            <|> ArgIdent <$> pIdent

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

pDefault :: MyParser (Maybe Default)
pDefault = Just <$> (spaces *> pEq *> spaces *> pDefault')
       <|> return Nothing
  where
    pDefault' = DefaultValue <$> pConstValue
            <|> DefaultString <$> stringLit


pQualifier :: MyParser (Maybe Qualifier)
pQualifier =  try (string "static" *> return (Just QuaStatic))
          <|> try (Just . QSpecials <$> many pSpecial)
          <|> return Nothing

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

pPrimTy :: MyParser PrimitiveType
pPrimTy = try (string "boolean" *> return Boolean)
      <|> try (string "byte" *> return Byte)
      <|> try (string "octet" *> return Octet)
      <|> try (PrimIntegerType <$> pIntegerType)
      <|> PrimFloatType <$> pFloatType

pIntegerType :: MyParser IntegerType
pIntegerType = IntegerType <$> pUnsigned <* pSpaces <*> pIntegerWidth
  where
    pIntegerWidth = string "short" *> return Short
                 <|> Long . length <$> many1 (try (string "long" <* pSpaces))

pUnsigned :: MyParser (Maybe Unsigned)
pUnsigned = optionMaybe (string "unsigned" *> return Unsigned)


pFloatType :: MyParser FloatType
pFloatType =  try (TyFloat <$> pModifier Unrestricted "unrestricted" <* spaces <* string "float")
          <|> TyDouble <$> pModifier Unrestricted "unrestricted" <* spaces <* string "double"

pType :: MyParser Type
pType =  TySingleType <$> pSingleType
     <|> TyUnionType <$> pUnionType <*> pTypeSuffix

pSingleType :: MyParser SingleType
pSingleType =  STyAny <$> (string "any" *> pTypeSuffix)
           <|> STyNonAny <$> pNonAnyType

pNonAnyType :: MyParser NonAnyType
pNonAnyType =  try (TyPrim <$> pPrimTy <*> pTypeSuffix)
           <|> TySequence <$> (string "sequence" *> pSpaces *> angles pType) <*> pNull
           <|> TyObject <$> (string "object" *> pTypeSuffix)
           <|> try (TyDOMString <$> (string "DOMString" *> pTypeSuffix))
           <|> try (TyDate <$> (string "Date" *> pTypeSuffix))
           <|> TyIdent <$> pIdent <*> pTypeSuffix

pTypeSuffix :: MyParser TypeSuffix
pTypeSuffix =  try (string "[]" *> return TypeSuffixArray)
           <|> try (char '?' *> return TypeSuffixNullable)
           <|> return TypeSuffixNone

-- FIXME: Not working correctly currently
pUnionType :: MyParser UnionType
pUnionType = parens (sepBy1 pUnionMemberType (spaces *> string "or" <* spaces))

pUnionMemberType :: MyParser UnionMemberType
pUnionMemberType =  UnionTy <$> pUnionType <*> pTypeSuffix
                <|> UnionTyNonAny <$> pNonAnyType
                <|> UnionTyAny <$> (string "any []" *> pTypeSuffix)

lexer = Tok.makeTokenParser emptyDef

parens     = Tok.parens lexer
brackets   = Tok.brackets lexer
braces     = Tok.braces lexer
angles     = Tok.angles lexer
pIdent     = Ident <$> Tok.identifier lexer
pInt       = Tok.integer lexer
pFloat     = Tok.float lexer
semi       = Tok.semi lexer
stringLit  = Tok.stringLiteral lexer
pEq        = spaces *> char '=' <* spaces

pSpaces = try (skipMany (spaces *> pComment <* spaces) <* spaces)
      <|> spaces

pComment = try pLineComment <|> pBlockComment

pLineComment = do
  _ <- string "//"
  comment <- manyTill anyChar (try newline)
  modifyState (\ps -> ParserState { _comments' = _comments' ps ++ [LineComment comment]})

pBlockComment = do
  _ <- string "/*"
  comment <- manyTill anyChar (try (string "*/"))
  modifyState (\ps -> ParserState { _comments' = _comments' ps ++ [BlockComment comment]})

getTag :: MyParser Tag
getTag = do
  pos <- getPosition
  ParserState comments <- getState
  putState $ ParserState []
  return $ Tag comments pos

pParenComma :: MyParser a -> MyParser [a]
pParenComma p = parens (pSpaces *> sepBy (p <* pSpaces) (char ',' <* pSpaces))

pString :: MyParser String
pString = many anyChar

pStringEnds :: String -> MyParser String
pStringEnds s = manyTill anyChar (try (string s))
