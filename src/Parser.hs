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
pInherit = undefined

pReadOnly :: MyParser (Maybe ReadOnly)
pReadOnly = undefined

pOperation :: MyParser (Operation ())
pOperation = Operation () <$> pQualifier <*> pReturnType <*> pMaybeIdent <*> parens (many pArg) <* semi

pArg :: MyParser Argument
pArg = undefined

pQualifier :: MyParser Qualifier
pQualifier = undefined


pReturnType :: MyParser ReturnType
pReturnType = undefined

pConstValue :: MyParser pConstValue
pConstValue = undefined

pNull :: MyParser (Maybe Null)
pNull = undefined

pPrimTy :: MyParser PrimitiveType
pPrimTy = undefined

pType :: MyParser Type
pType = undefined

lexer = Tok.makeTokenParser emptyDef

parens     = Tok.parens lexer
braces     = Tok.braces lexer
reserved   = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
whiteSpace = Tok.whiteSpace lexer
pIdent     = Ident <$> Tok.identifier lexer
pInt       = Tok.integer lexer
semi       = Tok.semi lexer
pEq        = char '='
