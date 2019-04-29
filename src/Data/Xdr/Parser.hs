-- for Token s ~ Char in alphaNumChar
{-# LANGUAGE TypeFamilies #-}

module Data.Xdr.Parser
  ( specification
  , parseFile
  , runParser
  , runStatefulParser
  ) where

import qualified Data.Text              as T
import qualified Data.Xdr.Lexer         as L
import           Data.Xdr.ParserState   (ParserState, Positioned)
import qualified Data.Xdr.ParserState   as PS
import           Data.Xdr.Parsing
import           Data.Xdr.Parsing.Error
import           Data.Xdr.Types
import           Protolude              hiding (many, try)
import           Text.Megaparsec
import           GHC.Unicode            (isAlphaNum)

-- #########################################################
isAlphaNum' :: Char -> Bool
isAlphaNum' c = (isAlphaNum c) || (c == '_')

alphaNumChar' :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
-- for resolver 13:
-- alphaNumChar' = satisfy isAlphaNum' <?> "plus underscore alphanumeric character"
alphaNumChar' = L.satisfy isAlphaNum' <?> "plus underscore alphanumeric character"
-- #########################################################


parseFile :: FilePath -> IO (Either ParserError Specification)
parseFile path = do
  raw <- readFile path
  let parser = runStatefulParser specification
  forM (runParser parser path raw) $ \(sp, finalState) ->
    print finalState $> sp -- TODO: remove print

runStatefulParser :: StatefulParser a -> Parser (a, ParserState)
runStatefulParser = flip runStateT PS.initialState

specification :: Parsing p => p Specification
specification = between L.space eof $ sepEndBy definition L.space

definition :: Parsing p => p Definition
definition = eitherP typeDef constantDef

addTypeDef :: Parsing p => SourcePos -> Identifier -> TypeSpecifier -> p ()
addTypeDef pos id ts = do
  modify (PS.addTypeIdentifier (pos, (id, ts)))
  when (isScopeCreating ts) createScope

typeDef :: Parsing p => p TypeDef
typeDef = choice
  [ typeDef'
  , typeDefEnum
  , typeDefStruct
  , typeDefUnion
  ] where

  typeDef' :: Parsing p => p TypeDef
  typeDef' = TypeDef
    <$> (L.rword "typedef" *> declaration)

  typeDefEnum :: Parsing p => p TypeDef
  typeDefEnum = do
    (pos, id) <- positioned $ L.rword "enum" *> identifier
    body <- enumBody <* L.semicolon
    addTypeDef pos id (TypeEnum body)
    pure $ TypeDefEnum id body

  typeDefStruct :: Parsing p => p TypeDef
  typeDefStruct = do
    (pos, id) <- positioned $ L.rword "struct" *> identifier
    body <- structBody <* L.semicolon
    addTypeDef pos id (TypeStruct body)
    pure $ TypeDefStruct id body

  typeDefUnion :: Parsing p => p TypeDef
  typeDefUnion = do
    (pos, id) <- positioned $ L.rword "union" *> identifier
    body <- unionBody <* L.semicolon
    addTypeDef pos id (TypeUnion body)
    pure $ TypeDefUnion id body

enumBody :: Parsing p => p EnumBody
enumBody = L.braces $ L.nonEmptyList L.comma idValue
  where
  idValue :: Parsing p => p (Identifier, Value)
  idValue = (,)
    <$> identifier <* L.symbol "="
    <*> value

structBody :: Parsing p => p StructBody
structBody = L.braces $ L.nonEmptyLines declaration

unionBody :: Parsing p => p UnionBody
unionBody = body
  <$> (L.rword "switch" *> L.parens unionDiscriminant)
  <*> L.braces ((,) <$> unionArms <*> unionDefault)
  where
  body discr (arms, def) = UnionBody discr arms def

  unionDefault :: Parsing p => p (Maybe Declaration)
  unionDefault = optional $ L.rword "default" >> L.colon *> declaration

  unionDiscriminant :: Parsing p => p Discriminant
  unionDiscriminant = choice
    [ DiscriminantInt         <$> (typeInt  *> identifier)
    , DiscriminantUnsignedInt <$> (typeUInt *> identifier)
    , DiscriminantBool        <$> (typeBool *> identifier)
    -- TODO: added this fake stuff because of changed DiscriminantEnum ctor
    -- in order to keep Enum type info
    , DiscriminantEnum   (Identifier "XXX")     <$> (typeEnum *> identifier)
    , do (posRef, IdentifierRef r) <- positioned identifierRef
         pid <- positioned identifier
         parserState <- get
         typeSpec <- PS.typeSpecById (Identifier r) parserState &
           maybe (invalidDiscriminant posRef) (pure . unPositioned)
         typedDiscriminant pid typeSpec (Identifier r)
    ] where
    -- TODO: signature changed for DiscriminantEnum case
    typedDiscriminant
      :: Parsing p
      => Positioned Identifier
      -> TypeSpecifier
      -> Identifier
      -> p Discriminant
    typedDiscriminant (_, id) TypeInt         _ = pure $ DiscriminantInt  id
    typedDiscriminant (_, id) TypeUnsignedInt _ = pure $ DiscriminantUnsignedInt id
    typedDiscriminant (_, id) TypeBool        _ = pure $ DiscriminantBool id
    typedDiscriminant (_, id) (TypeEnum _)  eid = pure $ DiscriminantEnum eid id
    typedDiscriminant (pos, _) ts           _   = invalidDiscriminantType ts pos

  unionArms :: Parsing p => p (NonEmpty CaseSpec)
  unionArms = L.nonEmptyLines $
    CaseSpec <$> caseSpecValues <*> declaration

  caseSpecValues :: Parsing p => p (NonEmpty Value)
  caseSpecValues = L.nonEmptyLines $
    L.rword "case" *> value <* L.colon

declaration :: Parsing p => p Declaration
declaration = choice
  [ declarationSingle
  , declarationArrayFixLen
  , declarationArrayVarLen
  , declarationOpaqueFixLen
  , declarationOpaqueVarLen
  , declarationString
  , declarationOptional
  , declarationVoid
  ] where

  declarationSingle :: Parsing p => p Declaration
  declarationSingle = try $ do
    (pos, ts) <- positioned typeSpecifier
    id <- identifier
    _ <- L.semicolon
    addTypeDef pos id ts
    pure $ DeclarationSingle ts id

  declarationArrayFixLen :: Parsing p => p Declaration
  declarationArrayFixLen = try $ do
    (pos, ts) <- positioned typeSpecifier
    id <- identifier
    len <- L.brackets nonNegativeValue
    _ <- L.semicolon
    addTypeDef pos id ts
    pure $ DeclarationArrayFixLen ts id len

  declarationArrayVarLen :: Parsing p => p Declaration
  declarationArrayVarLen = try $ do
    (pos, ts) <- positioned typeSpecifier
    id <- identifier
    len <- L.angles (optional nonNegativeValue)
    _ <- L.semicolon
    addTypeDef pos id ts
    pure $ DeclarationArrayVarLen ts id len

  declarationOpaqueFixLen :: Parsing p => p Declaration
  declarationOpaqueFixLen = try $ DeclarationOpaqueFixLen
    <$> (L.rword "opaque" *> identifier)
    <*> L.brackets nonNegativeValue <* L.semicolon

  declarationOpaqueVarLen :: Parsing p => p Declaration
  declarationOpaqueVarLen = try $ DeclarationOpaqueVarLen
    <$> (L.rword "opaque" *> identifier)
    <*> L.angles (optional nonNegativeValue) <* L.semicolon

  declarationString :: Parsing p => p Declaration
  declarationString = try $ DeclarationString
    <$> (L.rword "string" *> identifier)
    <*> L.angles (optional nonNegativeValue) <* L.semicolon

  declarationOptional :: Parsing p => p Declaration
  declarationOptional = try $ do
    (pos, ts) <- positioned typeSpecifier <* L.symbol "*"
    id <- identifier
    _ <- L.semicolon
    addTypeDef pos id ts
    pure $ DeclarationOptional ts id

  declarationVoid :: Parsing p => p Declaration
  declarationVoid = DeclarationVoid
    <$ L.rword "void"
    <* L.semicolon

typeSpecifier :: Parsing p => p TypeSpecifier
typeSpecifier = choice
  [ typeInt
  , typeUInt
  , typeHyper
  , typeUHyper
  , typeFloat
  , typeDouble
  , typeQuadruple
  , typeBool
  , typeEnum
  , typeStruct
  , typeUnion
  , typeIdentifier
  ]

unsigned :: Parsing p => p ()
unsigned = L.rword "unsigned"

typeUInt :: Parsing p => p TypeSpecifier
typeUInt = TypeUnsignedInt
  <$ try (unsigned >> L.rword "int")

typeInt :: Parsing p => p TypeSpecifier
typeInt = TypeInt <$ L.rword "int"

typeUHyper :: Parsing p => p TypeSpecifier
typeUHyper = TypeUnsignedHyper
  <$ try (unsigned >> L.rword "hyper")

typeHyper :: Parsing p => p TypeSpecifier
typeHyper = TypeHyper <$ L.rword "hyper"

typeFloat :: Parsing p => p TypeSpecifier
typeFloat = TypeFloat <$ L.rword "float"

typeDouble :: Parsing p => p TypeSpecifier
typeDouble = TypeDouble <$ L.rword "double"

typeQuadruple :: Parsing p => p TypeSpecifier
typeQuadruple = TypeQuadruple <$ L.rword "quadruple"

typeBool :: Parsing p => p TypeSpecifier
typeBool = TypeBool <$ L.rword "bool"

typeEnum :: Parsing p => p TypeSpecifier
typeEnum = TypeEnum <$> try (L.rword "enum" *> enumBody)

typeStruct :: Parsing p => p TypeSpecifier
typeStruct = TypeStruct <$> try (L.rword "struct" *> structBody)

typeUnion :: Parsing p => p TypeSpecifier
typeUnion = TypeUnion <$> try (L.rword "union" *> unionBody)

typeIdentifier :: Parsing p => p TypeSpecifier
typeIdentifier = TypeIdentifier <$> identifierRef

value :: Parsing p => p Value
value = eitherP constant identifierRef

nonNegativeValue :: Parsing p => p Value
nonNegativeValue = do
  pos <- getPosition
  v <- value
  checkNonNegValue pos v
  pure v

  where
    checkNonNegValue :: Parsing p => SourcePos -> Value -> p ()
    checkNonNegValue pos (Left cd)  =
      when (isNegativeConst cd) (negativeArrayLengthConst pos)
    checkNonNegValue pos (Right idr@(IdentifierRef id)) =
      whenM (isNegativeIdRef idr pos) (negativeArrayLengthId id pos)

    isNegativeConstDef (ConstantDef _ c) = isNegativeConst c

    isNegativeConst (DecConstant i) = i < 0
    isNegativeConst (HexConstant i) = i < 0
    isNegativeConst (OctConstant i) = i < 0

    isNegativeIdRef :: Parsing p => IdentifierRef -> SourcePos -> p Bool
    isNegativeIdRef idr@(IdentifierRef id) pos = do
      parserState <- get
      PS.constantById parserState idr &
        maybe (negativeArrayLengthId id pos)
              (pure . isNegativeConstDef . unPositioned)

constantDef :: Parsing p => p ConstantDef
constantDef = do
  (pos, word) <- positioned $ L.rword "const" *> identifierWord
  let id' = Identifier word
  existing <- get <&> PS.typeOrConstandIdPos id'
  id <- maybe (pure id') (conflictingIdentifier word) existing
  val <- L.symbol "=" *> constant <* L.semicolon
  let cd = ConstantDef id val
  modify (PS.addConstantDef (pos, cd))
  pure cd

constant :: Parsing p => p Constant
constant = choice
  [ hexadecimalConstant
  , octalConstant
  , decimalConstant
  ]

identifierWord :: Parsing p => p Text
identifierWord = L.lexeme . try $ do
  word <- T.pack <$> ((:) <$> L.letterChar <*> many alphaNumChar')
  -- Check keyword
  if word `elem` L.reservedWords
    then keywordIdentifier word
    else pure word

identifierRef :: Parsing p => p IdentifierRef
identifierRef = IdentifierRef <$> identifierWord

identifier :: Parsing p => p Identifier
identifier = try $ do
  word <- identifierWord
  let id = Identifier word
  -- Check uniqueness
  -- TODO: identifiers with same name in different struct
  --       should not be treated as if they are in the same scope
  --existing <- get <&> PS.identifierPos id
  --maybe (pure id) (conflictingIdentifier word) existing
  pure id

isScopeCreating :: TypeSpecifier -> Bool
isScopeCreating (TypeStruct _) = True
isScopeCreating (TypeUnion  _) = True
isScopeCreating _              = False

createScope :: Parsing p => p ()
createScope = modify PS.createScope

decimalConstant :: Parsing p => p Constant
decimalConstant = DecConstant
  <$> L.signed L.space (L.lexeme L.decimal)

octalConstant :: Parsing p => p Constant
octalConstant = OctConstant
  <$> try (L.char '0' >> L.octal)

hexadecimalConstant :: Parsing p => p Constant
hexadecimalConstant = HexConstant
  <$> try (L.char '0' >> L.char' 'x' >> L.hexadecimal)

positioned :: Parsing p => p a -> p (Positioned a)
positioned p = (,) <$> getPosition <*> p

unPositioned :: Positioned a -> a
unPositioned = snd
