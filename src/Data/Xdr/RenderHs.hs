module Data.Xdr.RenderHs where

import           GHC.Base
import           GHC.Show
import           System.IO
import           System.Environment
import           Data.Xdr.Parser
import           Data.Xdr.Types
import           Data.Either
import           Data.Text hiding (empty, intercalate, map, toUpper)
import           Data.List
import           Data.Char (toUpper)

{-
choice :: (Foldable f, Alternative m) => f (m a) -> m a
  	-- Defined in ‘parser-combinators-1.0.0:Control.Applicative.Combinators’
find :: Foldable t => (a -> Bool) -> t a -> Maybe a
  	-- Defined in ‘Data.Foldable’
-}


eol :: String
eol = "\n"


comment :: String
comment = "-- "


indent :: String
indent = "  "


sep :: String
sep = " "


finishEnum :: String
finishEnum = "deriving (Eq, Show)"


capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs


doRender::IO()
doRender = do
  putStrLn empty
  [xdrsource] <- getArgs
  raw   <- readFile xdrsource
  let par = runStatefulParser specification
      res = runParser par xdrsource (pack raw)
  -- print res
  case res of
    Right (spec, _) -> renderSpec spec
    _ -> putStrLn empty


renderSpec :: Specification -> IO()
renderSpec [] = putStrLn empty
renderSpec (x:xs) = do
  case x of
    Right constdef -> putStrLn $ renderConstantDef constdef
    Left typedef -> putStrLn $ renderTypeDef typedef
  renderSpec xs


renderConstantDef :: ConstantDef -> String
renderConstantDef (ConstantDef idnt exp) =
  (renderIdentifier idnt)
  ++ " = "
  ++ (renderConstant exp)


renderIdentifier :: Identifier -> String
renderIdentifier (Identifier x) = unpack x


-- Ensure Capitalization for Types
renderTypeIdentifier :: Identifier -> String
renderTypeIdentifier (Identifier x) = capitalize $ unpack x


-- ? TypeIdentifier IdentifierRef ?
renderTypeIdentifier' :: IdentifierRef -> String
renderTypeIdentifier' (IdentifierRef x) = capitalize $ unpack x


renderValue :: Value -> String
renderValue (Left x) = renderConstant x
renderValue (Right (IdentifierRef x))  = unpack x


renderConstant :: Constant -> String
renderConstant (DecConstant x) = show x
renderConstant (HexConstant x) = show x
renderConstant (OctConstant x) = show x


renderTypeDef :: TypeDef -> String
renderTypeDef (TypeDefEnum ident body) = renderTypeDefEnum ident body
renderTypeDef (TypeDefStruct ident body) = renderTypeDefStruct ident body
renderTypeDef (TypeDefUnion ident body) = renderTypeDefUnion ident body
renderTypeDef (TypeDef decl) = renderTypeDeclaration decl


renderTypeDeclaration :: Declaration -> String
renderTypeDeclaration  (DeclarationSingle ts ident ) =
  "type " ++ renderTypeIdentifier ident ++ " = " ++ renderTypeSpecifier ts ++ eol
renderTypeDeclaration  (DeclarationArrayFixLen ts ident _)  =
  "type " ++ renderTypeIdentifier ident ++ " = [" ++ renderTypeSpecifier ts ++ "]" ++ eol
renderTypeDeclaration  (DeclarationArrayVarLen ts ident _) =
  "type " ++ renderTypeIdentifier ident ++ " = [" ++ renderTypeSpecifier ts ++ "]" ++ eol
renderTypeDeclaration  (DeclarationOpaqueFixLen ident _) =
  "type " ++ renderTypeIdentifier ident ++ " = String" ++ eol
renderTypeDeclaration  (DeclarationOpaqueVarLen ident _) =
  "type " ++ renderTypeIdentifier ident ++ " = String" ++ eol
renderTypeDeclaration  (DeclarationString ident _) =
  "type " ++ renderTypeIdentifier ident ++ " = String" ++ eol
renderTypeDeclaration  (DeclarationOptional ts ident) =
  "type " ++ renderTypeIdentifier ident ++ " = " ++ renderTypeSpecifier ts ++ eol
renderTypeDeclaration  (DeclarationVoid) = empty
renderTypeDeclaration x =
  comment ++ "TODO: Typedef"


-- | data MsgType = CALL | REPLY deriving (Show, Eq)
renderTypeDefEnum :: Identifier -> EnumBody -> String
renderTypeDefEnum ident body =
  "data "
  ++ renderTypeIdentifier ident
  ++ renderEnumBody body
  ++ renderEnumInstance ident body
  ++ eol


renderEnumEntryName :: (Identifier, Value) -> String
renderEnumEntryName (ident, _) = renderTypeIdentifier ident


renderEnumBody :: EnumBody -> String
renderEnumBody (x:|xs) =
  " = "
  ++ intercalate " | " (map renderEnumEntryName xz)
  ++ sep
  ++ finishEnum
  ++ eol
  where xz = x:xs


-- instance Enum MsgType where
--   fromEnum CALL = 0
--   toEnum 0 = CALL
renderEnumInstance :: Identifier -> EnumBody -> String
renderEnumInstance ident body =
  "instance Enum "
  ++ renderTypeIdentifier ident
  ++ " where\n"
  ++ renderEnumeration body


renderEnumeration :: EnumBody -> String
renderEnumeration (x:|xs) =
  intercalate eol (map renderIdentifierValueFromEnum xz)
  ++ eol
  ++ intercalate eol (map renderIdentifierValueToEnum xz)
  where xz = x:xs


renderIdentifierValueFromEnum :: (Identifier, Value) -> String
renderIdentifierValueFromEnum (i, v) =
  indent
  ++ "fromEnum "
  ++ renderTypeIdentifier i
  ++ " = "
  ++ renderValue v

renderIdentifierValueToEnum :: (Identifier, Value) -> String
renderIdentifierValueToEnum (i, v) =
  indent
  ++ "toEnum "
  ++ renderValue v
  ++ " = "
  ++ renderTypeIdentifier i

-- data RpcMsg = RpcMsg
--     { xid :: Int32
--     , msg :: MsgBody
--     } deriving (Show)
renderTypeDefStruct :: Identifier -> StructBody -> String
renderTypeDefStruct ident body =
  "data "
  ++ i ++ " = " ++ i
  ++ renderStructBody body
  ++ eol
  where i = renderTypeIdentifier ident

renderStructBody :: StructBody -> String
renderStructBody (x:|xs) =
  eol
  ++ indent
  ++ "{ "
  ++ intercalate ("\n" ++ indent ++ ", ") (map renderDeclaration xz)
  ++ eol
  ++ indent
  ++ "} deriving (Show)"
  where xz = x:xs


renderDeclaration :: Declaration -> String
renderDeclaration (DeclarationArrayFixLen ts ident val) =
  renderIdentifier ident
  ++ " :: ["
  ++ renderTypeSpecifier ts
  ++ "]"
  -- TODO: renderValue val
renderDeclaration (DeclarationOpaqueFixLen ident val) =
  renderIdentifier ident
  ++ " :: String"
  -- TODO: renderValue val
renderDeclaration (DeclarationOpaqueVarLen ident (Just val)) =
  renderIdentifier ident
  ++ " :: String"
  -- TODO: renderValue val
renderDeclaration (DeclarationOpaqueVarLen ident (Nothing)) =
  renderIdentifier ident
  ++ " :: String"
renderDeclaration (DeclarationSingle ts ident) =
  renderIdentifier ident
  ++ " :: "
  ++ renderTypeSpecifier ts
renderDeclaration (DeclarationArrayVarLen ts ident (Just val)) =
  renderIdentifier ident
  ++ " :: ["
  ++ renderTypeSpecifier ts
  ++ "]"
renderDeclaration (DeclarationArrayVarLen ts ident (Nothing)) =
  renderIdentifier ident
  ++ " :: ["
  ++ renderTypeSpecifier ts
  ++ "]"
renderDeclaration (DeclarationString ident (Just val)) =
  renderIdentifier ident
  ++ " :: String"
renderDeclaration (DeclarationString ident (Nothing)) =
  renderIdentifier ident
  ++ " :: String"
renderDeclaration (DeclarationVoid) = empty
-- TODO:
renderDeclaration (DeclarationOptional ts ident) =
  renderIdentifier ident
  ++ " :: " ++ renderTypeSpecifier ts
  ++ indent ++ comment ++ "OPTIONAL"

-- TODO: DONT COPY YOURSELF...
renderMaybeDeclaration :: Declaration -> String
renderMaybeDeclaration (DeclarationArrayFixLen ts ident val) =
  renderIdentifier ident
  ++ " :: Maybe ["
  ++ renderTypeSpecifier ts
  ++ "]"
renderMaybeDeclaration (DeclarationOpaqueFixLen ident val) =
  renderIdentifier ident
  ++ " :: Maybe String"
renderMaybeDeclaration (DeclarationOpaqueVarLen ident (Just val)) =
  renderIdentifier ident
  ++ " :: Maybe String"
renderMaybeDeclaration (DeclarationOpaqueVarLen ident (Nothing)) =
  renderIdentifier ident
  ++ " :: Maybe String"
renderMaybeDeclaration (DeclarationSingle ts ident) =
  renderIdentifier ident
  ++ " :: Maybe "
  ++ renderTypeSpecifier ts
renderMaybeDeclaration (DeclarationArrayVarLen ts ident (Just val)) =
  renderIdentifier ident
  ++ " :: Maybe ["
  ++ renderTypeSpecifier ts
  ++ "]"
renderMaybeDeclaration (DeclarationArrayVarLen ts ident (Nothing)) =
  renderIdentifier ident
  ++ " :: Maybe["
  ++ renderTypeSpecifier ts
  ++ "]"
renderMaybeDeclaration (DeclarationString ident (Just val)) =
  renderIdentifier ident
  ++ " :: Maybe String"
renderMaybeDeclaration (DeclarationString ident (Nothing)) =
  renderIdentifier ident
  ++ " :: Maybe String"
renderMaybeDeclaration (DeclarationVoid) = empty

renderTypeSpecifier :: TypeSpecifier -> String
renderTypeSpecifier (TypeInt) = "Int32"
renderTypeSpecifier (TypeUnsignedInt) = "Int32"
renderTypeSpecifier (TypeHyper) = "Int64"
renderTypeSpecifier (TypeUnsignedHyper) = "Int64"
renderTypeSpecifier (TypeFloat) = "Float"
renderTypeSpecifier (TypeDouble) = "Double"
renderTypeSpecifier (TypeQuadruple) = "Quad"
renderTypeSpecifier (TypeBool) = "Bool"
-- TODO:
--renderTypeSpecifier (TypeEnum EnumBody x) = "Enum"
--renderTypeSpecifier (TypeStruct StructBody x) = "Int"
--renderTypeSpecifier (TypeUnion UnionBody x) = "Int"
-- ? TypeIdentifier IdentifierRef ?
renderTypeSpecifier (TypeIdentifier x) = renderTypeIdentifier' x
renderTypeSpecifier x = "Unknown"


-- data MsgBody = MsgBody
--     { mtype :: MsgType
--     , cbody :: Maybe CallBody
--     , rbody :: Maybe ReplyBody
--     } deriving (Show)
renderTypeDefUnion :: Identifier -> UnionBody -> String
renderTypeDefUnion ident body =
  "data "
  ++ i ++ " = " ++ i
  ++ renderUnionBody body
  ++ eol
  where i = renderTypeIdentifier ident


renderUnionBody :: UnionBody -> String
renderUnionBody x =
  eol
  ++ indent
  ++ "{ "
  ++ d
  ++ eol
  ++ c
  ++ s
  ++ indent
  ++ "} deriving (Show)"
  where d = renderDiscriminant (unionDiscriminant x)
        c = renderCaseSpec (unionArms x)
        s = renderUnionDefault (unionDefault x)


renderDiscriminant :: Discriminant -> String
renderDiscriminant (DiscriminantInt x) = renderIdentifier x
renderDiscriminant (DiscriminantUnsignedInt x) = renderIdentifier x
renderDiscriminant (DiscriminantBool x) = renderIdentifier x
renderDiscriminant (DiscriminantEnum typeId varId) =
  (renderIdentifier varId) ++ " :: " ++ (renderTypeIdentifier typeId)


renderCaseSpec :: NonEmpty CaseSpec -> String
renderCaseSpec (x:|xs) =
  intercalate "" (map renderCaseSpecItem xz)
  where xz = x:xs


renderCaseSpecItem :: CaseSpec -> String
renderCaseSpecItem (CaseSpec _ (DeclarationVoid)) = empty
renderCaseSpecItem x =
  indent ++ ", "
  ++ renderMaybeDeclaration (caseSpecDeclaration x)
  ++ eol


renderUnionDefault :: Maybe Declaration -> String
renderUnionDefault (Nothing) = empty
renderUnionDefault (Just DeclarationVoid) = empty
renderUnionDefault (Just x) =
  indent ++ ", "
  ++ renderMaybeDeclaration x
  ++ eol
