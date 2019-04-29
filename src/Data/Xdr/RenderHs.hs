module Data.Xdr.RenderHs where

import           GHC.Base
import           GHC.Show
import           System.IO
import           System.Environment
import           Data.Xdr.Parser
import           Data.Xdr.Types
import           Data.Either
import           Data.Text hiding (empty, intercalate, map)
import           Data.List


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
renderIdentifier (Identifier x) = (unpack x)


renderValue :: Value -> String
renderValue (Left x) = renderConstant x
renderValue (Right (IdentifierRef x))  = unpack x


renderConstant :: Constant -> String
renderConstant (DecConstant x) = show x
renderConstant (HexConstant x) = show x
renderConstant (OctConstant x) = show x


renderTypeDef :: TypeDef -> String
renderTypeDef (TypeDef x) =
  comment ++ "TypeDef"
renderTypeDef (TypeDefEnum ident body) = renderTypeDefEnum ident body
renderTypeDef (TypeDefStruct ident body) = renderTypeDefStruct ident body
renderTypeDef (TypeDefUnion ident body) = renderTypeDefUnion ident body


-- | data MsgType = CALL | REPLY deriving (Show, Eq)
renderTypeDefEnum :: Identifier -> EnumBody -> String
renderTypeDefEnum ident body =
  "data "
  ++ renderIdentifier ident
  ++ renderEnumBody body
  ++ renderEnumInstance ident body
  ++ eol


renderEnumEntryName :: (Identifier, Value) -> String
renderEnumEntryName (ident, _) = renderIdentifier ident


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
  ++ renderIdentifier ident
  ++ " where\n"
  ++ renderEnumeration body


renderEnumeration :: EnumBody -> String
renderEnumeration (x:|xs) =
  intercalate eol (map renderIdentifierValue xz)
  where xz = x:xs


renderIdentifierValue :: (Identifier, Value) -> String
renderIdentifierValue (i, v) =
    indent
    ++ "fromEnum "
    ++ renderIdentifier i
    ++ " = "
    ++ renderValue v
    ++ eol
    ++ indent
    ++ "toEnum "
    ++ renderValue v
    ++ " = "
    ++ renderIdentifier i

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
  where i = renderIdentifier ident

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
-- renderDeclaration (DeclarationOptional ts ident) =
--     renderTypeSpecifier ts
--     ++ " *" -- see RFC
--     ++ renderIdentifier ident

renderTypeSpecifier :: TypeSpecifier -> String
renderTypeSpecifier (TypeInt) = "Int32"
renderTypeSpecifier (TypeUnsignedInt) = "Int32"
renderTypeSpecifier (TypeHyper) = "Hyper?"
renderTypeSpecifier (TypeUnsignedHyper) = "UHyper"
renderTypeSpecifier (TypeFloat) = "Float"
renderTypeSpecifier (TypeDouble) = "Double"
renderTypeSpecifier (TypeQuadruple) = "Quad"
renderTypeSpecifier (TypeBool) = "Bool"
-- TODO:
--renderTypeSpecifier (TypeEnum EnumBody x) = "Enum"
--renderTypeSpecifier (TypeStruct StructBody x) = "Int"
--renderTypeSpecifier (TypeUnion UnionBody x) = "Int"
-- ? TypeIdentifier IdentifierRef ?
renderTypeSpecifier (TypeIdentifier x) = renderTypeIdentifier x
renderTypeSpecifier x = "Unknown"


-- ? TypeIdentifier IdentifierRef ?
renderTypeIdentifier :: IdentifierRef -> String
renderTypeIdentifier (IdentifierRef x) = unpack x

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
  where i = renderIdentifier ident


renderUnionBody :: UnionBody -> String
renderUnionBody x =
  eol
  ++ indent
  ++ "{ "
  ++ d ++ " :: ENUM_TYPE_TODO"
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
renderDiscriminant (DiscriminantEnum x) = renderIdentifier x


renderCaseSpec :: NonEmpty CaseSpec -> String
renderCaseSpec (x:|xs) =
  renderCaseSpecItem x
  ++ intercalate "" (map renderCaseSpecItem xz)
  where xz = x:xs


renderCaseSpecItem :: CaseSpec -> String
renderCaseSpecItem (CaseSpec _ (DeclarationVoid)) = empty
renderCaseSpecItem x =
  indent ++ ", "
  -- TODO : add Maybe
  ++ renderDeclaration (caseSpecDeclaration x)
  ++ eol


renderUnionDefault :: Maybe Declaration -> String
renderUnionDefault (Nothing) = empty
renderUnionDefault (Just x) =
  indent ++ ", "
  ++ renderDeclaration x
  ++ eol