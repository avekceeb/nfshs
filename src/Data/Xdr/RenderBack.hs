-- {-# LANGUAGE OverloadedStrings #-}

module Data.Xdr.RenderBack where

import           Data.Xdr.Parser
import           Data.Xdr.Types
import           System.Environment
import           GHC.Base
import           Data.Either
import           System.IO
import           Data.Text hiding (empty)
import           GHC.Show

doRender::IO()
doRender = do
    putStrLn empty
    [xdrsource] <- getArgs
    raw   <- readFile xdrsource
    print raw
    putStrLn empty
    putStrLn empty
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


renderConstant :: Constant -> String
renderConstant (DecConstant x) = show x
renderConstant (HexConstant x) = show x
renderConstant (OctConstant x) = show x


renderTypeDef :: TypeDef -> String
renderTypeDef (TypeDef x) = "TypeDef"
renderTypeDef (TypeDefEnum ident body) =
    "Enum "
    ++ (renderIdentifier ident)
    ++ " "
    ++ renderEnumBody body
renderTypeDef (TypeDefStruct ident body) =
    "Struct "
    ++ renderIdentifier ident
    ++ renderStructBody body
renderTypeDef (TypeDefUnion ident body) =
    "Union "
    ++ renderIdentifier ident
    ++ renderUnionBody body


renderEnumBody :: EnumBody -> String
renderEnumBody (x:|xs) =
    " {\n"
    ++ renderEnumEntry x
    ++ "\n"
    ++ renderEnumBody' xs
    ++ "}"
renderEnumBody x = empty


-- ? Is there a better way to process NonEmpty ?
renderEnumBody' :: [(Identifier, Value)] -> String
renderEnumBody' [] = empty
renderEnumBody' (x:xs) =
    renderEnumEntry x
    ++ "\n"
    ++ renderEnumBody' xs


renderEnumEntry :: (Identifier, Value) -> String
renderEnumEntry (ident, val) = renderIdentifier ident
    ++ " = "
    ++ renderValue val


renderValue :: Value -> String
renderValue (Left x) = renderConstant x
renderValue (Right (IdentifierRef x))  = unpack x


renderStructBody :: StructBody -> String
renderStructBody (x:|xs) =
    " {\n"
    ++ renderDeclaration x
    ++ "\n"
    ++ renderStructBody' xs
    ++ "}"
renderStructBody x = empty


renderStructBody' :: [Declaration] -> String
renderStructBody' [] = empty
renderStructBody' (x:xs) =
    renderDeclaration x
    ++ "\n"
    ++ renderStructBody' xs


renderDeclaration :: Declaration -> String
renderDeclaration (DeclarationArrayFixLen ts ident val) =
    renderTypeSpecifier ts
    ++ " "
    ++ renderIdentifier ident
    ++ "["
    ++ renderValue val
    ++ "]"
renderDeclaration (DeclarationOpaqueFixLen ident val) =
    "opaque "
    ++ renderIdentifier ident
    ++ "["
    ++ renderValue val
    ++ "]"
renderDeclaration (DeclarationOpaqueVarLen ident (Just val)) =
    "opaque "
    ++ renderIdentifier ident
    ++ "<"
    ++ renderValue val
    ++ ">"
renderDeclaration (DeclarationOpaqueVarLen ident (Nothing)) =
    "opaque "
    ++ renderIdentifier ident
    ++ "<>"
renderDeclaration (DeclarationSingle ts ident) =
    renderTypeSpecifier ts
    ++ " "
    ++ renderIdentifier ident
renderDeclaration (DeclarationArrayVarLen ts ident (Just val)) =
    renderTypeSpecifier ts
    ++ " "
    ++ renderIdentifier ident
    ++ "<"
    ++ renderValue val
    ++ ">"
renderDeclaration (DeclarationArrayVarLen ts ident (Nothing)) =
    renderTypeSpecifier ts
    ++ " "
    ++ renderIdentifier ident
    ++ "<>"
renderDeclaration (DeclarationString ident (Just val)) =
    "string "
    ++ renderIdentifier ident
    ++ "<"
    ++ renderValue val
    ++ ">"
renderDeclaration (DeclarationString ident (Nothing)) =
    "string "
    ++ renderIdentifier ident
    ++ "<>"
renderDeclaration (DeclarationVoid) = empty
renderDeclaration (DeclarationOptional ts ident) =
    renderTypeSpecifier ts
    ++ " *" -- see RFC
    ++ renderIdentifier ident


renderTypeSpecifier :: TypeSpecifier -> String
renderTypeSpecifier (TypeInt) = "Int"
renderTypeSpecifier (TypeUnsignedInt) = "UInt"
renderTypeSpecifier (TypeHyper) = "Hyper"
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


renderUnionBody :: UnionBody -> String
renderUnionBody x =
    " switch ("
    ++ renderDiscriminant (unionDiscriminant x)
    ++ ") {\n"
    ++ renderCaseSpec (unionArms x)
    ++ renderUnionDefault (unionDefault x)
    ++ "}\n"


renderDiscriminant :: Discriminant -> String
-- ? Is there a simpler notation for this ?
renderDiscriminant (DiscriminantInt x) = renderIdentifier x
renderDiscriminant (DiscriminantUnsignedInt x) = renderIdentifier x
renderDiscriminant (DiscriminantBool x) = renderIdentifier x
renderDiscriminant (DiscriminantEnum x) = renderIdentifier x


renderCaseSpec :: NonEmpty CaseSpec -> String
renderCaseSpec (x:|xs) =
    renderCaseSpecItem x
    ++ renderCaseSpec' xs


renderCaseSpec' :: [CaseSpec] -> String
renderCaseSpec' [] = empty
renderCaseSpec' (x:xs) =
    renderCaseSpecItem x
    ++ renderCaseSpec' xs


renderCaseSpecItem :: CaseSpec -> String
renderCaseSpecItem x =
    renderCaseSpecValues (caseSpecValues x)
    ++ renderDeclaration (caseSpecDeclaration x)
    ++ "\n"

renderCaseSpecValues :: NonEmpty Value -> String
renderCaseSpecValues (x:|xs) =
    "case "
    ++ renderValue x
    ++ " : "
    ++ renderCaseSpecValues' xs
    ++ "\n"


renderCaseSpecValues' :: [Value] -> String
renderCaseSpecValues' [] = empty
renderCaseSpecValues' (x:xs) =
    "case "
    ++ renderValue x
    ++ " : "
    ++ renderCaseSpecValues' xs


renderUnionDefault :: Maybe Declaration -> String
renderUnionDefault (Just x) =
    "Default:\n"
    ++ renderDeclaration x
renderUnionDefault (Nothing) = empty
