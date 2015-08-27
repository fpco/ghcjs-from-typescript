module GHCJS.FromTypeScript.Render (render) where

import           Control.Applicative ((<$>))
import           Data.List (intercalate)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Monoid(..), (<>))
import           GHCJS.FromTypeScript.Types
import           GHCJS.FromTypeScript.Util
import           Language.TypeScript.Types

render :: M.Map ModuleName (M.Map String Decl)
       -> [(ModuleName, OutputModule)]
render =
  map (\(mn, decls) ->
    ( mn <> ModuleName ["Raw"]
    , OutputModule standardImports (concatMap renderDecl (M.elems decls))
    )) .
  M.toList

renderDecl :: Decl -> [String]
renderDecl (InterfaceDecl (Interface _ name mparams mextends body)) =
    [ ""
    , "newtype " ++ ty ++ " = " ++ name ++ " (GHCJS.JSRef (" ++ ty ++ "))"
    , "  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)"
    , "type instance TS.Members " ++ ty ++ " =" ++
      case mextends of
        Nothing -> ""
        Just extends -> " TS.Extends '[" ++ intercalate ", " (map renderTypeRef extends) ++ "]"
    ] ++ renderTypeBody ty body
  where
    ty = name ++
      case mparams of
        Nothing -> ""
        Just params -> " " <> intercalate " " (map renderTypeParameter params)

renderTypeBody :: String -> TypeBody -> [String]
renderTypeBody ty (TypeBody members) =
  vert 2
       "('[ "
       "  , "
       "  ])"
       (map (lines . renderMember ty . snd) members)

vert :: Int -> String -> String -> String -> [[String]] -> [String]
vert n leading between end ls =
    case ls of
      [] -> [indent ++ leading ++ end]
      ((x0 : xs0) : xss) ->
        (indent ++ leading ++ x0) :
        bodyLines xs0 ++
        concatMap (\(x : xs) -> (indent ++ between ++ x) : bodyLines xs) xss ++
        [indent ++ end]
  where
    indent = replicate n ' '
    bodyLines = map (\x -> indent ++ replicate (length between) ' ' ++ x)

renderMember :: String -> TypeMember -> String
renderMember _ (PropertySignature name optional mtype) =
  "'( 'TS.Property " ++ show name ++ ", " ++ renderOptional optional (renderType (maybeAny mtype)) ++ " )"
-- NOTE: overloading / specialization not yet handled, so this won't
-- work correctly for that
--
-- FIXME: figure out how to add generics here.
--
-- FIXME: should IO be on the return type insead of being implicit?
-- (if so, awareness of IO would need to be added to ghcjs-typescript)
renderMember _ (CallSignature plart) =
  "'( 'TS.Call, " ++ renderPlart plart ++ " )"
renderMember _ (MethodSignature name optional plart) =
    "TS." ++ optionalPrefix ++ "Method " ++ show name ++ " (" ++ renderPlart plart ++ ")"
  where
    optionalPrefix =
      case optional of
        Just Optional -> "Optional"
        Nothing -> ""
renderMember ty (ConstructSignature mtparams params mresult) =
    "'( 'TS.Constructor, " ++ renderFunctionType mtparams params result ++ " )"
  where
    -- FIXME: is it correct to use the type head for the default
    -- constructor return type?
    result = maybe ty renderType mresult
-- FIXME: should name be used? doesn't seem needed
renderMember _ (TypeIndexSignature (IndexSignature _name String ty)) =
  "'( 'TS.StringIndex, " ++ renderType ty ++ " )"
renderMember _ (TypeIndexSignature (IndexSignature _name Number ty)) =
  "'( 'TS.NumericIndex, " ++ renderType ty ++ " )"

renderPlart :: ParameterListAndReturnType -> String
renderPlart (ParameterListAndReturnType mtparams params mreturn) =
  renderFunctionType mtparams params (renderType (maybeVoid mreturn))

standardImports :: [String]
standardImports =
  [ "import qualified GHCJS.Types as GHCJS"
  , "import qualified GHCJS.Marshal as GHCJS"
  , "import qualified GHCJS.TypeScript.Types as TS"
  , "import GHCJS.TypeScript.Types ((:|:))"
  , "import qualified Data.Typeable"
  ] ++ map addType
  [ "Event", "HTMLElement", "Function" ]

addType :: String -> String
addType name = "newtype " ++ name ++ " = " ++ name ++ " (GHCJS.JSRef " ++ name ++ ")"

noComment :: CommentPlaceholder
noComment = Right mempty

maybeAny :: Maybe Type -> Type
maybeAny = fromMaybe (Predefined AnyType)

maybeVoid :: Maybe Type -> Type
maybeVoid = fromMaybe (Predefined VoidType)

--------------------------------------------------------------------------------
-- Render types

renderType :: Type -> String
renderType ty = case ty of
  Predefined AnyType -> "TS.Any"
  Predefined NumberType -> "TS.Number"
  Predefined BooleanType -> "TS.Boolean"
  Predefined StringType -> "TS.String"
  Predefined VoidType -> "TS.Void"
  TypeReference ref -> renderTypeRef ref
  ObjectType body -> unlines $ "TS.Object" : renderTypeBody (error "should be unused") body
  ArrayType ty -> "TS.Array (" ++ renderType ty ++ ")"
  UnionType t1 t2 -> "(" ++ renderType t1 ++ ") :|: (" ++ renderType t2 ++ ")"
  TupleType ts -> "(" ++ intercalate ", " (map renderType ts) ++ ")"
  FunctionType mtparams params result ->
    "(" ++ renderFunctionType mtparams params (renderType result) ++ ")"
  -- FIXME: should this be distinguished?
  ConstructorType mtparams params result ->
    renderFunctionType mtparams params (renderType result)

renderTypeRef :: TypeRef -> String
renderTypeRef (TypeRef name Nothing) =
    renderTypeName name
renderTypeRef (TypeRef name (Just params)) =
    "(" ++
    renderTypeName name ++
    concatMap (\ty -> " (" ++ renderType ty ++ ")") params ++
    ")"

renderTypeName :: TypeName -> String
renderTypeName (TypeName Nothing name) = name
renderTypeName x = notSupported x

renderFunctionType :: Maybe [TypeParameter] -> [Parameter] -> String -> String
renderFunctionType Nothing params result =
  concatMap ((++ " -> ") . renderParameter) params ++ result
renderFunctionType x _ _ = notSupported x

renderParameter :: Parameter -> String
renderParameter (RequiredOrOptionalParameter _ _ optional mtype) =
  renderOptional optional (renderType (maybeAny mtype))
renderParameter x = notSupported x

renderOptional :: Maybe Optional -> String -> String
renderOptional Nothing xs = xs
renderOptional (Just Optional) xs = "TS.Optional (" ++ xs ++ ")"

-- FIXME: is fancier munging required?
renderTypeParameter :: TypeParameter -> String
renderTypeParameter (TypeParameter name Nothing) = decapitalize name
renderTypeParameter x = notSupported x
