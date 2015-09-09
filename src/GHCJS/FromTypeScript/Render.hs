{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
module GHCJS.FromTypeScript.Render (render) where

import           Control.Applicative ((<$>))
import           Data.List (intercalate)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid (Monoid(..), (<>))
import qualified Data.Set as S
import           Debug.Trace (trace)
import           GHCJS.FromTypeScript.Munge
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
renderDecl (InterfaceDecl (Interface _ name mtparams mextends body)) =
    [ ""
    , "newtype " ++ ty ++ " = " ++ mungedName ++ " (GHCJS.JSRef (" ++ ty ++ "))"
    , "  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)"
    , "type instance TS.Members " ++ ty ++ " =" ++
      case mextends of
        Nothing -> ""
        Just extends -> " TS.Extends '[" ++ intercalate ", " (map (renderTypeRef tvs) extends) ++ "]"
    ] ++ renderTypeBody tvs tyref body
  where
    --FIXME was done with DatatypeContexts
    -- ctxt = maybe "" renderTypeParametersContext tvs mtparams
    tvs = maybe mempty paramsToTyVars mtparams
    mungedName = mungeUpperName name
    tyref = TypeRef (TypeName Nothing name) $
      case mtparams of
        Nothing -> Nothing
        Just tparams -> Just $
          map (\(TypeParameter name _) -> TypeReference (TypeRef (TypeName Nothing name) Nothing)) tparams
    ty = mungedName ++
      case mtparams of
        Nothing -> ""
        Just params -> " " <> intercalate " " (map renderTypeParameterName params)

renderTypeBody :: TyVars -> TypeRef -> TypeBody -> [String]
renderTypeBody tvs tyref (TypeBody members) =
  vert 2
       "('[ "
       "  , "
       "  ])"
       (map (lines . renderMember tvs tyref . snd) members)

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

renderMember :: TyVars -> TypeRef -> TypeMember -> String
renderMember tvs _ (PropertySignature name optional mtype) =
  show name ++ " ::" ++ (if isJust optional then "? " else ": ") ++
  renderType tvs (maybeAny mtype)
-- NOTE: overloading / specialization not yet handled, so this won't
-- work correctly for that
--
-- FIXME: figure out how to add generics here.
--
-- FIXME: should IO be on the return type insead of being implicit?
-- (if so, awareness of IO would need to be added to ghcjs-typescript)
renderMember tvs _ (MethodSignature name optional plart) =
  show name ++ " ::" ++ (if isJust optional then "? " else ": ") ++
  "TS.Fun (" ++ renderPlart tvs plart ++ ")"
renderMember tvs _ (CallSignature plart) =
  "TS.Call (" ++ renderPlart tvs plart ++ ")"
renderMember tvs tyref (ConstructSignature mtparams params mresult) =
  "TS.Constructor (" ++
  -- FIXME: is it correct to use the type head for the default
  -- constructor return type?
  renderFunctionType tvs mtparams params (fromMaybe (TypeReference tyref) mresult) ++
  ")"
-- FIXME: should name be used? doesn't seem needed
renderMember tvs _ (TypeIndexSignature (IndexSignature _name String ty)) =
  "TS.StringIndex (" ++ renderType tvs ty ++ ")"
renderMember tvs _ (TypeIndexSignature (IndexSignature _name Number ty)) =
  "TS.NumericIndex (" ++ renderType tvs ty ++ ")"

renderPlart :: TyVars -> ParameterListAndReturnType -> String
renderPlart tvs (ParameterListAndReturnType mtparams params mreturn) =
  renderFunctionType tvs mtparams params (maybeVoid mreturn)

standardImports :: [String]
standardImports =
  [ "import qualified GHCJS.TypeScript as TS"
  , "import GHCJS.TypeScript (type (:|:), type (:::), type (::?))"
  , "import qualified GHCJS.Marshal as GHCJS"
  , "import qualified GHCJS.Types as GHCJS"
  , "import qualified Data.Typeable"
  ] ++ map addType
  [ "EventListenerOrEventListenerObject" ]

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

newtype TyVars = TyVars { unTyVars :: S.Set String }
  deriving (Monoid)

paramsToTyVars :: [TypeParameter] -> TyVars
paramsToTyVars = TyVars . S.fromList . map (\(TypeParameter name _) -> name)

renderType :: TyVars -> Type -> String
renderType tvs ty = case ty of
  Predefined AnyType -> "TS.Any"
  Predefined NumberType -> "TS.Number"
  Predefined BooleanType -> "TS.Boolean"
  Predefined StringType -> "TS.String"
  Predefined VoidType -> "TS.Void"
  TypeReference ref -> renderTypeRef tvs ref
  ObjectType body -> unlines $ "TS.Obj" : renderTypeBody tvs (error "should be unused") body
  ArrayType ty -> "(TS.Array " ++ renderType tvs ty ++ ")"
  UnionType t1 t2 -> "(" ++ renderType tvs t1 ++ " :|: " ++ renderType tvs t2 ++ ")"
  TupleType ts -> "(" ++ intercalate ", " (map (renderType tvs) ts) ++ ")"
  FunctionType mtparams params result ->
    "(" ++ renderFunctionType tvs mtparams params result ++ ")"
  ConstructorType mtparams params result ->
    "TS.Obj '[TS.Constructor " ++
    renderFunctionType tvs mtparams params result ++
    "]"
  tq@(TypeQuery{}) ->
    trace ("Skipping " ++ show tq) "TS.Any"

renderTypeRef :: TyVars -> TypeRef -> String
renderTypeRef tvs (TypeRef name Nothing) =
    renderTypeName tvs name
renderTypeRef tvs (TypeRef name (Just params)) =
    "(" ++
    renderTypeName tvs name ++
    concatMap (\ty -> " (" ++ renderType tvs ty ++ ")") params ++
    ")"

renderTypeName :: TyVars -> TypeName -> String
-- FIXME: use module qualification
renderTypeName tvs (TypeName mn name)
  | isNothing mn && name `S.member` (unTyVars tvs) = mungeLowerName name
  | otherwise = mungeUpperName name

renderFunctionType
    :: TyVars
    -> Maybe [TypeParameter]
    -> [Parameter]
    -> Type
    -> String
renderFunctionType tvs Nothing params result =
  concatMap ((++ " -> ") . renderParameter tvs) params ++ renderType tvs result
renderFunctionType tvs (Just tparams) params result =
  "TS.Any {- forall " ++
  intercalate " " (map renderTypeParameterName tparams) ++
  ". " ++
  renderTypeParametersContext tvs' tparams ++
  renderFunctionType tvs' Nothing params result ++
  " -}"
  where
    tvs' = tvs <> paramsToTyVars tparams

renderParameter :: TyVars -> Parameter -> String
renderParameter tvs (RequiredOrOptionalParameter _ _ optional mtype) =
  renderOptional optional (renderParameterType tvs mtype)
renderParameter tvs (RestParameter _ mtype) =
  "TS.Rest (" ++ renderType tvs (maybeAny mtype) ++ ")"

renderParameterType :: TyVars -> Maybe ParameterType -> String
renderParameterType tvs Nothing = renderType tvs (Predefined AnyType)
renderParameterType tvs (Just (ParameterType ty)) = renderType tvs ty
renderParameterType tvs (Just (ParameterSpecialized str)) = "TS.Specialize " ++ show str

renderOptional :: Maybe Optional -> String -> String
renderOptional Nothing xs = xs
renderOptional (Just Optional) xs = "TS.Optional (" ++ xs ++ ")"

optionalPrefix :: Maybe Optional -> String
optionalPrefix (Just Optional) = "Optional"
optionalPrefix Nothing = ""

renderTypeParameterName :: TypeParameter -> String
renderTypeParameterName (TypeParameter name _) = mungeLowerName name

renderTypeParametersContext :: TyVars -> [TypeParameter] -> String
renderTypeParametersContext tvs tparams =
  case mapMaybe (\(TypeParameter name mty) -> (name, ) <$> mty) tparams of
    [] -> ""
    constraints ->
      "(" ++
      intercalate ", " (map (\(name, ty) -> mungeLowerName name ++ " TS.:= " ++ renderType tvs ty) constraints) ++
      ") => "
