module GHCJS.FromTypeScript.Collect (collect) where

import qualified Data.Map as M
import           Data.Monoid
import           GHCJS.FromTypeScript.Types
import           GHCJS.FromTypeScript.Util
import           Language.TypeScript

collect :: [DeclarationElement] -> M.Map ModuleName (M.Map String Decl)
collect des =
  -- Move declaration name into an inner map.
  M.mapKeysWith M.union fst $
  M.map (uncurry M.singleton) $
  -- Combine declarations by concatenating their members.
  M.fromListWith (\(k, x) (_, y) -> (k, combine x y)) $
  map (\(mn, decl) -> ((mn, declName decl), (declName decl, decl))) $
  concatMap collectDeclarationElement des

combine :: Decl -> Decl -> Decl
combine (InterfaceDecl (Interface cp1 name mparams1 mrefs1 (TypeBody body1)))
        (InterfaceDecl (Interface cp2 _ mparams2 mrefs2 (TypeBody body2))) =
  -- FIXME: bring back this check once there's a good way to equality compare these
  --
  -- if mparams1 /= mparams2 || mrefs1 /= mrefs2
  --   then error $ "Mismatched signatures in declarations for " ++ name
  --   else
  InterfaceDecl $ Interface cp1 name mparams1 mrefs1 $ TypeBody $ body1 ++ body2

declName :: Decl -> String
declName (InterfaceDecl (Interface _ name _ _ _)) = name

collectDeclarationElement :: DeclarationElement -> [(ModuleName, Decl)]
collectDeclarationElement (AmbientDeclaration _ _ ambient) =
  collectAmbientDeclaration (ModuleName []) ambient
collectDeclarationElement (InterfaceDeclaration _ _ iface) =
  [collectInterface (ModuleName []) iface]
collectDeclarationElement x =
  notSupported x

collectAmbientDeclaration :: ModuleName -> Ambient -> [(ModuleName, Decl)]
collectAmbientDeclaration mn (AmbientModuleDeclaration _ names decls) =
  concatMap (collectAmbientDeclaration (mn <> ModuleName names)) decls
collectAmbientDeclaration mn (AmbientExternalModuleDeclaration _ name xs) =
  concatMap (collectAmbientExternalModuleElement (mn <> ModuleName [capitalize name])) xs
collectAmbientDeclaration mn (AmbientInterfaceDeclaration iface) =
  [collectInterface mn iface]
collectAmbientDeclaration mn (AmbientClassDeclaration cp name typarams tyrefs1 tyrefs2 body) =
  --TODO: what are tyrefs1 / tyrefs2?
  [collectInterface mn $
    Interface cp name typarams tyrefs1 (TypeBody (map (\(cp, cbe) -> (cp, classBodyElementToTypeMember cbe)) body))]
collectAmbientDeclaration _ x = skip x

collectAmbientExternalModuleElement :: ModuleName -> AmbientExternalModuleElement -> [(ModuleName, Decl)]
collectAmbientExternalModuleElement mn (AmbientModuleElement ambient) =
  collectAmbientDeclaration mn ambient
collectAmbientExternalModuleElement _ x = notSupported x

-- TODO: revisit spec - I'm thinking the interface for a class doesn't
-- have the class's constructor.
classBodyElementToTypeMember :: AmbientClassBodyElement -> TypeMember
classBodyElementToTypeMember (AmbientConstructorDeclaration params) =
  ConstructSignature Nothing params Nothing
classBodyElementToTypeMember (AmbientMemberDeclaration mpublicOrPrivate mstatic name fieldOrFunc) =
  case fieldOrFunc of
    Left field -> PropertySignature name Nothing field
    Right func -> MethodSignature name Nothing func
classBodyElementToTypeMember (AmbientIndexSignature indexSig) =
  TypeIndexSignature indexSig

collectInterface :: ModuleName -> Interface -> (ModuleName, Decl)
collectInterface mn iface = (mn, InterfaceDecl iface)
