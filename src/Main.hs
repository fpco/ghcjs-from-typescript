{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import           Control.Applicative ((<$>))
import           Control.Lens (makeLenses, over)
import           Control.Monad (when)
import           Control.Monad.State.Strict hiding (forM_)
import           Data.Char (isAlpha, toLower, toUpper)
import           Data.Foldable (forM_)
import           Data.List (intercalate)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Monoid(..), (<>))
import qualified Data.Set as S
import           Data.String.Utils (replace)
import           Data.Typeable
import           Language.TypeScript
import           System.Directory (createDirectoryIfMissing, doesFileExist, copyFile)
import           System.FilePath ((</>), splitFileName)
import           Text.Parsec (parse)

data Config = Config
  { outputDir :: FilePath
  , extraImports :: [String]
  , rewriteTypes :: Type -> Type
  }

type M = StateT Output IO

data Output = Output
  { _outputModules :: S.Set ModuleName
  }

$(makeLenses ''Output)

emptyOutput :: Output
emptyOutput = Output
  { _outputModules = S.empty
  }

main :: IO ()
main = do
  let config = Config
        { outputDir = "ghcjs-typescript"
        , extraImports = []
        , rewriteTypes = defaultRewriteTypes
        }
  output <- ghcjsFromTypeScript "typescript.d.ts" config
  writeCabalFile config output
  copyFile "LICENSE" "ghcjs-typescript/LICENSE"

ghcjsFromTypeScript :: FilePath -> Config -> IO Output
ghcjsFromTypeScript fp config = do
  parsed <- parseTypeScript fp
  flip execStateT emptyOutput $ do
    addTypeScriptModule config
    mapM_ (processDeclaration config) parsed

parseTypeScript :: FilePath -> IO [DeclarationElement]
parseTypeScript fp =
  either (fail . show) return .
  parse declarationSourceFile fp .
  replace "export " "" =<<
  readFile fp

processDeclaration :: Config -> DeclarationElement -> M ()
processDeclaration config (AmbientDeclaration _ _ ambient) = do
  processAmbientDeclaration config (ModuleName ["JavaScript"]) ambient
processDeclaration _ x = notSupported x

processAmbientDeclaration :: Config -> ModuleName -> Ambient -> M ()
processAmbientDeclaration config mn (AmbientModuleDeclaration _ names decls) = do
  let prefix = mn <> ModuleName names
  startModule config (prefix <> ModuleName ["Raw", "Types"])
  mapM_ (processAmbientDeclaration config prefix) decls
processAmbientDeclaration config mn (AmbientExternalModuleDeclaration _ name xs) = do
  let prefix = mn <> ModuleName [capitalize name]
      go (AmbientModuleElement ambient) = processAmbientDeclaration config prefix ambient
      go x = notSupported x
  startModule config (prefix <> ModuleName ["Raw", "Types"])
  mapM_ go xs
processAmbientDeclaration config mn (AmbientInterfaceDeclaration iface) =
  processInterface config mn iface
processAmbientDeclaration config mn (AmbientEnumDeclaration _ _ xs) =
  --FIXME: pattern synonym based output
  return ()
processAmbientDeclaration config mn (AmbientClassDeclaration cp name typarams tyrefs1 tyrefs2 body) =
  --TODO: what are tyrefs1 / tyrefs2?
  processInterface config mn $
    Interface cp name typarams tyrefs1 (TypeBody (map (\(cp, cbe) -> (cp, classBodyElementToTypeMember cbe)) body))
-- processAmbientDeclaration config mn (AmbientVariableDeclaration _ name ty) =
processAmbientDeclaration _ _ x = skip x

classBodyElementToTypeMember :: AmbientClassBodyElement -> TypeMember
classBodyElementToTypeMember (AmbientConstructorDeclaration params) =
  ConstructSignature Nothing params Nothing
classBodyElementToTypeMember (AmbientMemberDeclaration mpublicOrPrivate mstatic name fieldOrFunc) =
  case fieldOrFunc of
    Left field -> PropertySignature name Nothing field
    Right func -> MethodSignature name Nothing func
classBodyElementToTypeMember (AmbientIndexSignature indexSig) =
  TypeIndexSignature indexSig

processInterface :: Config -> ModuleName -> Interface -> M ()
processInterface config prefix (Interface _ name typeParams typeRefs body) = do
  let mn = prefix <> ModuleName ["Raw", name]
      typesMn = prefix <> ModuleName ["Raw", "Types"]
  startModule config mn
  appendLine config mn $ "import " ++ renderModuleName typesMn ++ "\n"
  appendOutput config typesMn $
    "newtype " ++ name ++ " = " ++ name ++ " (GHCJS.JSRef "  ++ name ++ ")\n" ++
    "  deriving (Data.Typeable.Typeable, GHCJS.ToJSRef, GHCJS.FromJSRef)\n"
  forM_ (getMembers body) $ \(hsName, member) ->
    case member of
      PropertySignature field _ mtyp ->
        foreignImport
          config
          mn
          ("$1." ++ field)
          (hsName ++ " :: " ++ name ++ " -> IO (" ++ renderType config (maybeAny mtyp) ++ ")")
      --TODO: handle optional
      MethodSignature field optional (ParameterListAndReturnType _ params mresult) ->
        foreignImport
          config
          mn
          ("$" ++ thisIx ++ "." ++ field ++ "(" ++ intercalate "," (zipWith (\ix _ -> "$" ++ show ix) ixs params) ++ ")")
          (hsName ++ " :: " ++ renderFunctionType config params' (maybeVoid mresult))
        where
          thisBack = False -- TODO: make this configureable
          thisIx = if thisBack then show (length params + 1) else "1"
          ixs = if thisBack then [1..] else [2..]
          params' = if thisBack then params ++ [thisParam] else thisParam : params
          -- Create a parameter for passing in 'this'
          thisParam = RequiredOrOptionalParameter Nothing "" Nothing (Just thisType)
          thisType = TypeReference $ TypeRef (TypeName Nothing name) Nothing
      _ -> notSupported member

getMembers :: TypeBody -> [(String, TypeMember)]
getMembers (TypeBody xs) =
    go M.empty xs
  where
    go _ [] = []
    go mp ((_, x@(PropertySignature field _ _)):xs) =
      (sanitizeName field, x) : go mp xs
    go mp ((_, x@(MethodSignature field _ _)):xs) =
        --FIXME: this could still yield names that alias
        case M.lookup field mp of
          Nothing -> addMethod (sanitizeName field) (M.insert field 1 mp)
          Just ix -> addMethod (sanitizeName field ++ show ix) (M.insert field (ix + 1) mp)
      where
        addMethod name mp' = (name, x) : go mp' xs
    -- go mp ((_ , x@(TypeIndexSignature name input result))) =
    go _ (x:xs) = notSupported x

--FIXME: Removing special chars can cause names to alias.
sanitizeName :: String -> String
sanitizeName = defaultForEmpty . avoidKeywords . onlyValidIdentifiers
  where
    defaultForEmpty "" = "_defaultForEmpty_"
    defaultForEmpty x = x
    onlyValidIdentifiers [] = []
    onlyValidIdentifiers (x:xs)
      | isAlpha x = toLower x : onlyValidIdentifiers' xs
      | otherwise = onlyValidIdentifiers xs
    onlyValidIdentifiers' [] = []
    onlyValidIdentifiers' (x:xs)
      | isAlpha x = x : onlyValidIdentifiers' xs
      | otherwise = onlyValidIdentifiers' xs

--FIXME: If both "type" and "type_" exist, then this will cause the
--first to alias the second, erroneously.
avoidKeywords :: String -> String
avoidKeywords x | x `elem` keywords = x ++ "_"
avoidKeywords x = x

-- https://wiki.haskell.org/Keywords
keywords :: [String]
keywords =
  [ "as"
  , "case"
  , "of"
  , "class"
  , "data"
  , "default"
  , "do"
  , "forall"
  , "foreign"
  , "hiding"
  , "if"
  , "then"
  , "else"
  , "import"
  , "infix"
  , "infixl"
  , "infixr"
  , "instance"
  , "let"
  , "in"
  , "mdo"
  , "module"
  , "newtype"
  , "proc"
  , "qualified"
  , "rec"
  , "type"
  , "where"
  ]

--------------------------------------------------------------------------------
-- Render types

-- TODO: make sure the type variable generation for AnyType doesn't
-- alias other type variables (once those are supported).

renderType :: Config -> Type -> String
renderType config typ =
  evalState (renderType' config typ) 0

renderFunctionType :: Config -> [Parameter] -> Type -> String
renderFunctionType config params result =
  evalState (renderFunctionType' config params result) 0

renderType' :: Config -> Type -> State Int String
renderType' config typ = case rewriteTypes config typ of
  Predefined AnyType -> do
    i <- state (\i -> (i, i+1))
    return $ "GHCJS.JSRef obj" ++ show i
  Predefined NumberType -> return "GHCJS.JSNumber"
  Predefined BooleanType -> return "GHCJS.JSBool"
  Predefined StringType -> return "GHCJS.JSString"
  Predefined VoidType -> return "()"
  TypeReference ref -> return $ renderTypeRef config ref
  ArrayType ty -> return $ "GHCJS.JSArray (" ++ renderType config ty ++ ")"
  FunctionType Nothing params result -> do
   result <- renderFunctionType' config params result
   return $ "GHCJS.JSFun (" ++ result ++ ")"
  _ -> notSupported typ

renderFunctionType' :: Config -> [Parameter] -> Type -> State Int String
renderFunctionType' config params result = do
  rparams <- mapM (\param -> (\r -> "(" ++ r ++ ") -> ") <$> renderParamType config param) params
  rresult <- renderType' config result
  return $ concat rparams ++ "IO (" ++ rresult ++ ")"

renderParamType :: Config -> Parameter -> State Int String
renderParamType config (RequiredOrOptionalParameter Nothing _ _ mtyp) =
  renderType' config (maybeAny mtyp)
renderParamType _ x = notSupported x

renderTypeRef :: Config -> TypeRef -> String
renderTypeRef config (TypeRef name Nothing) =
    renderTypeName config name
renderTypeRef config (TypeRef name (Just params)) =
    "(" ++
    renderTypeName config name ++
    concatMap (\ty -> " (" ++ renderType config ty ++ ")") params ++
    ")"

renderTypeName :: Config -> TypeName -> String
renderTypeName config (TypeName Nothing name) = name
renderTypeName _ x = notSupported x

--------------------------------------------------------------------------------
-- Typescript type rewriting

defaultRewriteTypes :: Type -> Type
defaultRewriteTypes x = x

--------------------------------------------------------------------------------
-- Output

foreignImport :: Config -> ModuleName -> String -> String -> M ()
foreignImport config mn js decl = appendOutput config mn $
  "foreign import javascript " ++ show js ++ " " ++ decl ++ "\n"

startModule :: Config -> ModuleName -> M ()
startModule config mn = do
  let path = modulePath config mn
  exists <- liftIO $ doesFileExist path
  unless exists $
    appendOutput config mn $ unlines $
      [ "module " ++ renderModuleName mn ++ " where"
      , "import qualified GHCJS.Types as GHCJS"
      , "import qualified GHCJS.Marshal as GHCJS"
      , "import qualified Data.Typeable"
      , "import GHCJS.FFI.TypeScript"
      ] ++ extraImports config
  modify (over outputModules $ S.insert mn)

appendTypes :: Config -> ModuleName -> String -> M ()
appendTypes config prefix =
  appendOutput config (prefix <> ModuleName ["Types"])

appendLine :: Config -> ModuleName -> String -> M ()
appendLine config mn line = appendOutput config mn (line ++ "\n")

appendOutput :: Config -> ModuleName -> String -> M ()
appendOutput config mn output = liftIO $ do
  let path = modulePath config mn
      (dir, _) = splitFileName path
  createDirectoryIfMissing True dir
  appendFile path output

modulePath :: Config -> ModuleName -> FilePath
modulePath config mn = outputDir config </> moduleNamePath mn

--------------------------------------------------------------------------------
-- Utilities

-- TODO: Are the usages of this and maybeVoid correct?  Couldn't
-- easily find docs on what it means if the type is missing in various
-- contexts.
maybeAny :: Maybe Type -> Type
maybeAny = fromMaybe (Predefined AnyType)

maybeVoid :: Maybe Type -> Type
maybeVoid = fromMaybe (Predefined VoidType)

skip :: (Show a, MonadIO m) => a -> m ()
skip x = liftIO $ putStrLn $ "Skipping " ++ show x

notSupported :: (Show a, Typeable a) => a -> b
notSupported x = error $ show (typeOf x) ++ " not yet supported: " ++ show x

deriving instance Eq ModuleName
deriving instance Ord ModuleName

instance Monoid ModuleName where
    mempty = ModuleName mempty
    mappend (ModuleName xs) (ModuleName ys) = ModuleName (xs ++ ys)

renderModuleName :: ModuleName -> String
renderModuleName (ModuleName xs) = intercalate "." xs

moduleNamePath :: ModuleName -> FilePath
moduleNamePath (ModuleName xs) = intercalate "/" xs ++ ".hs"

capitalize :: String -> String
capitalize [] = []
capitalize (c:xs) = toUpper c : xs

addTypeScriptModule :: Config -> M ()
addTypeScriptModule config = do
  let mn = ModuleName ["GHCJS", "FFI", "TypeScript"]
  appendOutput config mn $ "\
    \module GHCJS.FFI.TypeScript where\n\
    \import GHCJS.Types (JSRef)\n\
    \\n\
    \data Function_\n\
    \type Function = JSRef Function_"

writeCabalFile :: Config -> Output -> IO ()
writeCabalFile config Output {..} =
  writeFile (outputDir config </> "ghcjs-ace.cabal") $
    "name:                ghcjs-ace\n\
    \version:             0.0.0\n\
    \synopsis:            Bindings to the ace editor\n\
    \description:\n\
    \license:             BSD3\n\
    \license-file:        LICENSE\n\
    \author:              FP Complete\n\
    \maintainer:          sloan@fpcomplete.com\n\
    \copyright:           (c) 2015 FP Complete\n\
    \category:            Web\n\
    \build-type:          Simple\n\
    \extra-source-files:  README.md\n\
    \cabal-version:       >=1.10\n\
    \\n\
    \library\n\
    \  exposed-modules:" ++
    concatMap (('\n':replicate 22 ' ') ++) (map renderModuleName (S.toList _outputModules)) ++ "\n" ++
    "  build-depends:      base >= 4 && < 5,\n\
    \                      ghcjs-base,\n\
    \                      ghcjs-dom\n\
    \  default-language:   Haskell2010\n\
    \  default-extensions: DeriveDataTypeable\n\
    \                      ForeignFunctionInterface\n\
    \                      GeneralizedNewtypeDeriving\n\
    \                      JavaScriptFFI\n"
