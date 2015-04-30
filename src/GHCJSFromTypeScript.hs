module GHCJS.FromTypeScript where

import           Control.Applicative ((<$>))
import           Control.Monad (when)
import           Control.Monad.State (State, state, evalState)
import           Data.Char (isAlpha, toLower)
import           Data.Foldable (forM_)
import           Data.List (intercalate)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Monoid(..), (<>))
import           Data.String.Utils (replace)
import           Data.Typeable
import           Language.TypeScript
import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.FilePath ((</>), splitFileName)
import           Text.Parsec (parse)

data Config = Config
  { outputDir :: FilePath
  , extraImports :: [String]
  , rewriteTypes :: Type -> Type
  }

main :: IO ()
main = ghcjsFromTypeScript "ace.d.ts" Config
  { outputDir = "output"
  , extraImports = ["import GHCJS.DOM.Types (HTMLElement)"]
  , rewriteTypes = defaultRewriteTypes
  }

ghcjsFromTypeScript :: FilePath -> Config -> IO ()
ghcjsFromTypeScript fp config = do
  addTypeScriptModule config
  mapM_ (processDeclaration config) =<< parseTypeScript fp

parseTypeScript :: FilePath -> IO [DeclarationElement]
parseTypeScript fp =
  either (fail . show) return .
  parse declarationSourceFile fp .
  replace "export " "" =<<
  readFile fp

processDeclaration :: Config -> DeclarationElement -> IO ()
processDeclaration config (AmbientDeclaration _ _ ambient) = do
  processAmbientDeclaration config (ModuleName ["GHCJS", "FFI"]) ambient
processDeclaration _ x = notSupported x

processAmbientDeclaration :: Config -> ModuleName -> Ambient -> IO ()
processAmbientDeclaration config mn (AmbientModuleDeclaration _ names decls) = do
  let prefix = mn <> ModuleName names
  startModule config (prefix <> ModuleName ["Raw", "Types"])
  mapM_ (processAmbientDeclaration config prefix) decls
processAmbientDeclaration config mn (AmbientInterfaceDeclaration iface) =
  processInterface config mn iface
processAmbientDeclaration _ _ x = notSupported x

processInterface :: Config -> ModuleName -> Interface -> IO ()
processInterface config prefix (Interface _ name typeParams typeRefs body) = do
  let mn = prefix <> ModuleName ["Raw", name]
      typesMn = prefix <> ModuleName ["Raw", "Types"]
  startModule config mn
  appendLine config mn $ "import " ++ renderModuleName typesMn ++ "\n"
  appendOutput config typesMn $ unlines
    [ "data " ++ name ++ "_"
    , "type " ++ name ++ " = GHCJS.JSRef " ++ name ++ "_"
    , ""
    ]
  forM_ (getMembers body) $ \(hsName, member) ->
    case member of
      PropertySignature field _ mtyp ->
        foreignImport
          config
          mn
          ("$1." ++ field)
          (hsName ++ " :: " ++ name ++ " -> " ++ renderType config (maybeAny mtyp))
      --TODO: handle optional
      MethodSignature field optional (ParameterListAndReturnType Nothing params mresult) ->
        foreignImport
          config
          mn
          ("$1." ++ field ++ "(" ++ intercalate "," (zipWith (\ix _ -> "$" ++ show ix) [2..] params) ++ ")")
          (hsName ++ " :: " ++ name ++ " -> " ++ renderFunctionType config params (maybeAny mresult))
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
   return $ "GHCJS.JSRef (" ++ result ++ ")"
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
renderTypeRef config (TypeRef name Nothing) = renderTypeName config name
renderTypeRef _ x = notSupported x

renderTypeName :: Config -> TypeName -> String
renderTypeName config (TypeName Nothing name) = name
renderTypeName _ x = notSupported x

--------------------------------------------------------------------------------
-- Typescript type rewriting

defaultRewriteTypes :: Type -> Type
defaultRewriteTypes x = x

--------------------------------------------------------------------------------
-- Output

foreignImport :: Config -> ModuleName -> String -> String -> IO ()
foreignImport config mn js decl = appendOutput config mn $
  "foreign import javascript " ++ show js ++ " " ++ decl ++ "\n"

startModule :: Config -> ModuleName -> IO ()
startModule config mn = do
  let path = modulePath config mn
  exists <- doesFileExist path
  when exists $ fail $ path ++ " already exists."
  appendOutput config mn $ unlines $
    [ "module " ++ renderModuleName mn ++ " where"
    , "import qualified GHCJS.Types as GHCJS"
    , "import GHCJS.FFI.TypeScript"
    ] ++ extraImports config

appendTypes :: Config -> ModuleName -> String -> IO ()
appendTypes config prefix =
  appendOutput config (prefix <> ModuleName ["Types"])

appendLine :: Config -> ModuleName -> String -> IO ()
appendLine config mn line = appendOutput config mn (line ++ "\n")

appendOutput :: Config -> ModuleName -> String -> IO ()
appendOutput config mn output = do
  let path = modulePath config mn
      (dir, _) = splitFileName path
  createDirectoryIfMissing True dir
  appendFile path output

modulePath :: Config -> ModuleName -> FilePath
modulePath config mn = outputDir config </> moduleNamePath mn

--------------------------------------------------------------------------------
-- Utilities

-- TODO: Are the usages of this correct?  Couldn't easily find docs on
-- what it means if the type is missing in various contexts.
maybeAny :: Maybe Type -> Type
maybeAny = fromMaybe (Predefined AnyType)

notSupported :: (Show a, Typeable a) => a -> b
notSupported x = error $ show (typeOf x) ++ " not yet supported: " ++ show x

instance Monoid ModuleName where
    mempty = ModuleName mempty
    mappend (ModuleName xs) (ModuleName ys) = ModuleName (xs ++ ys)

renderModuleName :: ModuleName -> String
renderModuleName (ModuleName xs) = intercalate "." xs

moduleNamePath :: ModuleName -> FilePath
moduleNamePath (ModuleName xs) = intercalate "/" xs ++ ".hs"

addTypeScriptModule :: Config -> IO ()
addTypeScriptModule config = do
  let mn = ModuleName ["GHCJS", "FFI", "TypeScript"]
  appendOutput config mn $ "\
    \module GHCJS.FFI.TypeScript where\n\
    \import GHCJS.Types (JSRef)\n\
    \\n\
    \data Function_\n\
    \type Function = JSRef Function_"
