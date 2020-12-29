module Mix.Modules where

import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Syntax (ImportDecl(..), ModuleName(..))
import           Mix.Util

newtype Module = Module [String]
  deriving (Eq, Ord, Read, Show)

-- | List of possible module extensions.
moduleExts :: [String]
moduleExts = [".hs", ".lhs"]

-- | Enumerate possible module paths.
modulePaths :: [FilePath] -> Module -> [FilePath]
modulePaths bases (Module xs) = fmap (<>) bases <*> fmap addPath moduleExts
  where addPath = (concat (('/':) <$> xs) <>)

-- | Parse imported modules from a file.
getModules :: FilePath -> IO [Module]
getModules hs = do
  src <- readFile hs
  case modules src of
       ParseFailed _ err -> fail $ "Reading " <> hs <> " failed: " <> err
       ParseOk mods      -> pure mods

-- | Convert a dotty module string to a Module
toModule :: ModuleName l -> Module
toModule = Module . split '.' . moduleString

-- | Parse a list of modules out of a string holding the header of a haskell
-- source file.
modules :: String -> ParseResult [Module]
modules src = fmap (toModule . importModule) <$> imports src

-- | Parse imports into a module list.
imports :: String -> ParseResult [ImportDecl SrcSpanInfo]
imports x = mhaiImports . unNonGreedy <$> parse x

mhaiImports :: ModuleHeadAndImports l -> [ImportDecl l]
mhaiImports (ModuleHeadAndImports _ _ _ x) = x

moduleString :: ModuleName l -> String
moduleString (ModuleName _ x) = x
