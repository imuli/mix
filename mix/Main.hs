{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Bits ((.&.), (.|.))
import           Data.Foldable
import           Data.Hashable
import           Data.List (intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, isNothing)
import           Data.Set (Set)
import qualified Data.Set as S
import           Mix.Modules
import           Mix.Util
import           Numeric (showHex)
import           System.Directory
import           System.Environment
import           System.IO

data Config = Config
   { confBaseDirs :: [FilePath]
   }

defConf :: Config
defConf = Config
  { confBaseDirs = ["."]
  }

usage :: IO ()
usage = do
  name <- getProgName
  hPutStrLn stderr $ "Usage: " <> name <> " " <> "{cmd}…"
  hPutStrLn stderr   "  inc {paths} {cmd}… # add colon-seperated path list search list, then run cmd"
  hPutStrLn stderr   "  dot {files}        # print out a graphviz dot file for module dependancies"
  hPutStrLn stderr   "  nix {files}        # print out nix expressions for building files"

run :: Config -> [String] -> IO ()
run _   []                   = pure ()
run cnf ("inc" : paths : xs) = run (cnf { confBaseDirs = confBaseDirs cnf <> split ':' paths }) xs
run cnf ("dot" : xs)         = let xs' = S.fromList xs in putDot cnf xs' =<< foldM (doModule cnf) mempty xs'
run cnf ("nix" : xs)         = let xs' = S.fromList xs in putNix cnf xs' =<< foldM (doModule cnf) mempty xs'
run _ _                      = usage

doModule :: Config -> M.Map FilePath (Set FilePath) -> FilePath -> IO (M.Map FilePath (Set FilePath))
doModule cnf done hs = do
  mods <- getModules hs
  let modPaths = concatMap (modulePaths $ confBaseDirs cnf) mods
  presentModules <- S.fromList <$> filterM doesFileExist modPaths
  let done' = M.insert hs presentModules done
      nextModules = S.filter (isNothing . flip M.lookup done') presentModules
  foldM (doModule cnf) done' nextModules

putDot :: Config -> (Set FilePath) -> M.Map FilePath (Set FilePath) -> IO ()
putDot _ toplevel deps = do
  putStrLn    "digraph mix {"
  mapM_ (\(x,ys) -> do
    putStrLn $ show x <> nodeAttrs toplevel x ys <> ";"
    mapM_ (\y -> putStrLn $ show x <> " -> " <> show y <> ";") ys
    ) (M.toList deps)
  putStrLn    "}"

putNix :: Config -> (Set FilePath) -> M.Map FilePath (Set FilePath) -> IO ()
putNix _ _ depGraph = do
  let (depTrans, _) = foldl' (transitiveClosure depGraph . fst) (mempty, mempty) (M.keys depGraph)
  putStrLn "{"
  mapM_ (\(x, ys) -> putStrLn $ "  " <> show x <> " = { transDeps = [" <> intercalate " " (show <$> toList ys) <> "]; };") (M.toList depTrans)
  putStrLn "}"

transitiveClosure :: forall a. Ord a
                  => Map a (Set a) -- ^ input graph
                  -> Map a (Set a) -- ^ current state of search
                  -> a             -- ^ current item
                  -> (Map a (Set a), Set a) -- ^ transative closure for 'x' and new state of search
transitiveClosure graph done x = fromMaybe (M.insert x S.empty done, S.empty) $ present <|> notPresent
  where
    present = (done,) <$> M.lookup x done
    notPresent = withDeps <$> M.lookup x graph
    withDeps :: Set a -> (Map a (Set a), Set a)
    withDeps deps = let (done', ys) = foldl' updateFromChild (done, deps) deps
                     in (M.insert x ys done', ys)
    updateFromChild :: (Map a (Set a), Set a) -> a -> (Map a (Set a), Set a)
    updateFromChild (cur, deps) = fmap (deps <>) . transitiveClosure graph cur

mkColor :: Hashable a => a -> String
mkColor = ('#':) . flip showHex "" . (0x808080 .|.) . (0xffffff .&.) . hash

nodeAttrs :: (Set FilePath) -> FilePath -> (Set FilePath) -> String;
nodeAttrs top x deps | null deps = "[color=" <> show (mkColor x) <> "]"
                     | S.member x top = "[color=yellow]"
                     | otherwise = ""

main :: IO ()
main = do
  args <- getArgs
  run defConf args
