module Main where

import           Control.Monad
import           Data.Bits ((.&.), (.|.))
import           Data.Foldable (find)
import           Data.Hashable
import qualified Data.Map.Strict as M
import           Data.Maybe (isJust, isNothing)
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
run cnf ("dot" : xs)         = putDot cnf xs =<< foldM (doModule cnf) mempty xs
run _ _                      = usage

doModule :: Config -> M.Map FilePath [FilePath] -> FilePath -> IO (M.Map FilePath [FilePath])
doModule cnf done hs = do
  mods <- getModules hs
  let modPaths = concatMap (modulePaths $ confBaseDirs cnf) mods
  presentModules <- filterM doesFileExist modPaths
  let done' = M.insert hs presentModules done
      nextModules = filter (isNothing . flip M.lookup done') presentModules
  foldM (doModule cnf) done' nextModules

putDot :: Config -> [FilePath] -> M.Map FilePath [FilePath] -> IO ()
putDot cnf toplevel deps = do
  putStrLn    "digraph mix {"
  mapM_ (\(x,ys) -> do
    putStrLn $ show x <> nodeAttrs toplevel x ys <> ";"
    mapM_ (\y -> putStrLn $ show x <> " -> " <> show y <> ";") ys
    ) (M.toList deps)
  putStrLn    "}"

mkColor :: Hashable a => a -> String
mkColor = ('#':) . flip showHex "" . (0x808080 .|.) . (0xffffff .&.) . hash

nodeAttrs :: [FilePath] -> FilePath -> [FilePath] -> String;
nodeAttrs _   x [] = "[color=" <> show (mkColor x) <> "]"
nodeAttrs top x _  | isJust $ find (== x) top = "[color=yellow]"
                   | otherwise = ""

main :: IO ()
main = do
  args <- getArgs
  run defConf args
