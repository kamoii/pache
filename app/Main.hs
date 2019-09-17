module Main where

import Prelude()
import Relude
import HieBin
import HieTypes
import NameCache (NameCache, initNameCache)
import UniqSupply (mkSplitUniqSupply)
import System.Environment (getArgs)
import qualified Data.Map as M

main :: IO ()
main = do
  [hiePath] <- getArgs
  nc <- makeNc
  (hieFileResult, nc') <- readHieFile nc hiePath
  let hieFile = hie_file_result hieFileResult
  let filePath = hie_hs_file hieFile
  let asts = hie_asts hieFile
  putStrLn $ "hieFile read!! from: " <> filePath
  print $ M.keys $ getAsts asts

-- https://gitlab.haskell.org/ghc/ghc/wikis/hie-files#reading-hie-files
makeNc :: IO NameCache
makeNc = do
  uniqSupply <- mkSplitUniqSupply 'z'
  return $ initNameCache uniqSupply []

-- Local Variables:
-- dante-target: "exe:pache"
-- eval: (dante-mode)
-- eval: (flycheck-mode)
-- End:
