module Main where

import Prelude()
import Relude
import HieBin
import NameCache
import UniqSupply (mkSplitUniqSupply)
import System.Environment (getArgs)


main :: IO ()
main = do
  [hiePath] <- getArgs
  nc <- makeNc
  (hieFile, nc') <- readHieFile nc hiePath
  putStrLn "hieFile read!"

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
