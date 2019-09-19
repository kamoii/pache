{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Prelude()
import Relude hiding (div)
import Relude.Extra.Tuple (dupe)
import HieBin
import HieTypes
import NameCache (NameCache, initNameCache)
import UniqSupply (mkSplitUniqSupply)
import System.Environment (getArgs)
import Data.Tree (Tree(Node))
import qualified Data.Map as M
import Concur.Core
import Concur.Replica
import Concur.Replica.Run (runDefault)
import qualified Data.TreeZipper as TZ

main :: IO ()
main = do
  [hiePath] <- getArgs
  main' hiePath

main' hiePath = do
  nc <- makeNc
  (hieFileResult, nc') <- readHieFile nc hiePath
  let hieFile = hie_file_result hieFileResult
  let filePath = hie_hs_file hieFile
  let asts = hie_asts hieFile
  let [ast] = M.elems $ getAsts asts
  putStrLn $ "hieFile read!! from: " <> filePath
  print $ M.keys $ getAsts asts
  runDefault 8080 "hie explorer" $ do
    main_ []
      [ h1 [] [ text "fooo" ]
      , div [] [ treeView renderer (toTree ast) ]
      ]
  where
    toTree :: HieAST i -> Tree (HieAST i)
    toTree ast = Data.Tree.Node ast (map toTree (nodeChildren ast))

    renderer :: HieAST i -> Bool -> Widget HTML v
    renderer ast _ = do
      div [] [ text "test" ]

treeView
  :: (forall v. a -> Bool -> Widget HTML v)
  -> Tree a
  -> Widget HTML v
treeView renderer root = render (TZ.zipper root)
  where
    renderer' tz isSelected = do
      renderer (TZ.current tz) isSelected

    render zp = do
      let before = reverse $ unfoldr (dupe <<$>> TZ.siblingBefore) zp
      let after  = unfoldr (dupe <<$>> TZ.siblingAfter) zp
      let all    = zip before [False ..] <> ((zp, True) : zip after [False ..])
      orr $ map (uncurry renderer') all

-- https://gitlab.haskell.org/ghc/ghc/wikis/hie-files#reading-hie-files
makeNc :: IO NameCache
makeNc = do
  uniqSupply <- mkSplitUniqSupply 'z'
  return $ initNameCache uniqSupply []

-- Local Variables:
-- dante-repl-command-line: ("cabal" "new-repl" "exe:pache" "--allow-newer")
-- eval: (dante-mode)
-- eval: (flycheck-mode)
-- End:
