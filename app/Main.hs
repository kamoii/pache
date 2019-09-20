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
    main_ [ style [ ("height", "95vh"), ("display", "grid"), ("grid-template-rows", "auto 1fr") ] ]
      [ h1 [ style [ ("grid-row", "1") ] ] [ text "fooo" ]
      , div [ style [ ("grid-row", "2"), ("overflow", "hidden"), ("display", "grid"), ("grid-template-columns", "40% 60%") ] ]
        [ div [ style [ ("grid-column", "1"), ("overflow", "scroll") ] ] [ treeView renderer (toTree ast) ]
        , div [ style [ ("grid-column", "2"), ("overflow", "scroll") ] ] [ sourceView $ decodeUtf8 $ hie_hs_src hieFile ]
        ]
      ]
  where
    toTree :: HieAST i -> Tree (HieAST i)
    toTree ast = Data.Tree.Node ast (map toTree (nodeChildren ast))

    renderer :: HieAST i -> Bool -> Widget HTML v
    renderer ast _ = do
      let span = show $ nodeSpan ast
      let anot = show $ nodeAnnotations $ nodeInfo ast
      div [] [ text anot ]

sourceView
  :: Text
  -> Widget HTML v
sourceView src = do
 div [ style [ ("white-space", "pre"), ("font-family", "monospace") ] ] [ text src ]

{-
親に戻る時にスクロール位置が一番上に戻ってしまう...
上の階層のdivを消さずに div を重ねるていくほうがいいのかな？
-}
treeView
  :: (forall v. a -> Bool -> Widget HTML v)
  -> Tree a
  -> Widget HTML v
treeView renderer root = render (TZ.zipper root)
  where
    renderer' tz isSelected = do
      div [ style [("border", "1px solid gray"), ("margin-bottom", "0.5rem")]]
        [ renderer (TZ.current tz) isSelected
        , whenJustA (TZ.downToFirstChild tz) $ \ctz -> button [ ctz <$ onClick ] [ text "cildren" ]
        ]

    render zp = do
      let before = reverse $ unfoldr (dupe <<$>> TZ.siblingBefore) zp
      let after  = unfoldr (dupe <<$>> TZ.siblingAfter) zp
      let all    = zip before (repeat False) <> [(zp, True)] <> zip after (repeat False)
      zp' <- div []
        [ whenJustA (TZ.up zp) $ \ctz -> button [ ctz <$ onClick ] [ text "parent" ]
        , div [] (map (uncurry renderer') all)
        ]
      render zp'

-- https://gitlab.haskell.org/ghc/ghc/wikis/hie-files#reading-hie-files
makeNc :: IO NameCache
makeNc = do
  uniqSupply <- mkSplitUniqSupply 'z'
  return $ initNameCache uniqSupply []

whenJustA :: Alternative m => Maybe a -> (a -> m v) -> m v
whenJustA m f = case m of
  Just a -> f a
  Nothing -> empty

-- Local Variables:
-- dante-repl-command-line: ("cabal" "new-repl" "exe:pache" "--allow-newer")
-- eval: (dante-mode)
-- eval: (flycheck-mode)
-- End:
