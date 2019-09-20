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
import SrcLoc
import System.Environment (getArgs)
import Data.Tree (Tree(Node))
import qualified Data.Map as M
import qualified Data.Text as T
import Concur.Core
import Concur.Replica
import Concur.Replica.Run (runDefault)
import qualified Data.TreeZipper as TZ
import Control.Concurrent.STM.TChan

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
    (dy, tree_) <- unsafeBlockingIO $ atomically $ treeView renderer (toTree ast)
    main_ [ style [ ("height", "95vh"), ("display", "grid"), ("grid-template-rows", "auto 1fr") ] ]
      [ h1 [ style [ ("grid-row", "1") ] ] [ text "fooo" ]
      , div [ style [ ("grid-row", "2"), ("overflow", "hidden"), ("display", "grid"), ("grid-template-columns", "40% 60%") ] ]
        [ div [ style [ ("grid-column", "1"), ("overflow", "scroll") ] ] [ tree_ ]
        , div [ style [ ("grid-column", "2"), ("overflow", "scroll") ] ] [ sourceView (decodeUtf8 (hie_hs_src hieFile)) (getSpan <$> dy) ]
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

    getSpan ast =
      let s = nodeSpan ast
      in ( (srcSpanStartLine s - 1, srcSpanStartCol s - 1)
         , (srcSpanEndLine s - 1, srcSpanEndCol s - 1)
         )
{-
0-index
-}
sourceView
  :: Text
  -> Dynamic ((Int,Int), (Int,Int))
  -> Widget HTML v
sourceView src dyHlSpan = do
  let lines = T.lines src
  onDynamic dyHlSpan $ \sp -> div [] $ map (div [ lineStyle ] . lineBody_ sp) $ zip [0..] lines
  where
    lineStyle = style [ ("white-space", "pre"), ("font-family", "monospace") ]
    lineBody_ ((startLineNo, startCol), (endLineNo, endCol)) (lineNo, txt)
      | endLineNo < lineNo || lineNo < startLineNo = [ text txt ]
      | startLineNo < lineNo && lineNo < endLineNo = [ hl_ txt ]
      | startLineNo == lineNo && lineNo < endLineNo = let (a,b) = T.splitAt startCol txt in [ text a, hl_ b ]
      | startLineNo < lineNo && lineNo == endLineNo = let (a,b) = T.splitAt endCol txt in [ hl_ a, text b ]
      | otherwise =
        let (a,b') = T.splitAt startCol txt
            (b, c) = T.splitAt (endCol - startCol) b'
        in [ text a, hl_ b, text c ]

    hl_ txt = span [ style [ ("background-color", "yellow") ] ] [ text txt ]

{-
親に戻る時にスクロール位置が一番上に戻ってしまう...
上の階層のdivを消さずに div を重ねるていくほうがいいのかな？
-}
treeView
  :: (forall v. a -> Bool -> Widget HTML v)
  -> Tree a
  -> STM (Dynamic a, Widget HTML v)
treeView renderer root = do
  let zp = TZ.zipper root
  (dy, update) <- mkDynamic (TZ.current zp)
  pure (dy, render update zp)
  where
    renderer' tz isSelected = do
      let baseCss = [("border", "1px solid gray"), ("margin-bottom", "0.5rem") ]
      let addiCss = if isSelected then selectedStyle else mempty
      div [ style (baseCss <> addiCss), tz <$ onClick ]
        [ renderer (TZ.current tz) isSelected
        , whenJustA (TZ.downToFirstChild tz) $ \ctz -> button [ ctz <$ onClick ] [ text "cildren" ]
        ]
      where
        selectedStyle = [( "background-color", "yellow")]

    render update zp = do
      let before = reverse $ unfoldr (dupe <<$>> TZ.siblingBefore) zp
      let after  = unfoldr (dupe <<$>> TZ.siblingAfter) zp
      let all    = zip before (repeat False) <> [(zp, True)] <> zip after (repeat False)
      zp' <- div []
        [ whenJustA (TZ.up zp) $ \ctz -> button [ ctz <$ onClick ] [ text "parent" ]
        , div [] (map (uncurry renderer') all)
        ]
      unsafeBlockingIO $ atomically $ update (TZ.current zp')
      render update zp'

-- https://gitlab.haskell.org/ghc/ghc/wikis/hie-files#reading-hie-files
makeNc :: IO NameCache
makeNc = do
  uniqSupply <- mkSplitUniqSupply 'z'
  return $ initNameCache uniqSupply []

whenJustA :: Alternative m => Maybe a -> (a -> m v) -> m v
whenJustA m f = case m of
  Just a -> f a
  Nothing -> empty

-- reflex-like Dynamic
-- state
data Dynamic a = Dynamic (STM a) (STM (STM a))

instance Functor Dynamic where
  fmap f (Dynamic r p) = Dynamic (f <$> r) (f <<$>> p)

mkDynamic :: a -> STM (Dynamic a, a -> STM ())
mkDynamic init = do
  var  <- newTVar init
  chan <- newBroadcastTChan
  pure
    ( Dynamic (readTVar var) (dupTChan chan >>= pure . readTChan)
    , \a -> writeTVar var a *> writeTChan chan a
    )

-- TChan a is a read-only TChan a. Writing will hang.
readDynamic :: Dynamic a -> STM (a, STM a)
readDynamic (Dynamic r p) = (,) <$> r <*> p

onDynamic :: (MonadIO m, Alternative m) => Dynamic a -> (a -> m v) -> m v
onDynamic dy m = uncurry go =<< liftIO (atomically (readDynamic dy))
  where
    go a pull = do
      r <- fmap Left (liftIO $ atomically pull) <|> fmap Right (m a)
      case r of
        Left a' -> go a' pull
        Right v -> pure v

-- Local Variables:
-- dante-repl-command-line: ("cabal" "new-repl" "exe:pache" "--allow-newer")
-- eval: (dante-mode)
-- eval: (flycheck-mode)
-- End:
