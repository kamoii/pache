{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Main where

import Prelude()
import Relude hiding (div)
import Relude.Extra.Tuple (dupe)
import HieBin
import HieTypes
import FastString (unpackFS)
import Name (nameUnique, nameOccName, dataName, tcClsName, tvName, varName)
import OccName (occNameString, occNameSpace)
import Module (Module(..), moduleStableString, moduleNameString, moduleName, moduleUnitId, unitIdString)
import NameCache (NameCache, initNameCache)
import UniqSupply (mkSplitUniqSupply)
import SrcLoc
import System.Environment (getArgs)
import Data.Tree (Tree(Node))
import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Tree as Tr
import Concur.Core
import Concur.Replica
import Concur.Replica.Run (run, mkDefaultConfig)
import qualified Data.TreeZipper as TZ
import Control.Concurrent.STM (retry)
import Control.Concurrent.STM.TChan
import qualified Text.ParserCombinators.ReadP as RP
import Stitch
import Optics
import Data.Tree.Optics

main :: IO ()
main = do
  [hiePath] <- getArgs
  main' hiePath

main' hiePath = do
  nc <- makeNc
  (hieFileResult, nc') <- readHieFile nc hiePath
  let hieFile = hie_file_result hieFileResult
  let filePath = hie_hs_file hieFile
  let module' = hie_module hieFile
  let asts = hie_asts hieFile
  let [ast] = M.elems $ getAsts asts
  putStrLn $ "hieFile read!! from: " <> filePath
  print $ M.keys $ getAsts asts
  let header = [ VNode "style" (fromList [("type", AText "text/css")]) [ VRawText cssStyle ] ]
  let cnf' = mkDefaultConfig 8080 "hie-viewer"
  let cnf = cnf' { cfgHeader = header }
  run cnf \_ -> do
    (_dy, tree_) <- unsafeBlockingIO $ atomically $ treeView2 renderHieAST (toTree ast)
    let dy = constDynamic ((0,0),(0,0))
    main_ [ style [ ("height", "95vh"), ("display", "grid"), ("grid-template-rows", "auto 1fr") ] ]
      [ header_ module'
      , div [ style [ ("grid-row", "2"), ("overflow", "hidden"), ("display", "grid"), ("grid-template-columns", "40% 60%") ] ]
        [ div [ style [ ("grid-column", "1"), ("overflow", "scroll") ] ] [ tree_ ]
        , div [ style [ ("grid-column", "2"), ("overflow", "scroll") ] ] [ sourceView (decodeUtf8 (hie_hs_src hieFile)) dy ]
        ]
      ]
  where
    header_ module' = do
      let Package{..} = modulePackage module'
      let modname = moduleNameString $ moduleName module'
      h1 [ style [ ("grid-row", "1") ] ]
        [ text (toText modname)
        , text . toText $ "(" <> packageName <> "-" <> packageVersion <> ")"
        ]

    toTree :: HieAST i -> Tree (HieAST i)
    toTree ast = Data.Tree.Node ast (map toTree (nodeChildren ast))

    getSpan ast =
      let s = nodeSpan ast
      in ( (srcSpanStartLine s - 1, srcSpanStartCol s - 1)
         , (srcSpanEndLine s - 1, srcSpanEndCol s - 1)
         )

-- css
cssStyle :: Text
cssStyle = renderCSS do
  ".ident-table" ? do
    "border-collapse" .= "collapse"
    "border" .= "1px solid black"
    "td" ? "border" .= "1px solid black"
    "th" ? "border" .= "1px solid black"

renderHieAST :: HieAST i -> Widget HTML v
renderHieAST ast = do
  let ni = nodeInfo ast
  table []
    [ tr [] [ th [] [ text "annots" ], td [] [ text (toText . showAnnots . nodeAnnotations $ ni) ] ]
    , tr [] [ th [] [ text "idents" ], td [] [ renderIdents (nodeIdentifiers ni) ] ]
    ]
  where
    showAnnots annots =
      if S.null annots
         then "[]"
         else intercalate ", " . map showAnnot $ S.toList annots

    -- Each annotation is (FastString, FastString)
    showAnnot (a, b) =
      unpackFS a <> "/" <> unpackFS b

    renderIdents idents
      | M.null idents = text "[]"
      | otherwise = do
          let th_ label = th [] [ text label ]
          table [ className "ident-table" ]
            $ [ tr [] [ th_ "name", th_ "name space", th_ "name sort", th_ "uniq" ] ]
            <> map renderIdentTr (M.toList idents)

    renderIdentTr (ident, detail) =
      case ident of
        Left moduleName ->
          tr []
            [ td [ colspan "4" ] [ text (toText $ moduleNameString moduleName <> "(module name)") ] ]
        Right name ->
          tr [] $ map (\t -> td [] [ text (toText t) ])
            [ occNameString $ nameOccName name
            , showNameSpace $ occNameSpace $ nameOccName name
            , ""
            , show $ nameUnique name
            ]

    -- NameSpace doesn't have `Show` instance. It doesn't export constructors too.
    showNameSpace ns
      | ns == dataName  = "data constructor"
      | ns == tcClsName = "type constructor or class"
      | ns == tvName    = "type variable"
      | ns == varName   = "variable"
      | otherwise      = error "New NameSpace was added?"



-- | Extract package information from `Module` type
--
-- Given a HIE file, only inforamtion you can get related to package
-- name and version is `hie_module :: Module`. Inside `Module` type,
-- there is a `UnitId`. UnitId has two constructor, but for HIE files,
-- (1)constructor should be `DefiniteUnitId DefUnitId`, which is
-- essentially a String.
--
-- This String should have following format(2):
--
--   <package name>-<version>-<hash(length 64)>
--   e.g. attoparsec-0.13.2.3-c26ea5327ea5e6e18a489b8b0eafa786f8941a0f85bf09f83aa8c5760fca2d79
--
-- NOTE: (1), (2) is an assume I made. It maybe wrong.
data Package = Package
  { packageName :: String
  , packageVersion :: String
  , packageHash :: String
  } deriving (Eq, Ord, Show)

modulePackage :: Module -> Package
modulePackage mod =
  let
    unitId = unitIdString $ moduleUnitId mod
    result = RP.readP_to_S parsePackage unitId
  in
    case result of
      [(package,"")] -> package
      _ -> error "Assumction about unitId broke"

-- e.g. attoparsec-0.13.2.3-c26ea5327ea5e6e18a489b8b0eafa786f8941a0f85bf09f83aa8c5760fca2d79
parsePackage :: RP.ReadP Package
parsePackage =
  Package
    <$> do RP.many1 (RP.satisfy $ C.isAlphaNum ||^ (=='_') ||^ (=='-'))
    <*> do RP.char '-' *> RP.munch1 (C.isNumber ||^ (=='.'))
    <*> do RP.char '-' *> RP.count 64 (RP.satisfy C.isAlphaNum) <* RP.eof

sourceView
  :: Text
  -> Dynamic ((Int,Int), (Int,Int))  -- 0-index
  -> Widget HTML v
sourceView src dyHlSpan = do
  let lines = T.lines src
  onDynamic dyHlSpan $ \sp -> container_ $ map (line_ sp) $ zip [0..] lines
  where
    container_ =
      div [ style [ ("padding", "0.5em 0px") ] ]

    line_ sp (i, txt) =
      div
        [ style [ ("white-space", "pre"), ("font-family", "monospace"), ("display", "grid"), ("grid-template-columns", "3em 1fr") ] ]
        [ div [ style [ ("grid-column", "1"), ("text-align", "right"), ("padding", "0px 0.5em") ] ] [ text $ show (i+1) ]
        , div [ style [ ("grid-column", "2") ] ] (lineBody_ sp (i,txt))
        ]

    lineBody_ ((startLineNo, startCol), (endLineNo, endCol)) (lineNo, txt)
      | endLineNo < lineNo || lineNo < startLineNo = [ text txt ]
      | startLineNo < lineNo && lineNo < endLineNo = [ hl_ txt ]
      | startLineNo == lineNo && lineNo < endLineNo = let (a,b) = T.splitAt startCol txt in [ text a, hl_ b ]
      | startLineNo < lineNo && lineNo == endLineNo = let (a,b) = T.splitAt endCol txt in [ hl_ a, text b ]
      | otherwise =
        let (a,b') = T.splitAt startCol txt
            (b, c) = T.splitAt (endCol - startCol) b'
        in [ text a, hl_ b, text c ]
      where
        hl_ txt = span [ style [ ("background-color", "yellow") ] ] [ text txt ]

{-
tree view状態をどのように表現するかが肝。
基本的に開閉状態かな？
Tree (Bool, a) にするか
-}
treeView2
  :: (forall v. a -> Widget HTML v)
  -> Tree a
  -> STM (Dynamic (Tree (Bool,a)), Widget HTML v)
treeView2 renderer tree = do
  let tree' = (False,) <$> tree
  (dy, update) <- mkDynamic tree'
  pure (dy, go tree' update)
  where
    go root update = do
      newRoot <- renderNode root
      unsafeBlockingIO $ atomically $ update newRoot
      go newRoot update

    renderNode node = do
      let (isOpen, a) = node ^. root
      div []
        [ div [ rootStyle_ ]
          [ renderer a
          , whenA (hasChild node)
            $ button [ (node & root % _1 %~ not) <$ onClick ] [ text "child toggle" ]
          ]
        , whenA (isOpen && hasChild node)
          $ div [ childrenStyle_ ]
          $ flip map (zip [0..] (node ^. branches))
          $ \(i, childNode) -> (\c -> set (branches % ix i) c node) <$> renderNode childNode
        ]
      where
        rootStyle_ = style [ ("border", "1px solid gray"), ("margin-bottom", "0.5rem") ]
        childrenStyle_ = style [ ("margin-left", "2em") ]
        hasChild node = not . null $ node ^. branches

{-
実装は簡単だし、効率も悪くないが、使いがって見やすさに問題がある。
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

whenA :: Alternative m => Bool -> m v -> m v
whenA b m = bool empty m b

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

constDynamic :: a -> Dynamic a
constDynamic a = Dynamic (pure a) (pure retry)

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
