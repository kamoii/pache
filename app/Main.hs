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
import HieUtils
import Avail
import FastString (unpackFS)
import Name (nameSrcSpan, nameUnique, nameOccName, dataName, tcClsName, tvName, varName)
import qualified Name as Name
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
import Control.Concurrent.STM (retry)
import Control.Concurrent.STM.TChan
import qualified Text.ParserCombinators.ReadP as RP
import Stitch
import Optics
import Data.Tree.Optics
import Rapid

import SysTools ( initSysTools )
import DynFlags ( DynFlags, defaultDynFlags )
import GHC.Paths (libdir)

dynFlagsForPrinting :: IO DynFlags
dynFlagsForPrinting = do
  systemSettings <- initSysTools libdir
  return $ defaultDynFlags systemSettings ([], [])

main :: IO ()
main = do
  [hiePath] <- getArgs
  main' hiePath

up = rapid 0 \r -> do
  restart r "server" $ main' "/home/sino/workspace/hies/Relude/Extra/Map.hie"

main' hiePath = do
  dynFlags <- dynFlagsForPrinting
  nc <- makeNc
  (hieFileResult, nc') <- readHieFile nc hiePath
  let hieFile = hie_file_result hieFileResult
  let filePath = hie_hs_file hieFile
  let module' = hie_module hieFile
  let asts = hie_asts hieFile
  let [ast] = M.elems $ getAsts asts
  putStrLn $ "hieFile read!! from: " <> filePath
  print $ M.keys $ getAsts asts
  let header =
        [ VLeaf "link" $ fromList [ ("rel", AText "stylesheet"), ("href", AText "https://unpkg.com/purecss@1.0.1/build/pure-min.css") ]
        , VNode "style" (fromList [("type", AText "text/css")]) [ VRawText (renderCSS cssStyle) ]
        ]
  let cnf' = mkDefaultConfig 8080 "hie-viewer"
  let cnf = cnf' { cfgHeader = header }
  run cnf \_ -> do
    (dy', update) <- unsafeBlockingIO $ atomically $ mkDynamic ast
    let dy = getSpan . nodeSpan <$> dy'
    main_ [ style [ ("height", "95vh"), ("display", "grid"), ("grid-template-rows", "auto 1fr") ] ]
      [ header_ module'
      , div [ style [ ("grid-row", "2"), ("overflow", "hidden"), ("display", "grid"), ("grid-template-columns", "40% 60%") ] ]
        [ div [ style [ ("grid-column", "1"), ("overflow", "scroll") ] ] [ leftPain_ dynFlags update hieFileResult hieFile ]
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

baseStyle :: _
baseStyle = do
  "html" ? do
    "font-family" .= "monospace"
    -- https://palx.jxnblk.com/07c
    "color" .= "#374047"
    "background-color" .= "#ebedee"

-- css
cssStyle :: _
cssStyle = do
  baseStyle
  ".ident-table" ? do
    "border-collapse" .= "collapse"
    "border" .= "1px solid black"
    "td" ? "border" .= "1px solid black"
    "th" ? "border" .= "1px solid black"
  ".context-info" ? do
    "width" .= "12em"
    "white-space" .= "nowrap"
    "overflow" .= "hidden"
    "text-overflow" .= "ellipsis"
  leftPainStyle

getSpan :: RealSrcSpan -> _
getSpan s =
  ( (srcSpanStartLine s - 1, srcSpanStartCol s - 1)
  , (srcSpanEndLine s - 1, srcSpanEndCol s - 1)
  )

-- Info / Avails / AST
-- Assumes there is only one AST.

leftPainStyle = do
  ".left-pain" ? do
    "padding" .= "0.5em"

data LeftPain
  = LPInfo
  | LPExports
  | LPAst
  deriving Eq

leftPain_ dynFlags spanUpdate hieFileResult hieFile = do
  let asts = hie_asts hieFile
  let [ast] = M.elems $ getAsts asts
  loopM_ LPExports \t -> do
    div [ className "left-pain" ]
      [ div [ className "pure-menu pure-menu-horizontal" ]
        [ ul [ className "pure-menu-list" ]
          [ pureMenuItem_ t LPInfo "Info"
          , pureMenuItem_ t LPExports "Exports"
          , pureMenuItem_ t LPAst "AST"
          ]
        ]
      , hr []
      , case t of
          LPInfo    -> info_ hieFileResult hieFile
          LPExports -> exports_ hieFile
          LPAst     -> treeView spanUpdate (renderHieAST dynFlags (hie_types hieFile)) (toTree ast)
      ]
  where
    pureMenuItem_ cur c =
      li [ className ("pure-menu-item" <> bool "" " pure-menu-selected" (cur == c)) ]
      . one
      . pureMenuLink_ c
    pureMenuLink_ c =
      a [ c <$ onClick, href "#", className "pure-menu-link" ] . one . text

    toTree :: HieAST i -> Tree (HieAST i)
    toTree ast = Data.Tree.Node ast (map toTree (nodeChildren ast))

--
info_ hieFileResult hieFile = do
  let Package{..} = modulePackage (hie_module hieFile)
  dl []
    [ dt_ "Result Version" ,   dd_ $ show $ hie_file_result_version hieFileResult
    , dt_ "GHC Version" ,      dd_ $ decodeUtf8 $ hie_file_result_ghc_version hieFileResult
    , dt_ "Source File Path" , dd_ (toText (hie_hs_file hieFile))
    , dt_ "Module" ,           dd_ $ toText $ moduleNameString $ moduleName $ hie_module hieFile
    , dt_ "Package" ,          dd_ $ toText $ packageName <> "-" <> packageVersion
    ]
  where
    dt_ = dt [] . one . text
    dd_ = dd [] . one . text

exports_ hieFile = do
  let curMod  = hie_module hieFile
  let exports = hie_exports hieFile
  orr
    [ h4 [] [ text $ "Export num: " <> show (length exports) ]
    , ul [] $ map (li [] . one. export_ curMod) exports
    ]
  where
    export_ curMod (Avail name) = do
      -- Name sort must be `external` so there shoud be a module.
      let Just mod = Name.nameModule_maybe name
      orr
        [ h5 [] [ text . toText . occNameString $ nameOccName name ]
        , dl []
          [ dt_ "Name Space"
          , dd_ $ (showNameSpace $ occNameSpace $ nameOccName name)
          , dt_ "Name Sort"
          , dd_ $ (showNameSort name)
          , dt_ "Definition"
          , dd_ $ if curMod == mod
                     then showSrcSpan (nameSrcSpan name)
                     else toText $ moduleNameString $ moduleName mod
          -- , dt_ "uniq", dd_ $ show (nameUnique name)
          -- , dt_ "span", dd_ $ showSrcSpan (nameSrcSpan name)
          ]
        ]

    export_ curMod (AvailTC name _ _) =
      text . toText . occNameString $ nameOccName name

    dt_ = dt [] . one . text
    dd_ = dd [] . one . text

renderHieAST :: _ -> _ -> HieAST TypeIndex -> Widget HTML v
renderHieAST dynFlags hieTypes ast = do
  let ni = nodeInfo ast
  table []
    [ tr [] [ th [] [ text "annots" ], td [] [ text (toText . showAnnots . nodeAnnotations $ ni) ] ]
    , tr [] [ th [] [ text "types"  ], td [] [ renderTypes (nodeType ni) ] ]
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

    renderTypes types =
      if null types
         then text "[]"
         else orr $ intersperse (br []) $ map (text . toText . showType) types

    showType typeIndex =
      renderHieType dynFlags $ recoverFullType typeIndex hieTypes

    renderIdents idents
      | M.null idents = text "[]"
      | otherwise = table [] $ map renderIdentTr (M.toList idents)

    renderIdentTr (ident, detail) = do
      let detailTd = td []
            [ b [] [ text "detail(contextual)" ]
            , br []
            , text "context info:"
            , br []
            , orr $ (map (contextInfo_ . show) $ S.toList $ identInfo detail)
            , br []
            , text "type:"
            , br []
            , text $ maybe "--" (toText . showType) (identType detail)
            ]
      case ident of
        Left moduleName ->
          tr []
            [ th [colspan "2"] [ text (toText $ moduleNameString moduleName <> "(module name)") ]
            , detailTd
            ]
        Right name -> do
          tr []
            [ th [] [ text . toText . occNameString $ nameOccName name ]
            , td [] $ intersperse (br [])
              [ b [] [ text "name(universal)" ]
              , text $ "name space: " <> (showNameSpace $ occNameSpace $ nameOccName name)
              , text $ "name sort: " <> (showNameSort name)
              , text $ "uniq: " <> show (nameUnique name)
              , text $ "span: " <> showSrcSpan (nameSrcSpan name)
              ]
            , detailTd
            ]

    contextInfo_ ci =
      div [ className "context-info" ] [ text ci ]

-- NameSpace doesn't have `Show` instance. It doesn't export constructors too.
showNameSpace ns
  | ns == dataName  = "Data Constructor"
  | ns == tcClsName = "Type Constructor or Class"
  | ns == tvName    = "Type Variable"
  | ns == varName   = "Variable"
  | otherwise      = error "New NameSpace was added?"

showNameSort name
  | Name.isBuiltInSyntax name = "Built-in-Syntax"
  | Name.isWiredInName name   = "Wired-in"
  | Name.isSystemName name    = "System"
  | Name.isExternalName name  = "External"
  | Name.isInternalName name  = "Internal"
  | otherwise                 = error "Unexpected name sort"

showSrcSpan (RealSrcSpan s) = show $ getSpan s
showSrcSpan (UnhelpfulSpan s) = toText $ unpackFS s


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
        hl_ txt = span [ style [ ("background-color", "#e0d668") ] ] [ text txt ]

{-
tree view状態をどのように表現するかが肝。
基本的に開閉状態かな？
Tree (Bool, a) にするか

  (dy, update) <- mkDynamic (tree ^. root)

-}
treeView
  :: _
  -> (forall v. a -> Widget HTML v)
  -> Tree a
  -> Widget HTML v
treeView update renderer tree = go ((False,) <$> tree) update
  where
    go root update =
      loopM_ root \root -> renderNode root update

    renderNode node update = do
      let (isOpen, a) = node ^. root
      div []
        [ div [ rootStyle_ ]
          [ renderer a
          , whenA (hasChild node)
            $ button [ (node & root % _1 %~ not) <$ onClick ] [ text "child toggle" ]
          , forever
            $ button [ onClick ] [ text "hightlight" ] *> (unsafeBlockingIO $ atomically $ update a)
          ]
        , whenA (isOpen && hasChild node)
          $ div [ childrenStyle_ ]
          $ flip map (zip [0..] (node ^. branches))
          $ \(i, childNode) -> (\c -> set (branches % ix i) c node) <$> renderNode childNode update
        ]

    rootStyle_ = style [ ("border", "1px solid gray"), ("margin-bottom", "0.5rem") ]
    childrenStyle_ = style [ ("margin-left", "2em") ]
    hasChild node = not . null $ node ^. branches

-- https://gitlab.haskell.org/ghc/ghc/wikis/hie-files#reading-hie-files
makeNc :: IO NameCache
makeNc = do
  uniqSupply <- mkSplitUniqSupply 'z'
  return $ initNameCache uniqSupply []

-- Same as `iterateM_` from `monad-loops`
loopM_ :: Monad m => a -> (a -> m a) -> m v
loopM_ a m = m a >>= \a' -> loopM_ a' m

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
