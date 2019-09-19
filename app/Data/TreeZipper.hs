{-# LANGUAGE NamedFieldPuns #-}
module Data.TreeZipper
  ( TreeZipper
  , zipper
  , current
  , downToFirstChild
  , up
  , siblingBefore
  , siblingAfter
  ) where

import Data.Tree

data TreeZipper a = TreeZipper
  { parent :: Maybe (TreeZipper a)
  , before :: [Tree a]
  , point :: Tree a
  , after :: [Tree a]
  }

zipper :: Tree a -> TreeZipper a
zipper tree = TreeZipper Nothing [] tree []

current :: TreeZipper a -> a
current tz = rootLabel $ point tz

downToFirstChild :: TreeZipper a -> Maybe (TreeZipper a)
downToFirstChild tz =
  case subForest (point tz) of
    x:xs -> Just $ TreeZipper (Just tz) [] x xs
    []   -> Nothing

up :: TreeZipper a -> Maybe (TreeZipper a)
up = parent

siblingBefore :: TreeZipper a -> Maybe (TreeZipper a)
siblingBefore tz@TreeZipper{before,point,after} =
  case before of
    b:bs -> Just $ tz { before = bs, point = b, after = point:after }
    []   -> Nothing

siblingAfter :: TreeZipper a -> Maybe (TreeZipper a)
siblingAfter tz@TreeZipper{before,point,after} =
  case after of
    a:as -> Just $ tz { before = point:before, point = a, after = as }
    []   -> Nothing

-- Local Variables:
-- dante-repl-command-line: ("cabal" "new-repl" "exe:pache" "--allow-newer")
-- eval: (dante-mode)
-- eval: (flycheck-mode)
-- End:
