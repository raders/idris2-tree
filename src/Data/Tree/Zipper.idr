-- copied from Haskell Data.Tree.Zipper
module Zipper

import Data.Either
import Data.List

import Tree

--%default total

export
data Empty a = E

export
data Full a  = F (Tree a)

-- | A position within a 'Tree'.
-- The parameter 't' inidcates if the position is pointing to
-- a specific tree (if 't' is 'Full'), or if it is pointing in-between
-- trees (if 't' is 'Empty').
public export
record TreePos t a where
  constructor MkTreePos
  content : t a
  before  : Forest a
  after   : Forest a
  parents : List (Forest a, a, Forest a)
  
export
tree : TreePos Full a -> Tree a
tree (MkTreePos (F x) before after parents) = x

prevSpace : TreePos Full a -> TreePos Empty a
prevSpace loc = {content := E, after := (tree loc :: after loc)} loc

prevTree : TreePos Empty a -> Maybe (TreePos Full a)
prevTree loc =
  case before loc of
    (t :: ts) => Just $ {content := F t, before := ts } loc
    []        => Nothing

nextSpace : TreePos Full a -> TreePos Empty a
nextSpace loc = {content := E, before := tree loc :: before loc } loc

nextTree : TreePos Empty a -> Maybe (TreePos Full a)
nextTree loc =
  case after loc of
    (t :: ts) => Just $ {content := F t, after := ts } loc
    []        => Nothing

-- | Positions may be either 'Full' or 'Empty'.
interface PosType t where
  prev   : TreePos t a -> Maybe (TreePos t a)
  next   : TreePos t a -> Maybe (TreePos t a)
  forest : TreePos t a -> Forest a
 
export
PosType Full where
  prev = prevTree . prevSpace
  next = nextTree . nextSpace
  forest loc = foldl (flip (::)) (tree loc :: after loc) (before loc)

export
PosType Empty where
  prev = map prevSpace . prevTree
  next = map nextSpace . nextTree
  forest loc = foldl (flip (::)) (after loc) (before loc)

export
parent : PosType t => TreePos t a -> Maybe (TreePos Full a)
parent loc =
  case parents loc of
    [] => Nothing
    ((ls, a, rs) :: ps) => Just (MkTreePos (F (Node a (forest loc))) ls rs ps)
  
export
root : TreePos Full a -> TreePos Full a
root loc = maybe loc root (parent loc)

export
children : TreePos Full a -> TreePos Empty a
children loc =
  MkTreePos E 
            [] 
            (subForest (tree loc) )
            ((before loc, rootLabel (tree loc), after loc) :: parents loc)

export
first : TreePos Empty a -> TreePos Empty a
first loc = {content := E, before := [], after := reverse (before loc) ++ after loc} loc

-- | The last space in the current forest.
export
last : TreePos Empty a -> TreePos Empty a
last loc = {content := E, before := reverse (after loc) ++ before loc, after := []} loc

-- | The empty space at the given index.  The first space is at index 0.
-- For indexes that are negative or too large, we return the first and last
-- position in the tree, respectively.
export
spaceAt : Nat -> TreePos Empty a -> TreePos Empty a
spaceAt n loc = let
                  (as,bs) = splitAt n (forest loc)
                in
                  {content := E
                 , before := reverse as
                 , after := bs
                   } loc


-- | The first child of the given location.
export
firstChild : TreePos Full a -> Maybe (TreePos Full a)
firstChild = nextTree . children

-- | The last child of the given location.
export
lastChild : TreePos Full a -> Maybe (TreePos Full a)
lastChild = prevTree . last . children

-- | The child at the given index in the tree.
-- The first child is at index 0.
export
childAt : Nat -> TreePos Full a -> Maybe (TreePos Full a)
childAt n = nextTree . spaceAt n . children


-- Conversions -----------------------------------------------------------------

-- | A location corresponding to the root of the given tree.
export
fromTree : Tree a -> TreePos Full a
fromTree t = MkTreePos (F t) [] [] []

-- | The location at the beginning of the forest.
export
fromForest : Forest a -> TreePos Empty a
fromForest ts = MkTreePos E [] ts []

-- | The tree containing this location.
export
toTree : TreePos Full a -> Tree a
toTree loc = tree (root loc)

-- | The forest containing this location.
export
toForest : PosType t => TreePos t a -> Forest a
toForest loc = case parent loc of
                 Nothing => forest loc
                 Just p  => toForest p -- polymprphic recursion

-- Queries ---------------------------------------------------------------------

-- | Are we at the top of the tree?
isRoot : PosType t => TreePos t a -> Bool
isRoot loc = null (parents loc)

-- | Are we the first position (of its kind) in a forest.
isFirst : PosType t => TreePos t a -> Bool
isFirst loc = null (before loc)

-- | Are we the last position (of its kind) in a forest.
isLast : PosType t => TreePos t a -> Bool
isLast loc = null (after loc)

-- | Are we at the bottom of the tree?
isLeaf : TreePos Full a -> Bool
isLeaf loc = null (subForest (tree loc))

-- | Do we have a parent?
isContained : PosType t => TreePos t a -> Bool
isContained loc = not (isRoot loc)

-- | Do we have children?
hasChildren : TreePos Full a -> Bool
hasChildren loc = not (isLeaf loc)

-- The current tree -----------------------------------------------------------


-- | The selected tree.
--tree : TreePos Full a -> Tree a
--tree x = unF (_content x)

-- | The current label.
label : TreePos Full a -> a
label loc = rootLabel (tree loc)

-- | Insert a new tree at the current position.
insert : Tree a -> TreePos Empty a -> TreePos Full a
insert t loc = { content := F t } loc

-- | Remove the tree at the current position.
delete : TreePos Full a -> TreePos Empty a
delete loc = { content := E } loc



-- | Change the current tree.
setTree : Tree a -> TreePos Full a -> TreePos Full a
setTree t loc = {content := F t } loc

-- | Modify the current tree.
modifyTree : (Tree a -> Tree a) -> TreePos Full a -> TreePos Full a
modifyTree f loc = setTree (f (tree loc)) loc

-- | Change the label at the current node.
setLabel : a -> TreePos Full a -> TreePos Full a
setLabel v loc = modifyTree (\t => {rootLabel := v } t) loc


-- | Modify the label at the current node.
modifyLabel : (a -> a) -> TreePos Full a -> TreePos Full a
modifyLabel f loc = setLabel (f (label loc)) loc

