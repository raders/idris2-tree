-- copied from Haskell Data.Tree
module Tree

import Data.Either
import Data.List

--%default total

--
-- Tree
--

public export
record Tree a where
  constructor Node
  rootLabel : a
  subForest : List (Tree a)

mapTree : (a -> b) -> Tree a -> Tree b
mapTree f (Node x ts) = Node (f x) (map (mapTree f) ts)  
  
Functor Tree where
  map = mapTree
  
Applicative Tree where
  pure x = Node x []
  Node f tfs <*> tx@(Node x txs) =
        Node (f x) (map (f <$>) txs ++ map (<*> tx) tfs)
        
Monad Tree where
  Node x ts >>= f = case f x of
                      Node x' ts' => assert_total $ Node x' (ts' ++ map (>>= f) ts)        

public export
Forest : Type -> Type  
Forest a = List (Tree a)
