module Utils.Lens where

import Prelude
import Data.Symbol (class IsSymbol, SProxy)
import Prim.Row (class Cons)
import Record (get, set) as R

data Lens' a b = Lens (a -> b) (a -> b -> a)

lens :: forall a b. (a -> b) -> (a -> b -> a) -> Lens' a b
lens = Lens

view :: forall a b. Lens' a b -> a -> b
view (Lens getter _setter) a = getter a

over :: forall a b. Lens' a b -> (b -> b) -> a -> a
over l f a = set l (f (view l a)) a

set :: forall a b. Lens' a b -> b -> a -> a
set (Lens _getter setter) b a = setter a b

prop :: forall l r1 r a. IsSymbol l => Cons l a r r1 => SProxy l -> Lens' (Record r1) a
prop l = lens (R.get l) (flip (R.set l))

composeLenses :: forall b c d. Lens' c d -> Lens' b c -> Lens' b d
composeLenses lensCD lensBC = lens getter setter
  where getter b = view lensCD (view lensBC b)
        setter b d = over lensBC (set lensCD d) b

instance lensSemigroupoid :: Semigroupoid Lens' where
  compose = composeLenses
