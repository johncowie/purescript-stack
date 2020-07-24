module Utils.Lens where

import Prelude
import Control.Apply (lift2)
import Data.Symbol (class IsSymbol, SProxy)
import Prim.Row (class Cons)
import Record (get, set) as R
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)

data Lens' a b = Lens (a -> b) (a -> b -> a)
data Iso a b = Iso (a -> b) (b -> a)

infixr 6 type Lens' as :->
infixr 6 type Iso as :<->:

lens :: forall a b. (a -> b) -> (a -> b -> a) -> Lens' a b
lens = Lens

iso :: forall a b. (a -> b) -> (b -> a) -> Iso a b
iso = Iso

view :: forall a b. Lens' a b -> a -> b
view (Lens getter _setter) a = getter a

over :: forall a b. Lens' a b -> (b -> b) -> a -> a
over l f a = set l (f (view l a)) a

set :: forall a b. Lens' a b -> b -> a -> a
set (Lens _getter setter) b a = setter a b

prop :: forall l r1 r a. IsSymbol l => Cons l a r r1 => SProxy l -> Lens' (Record r1) a
prop l = lens (R.get l) (flip (R.set l))

newtypeProp :: forall n l r1 r a. (Newtype n (Record r1)) => IsSymbol l => Cons l a r r1 => SProxy l -> Lens' n a
newtypeProp s = _newtype >>> prop s

flipIso :: forall a b. (a :<->: b) -> (b :<->: a)
flipIso (Iso fw bw) = Iso bw fw

isoToLens :: forall a b. (a :<->: b) -> (a :-> b)
isoToLens (Iso fw bw) = lens fw (const bw)

liftIso :: forall f a b. (Functor f) => (a :<->: b) -> f a :<->: f b
liftIso (Iso fw bw) = Iso (map fw) (map bw)

composeLenses :: forall b c d. Lens' c d -> Lens' b c -> Lens' b d
composeLenses lensCD lensBC = lens getter setter
  where getter b = view lensCD (view lensBC b)
        setter b d = over lensBC (set lensCD d) b

instance lensSemigroupoid :: Semigroupoid Lens' where
  compose = composeLenses

-- both :: forall a b c. (a :-> b) -> (a :-> c) -> (a :-> (b /\ c))
-- both = unsafeThrow "implement me"

-- some handy lenses
newtypeIso :: forall a b. (Newtype a b) => a :<->: b
newtypeIso = Iso unwrap wrap

_newtype :: forall a b. (Newtype a b) => Lens' a b
_newtype = isoToLens newtypeIso

_mapVal :: forall k v. (Ord k) => v -> k -> Lens' (M.Map k v) v
_mapVal default id = lens getter setter
  where getter m = fromMaybe default $ M.lookup id m
        setter m v = M.insert id v m

_mapValMaybe :: forall k v. (Ord k) => k -> Lens' (M.Map k v) (Maybe v)
_mapValMaybe id = lens getter setter
  where getter m = M.lookup id m
        setter m vM = case vM of
          (Just v) -> M.insert id v m
          (Nothing) -> m
