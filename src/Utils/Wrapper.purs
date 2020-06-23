module Utils.Wrapper where

import Prelude
import Data.Newtype (class Newtype, unwrap, wrap)

data Wrapper typ v = Wrapper v

instance newtypeWrapper :: Newtype (Wrapper typ v) v where
  wrap v = Wrapper v
  unwrap (Wrapper v) = v

derive instance eqWrapper :: (Eq v) => Eq (Wrapper typ v)
derive instance ordWrapper :: (Ord v) => Ord (Wrapper typ v)

instance showWrapper :: (Show v) => Show (Wrapper typ v) where
  show (Wrapper v) = show v

rewrap :: forall a b v. Wrapper a v -> Wrapper b v
rewrap = unwrap >>> wrap
