module Utils.NumberFormat
( toDP )
where

import Data.Number.Format as NF

toDP :: Int -> Number -> String
toDP dp = NF.toStringWith (NF.fixed dp)
