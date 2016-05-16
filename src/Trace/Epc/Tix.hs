module Trace.Epc.Tix where

import Trace.Hpc.Tix (Tix(Tix), TixModule(TixModule))
import Trace.Hpc.Util (HpcHash, toHash)

mkTix :: (Foldable f, HpcHash (f a)) => String -> f a -> Tix
mkTix name entries =
     Tix [TixModule name (toHash entries) (length entries) (replicate (length entries) 0)]
