-- | 

module Data.Clock.IntervalTree.Format where

import Data.Clock.IntervalTree

-- | pretty print a stamp, mathematical notation like in the original paper.
fmtStamp :: Stamp -> String
fmtStamp (Stamp i e) = "(" <> fmtId i <> ", " <> fmtEv e <> ")"
  where
    fmtId (ITCId False) = "0"
    fmtId (ITCId True) = "1"
    fmtId (ITCIdBranch l r) = "(" <> fmtId l <> ", " <> fmtId r <> ")"

    fmtEv (ITCEventLeaf n) = show n
    fmtEv (ITCEventBranch n l r) = "(" <> show n <> ", " <> fmtEv l <> ", " <> fmtEv r <> ")"
