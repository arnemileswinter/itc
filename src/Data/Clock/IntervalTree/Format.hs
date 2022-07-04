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

fmtStampTikz :: Stamp -> String
fmtStampTikz (Stamp i e) = fmtIdsTikz i ++ "\n" ++ fmtEventsTikz e
  where
    tikzWidth, tikzLineHeight :: Double
    tikzWidth = 5
    tikzLineHeight = 0.25

    tikzRect :: Maybe String -> (Double, Double) -> (Double, Double) -> String
    tikzRect _ _ (w,h) | w == 0 = ""
                       | h == 0 = ""
    tikzRect m (x, y) (w, h) =
        "\\node[rectangle,draw=black,anchor=north west,"
            <> (case m of Just s -> "fill=" <> s <> ","; Nothing -> "")
            <> "minimum width="
            <> show w
            <> "cm"
            <> ",minimum height="
            <> show h
            <> "cm] at ("
            <> show x
            <> "cm,"
            <> show y
            <> "cm) {};\n"

    fmtIdsTikz :: ITCId -> String
    fmtIdsTikz i0 = go 0 tikzWidth i0
      where
        go oh w (ITCId b)
            | b = tikzRect (Just "blue!40!white") (oh, -tikzLineHeight) (w, tikzLineHeight)
            | otherwise = ""
        go oh w (ITCIdBranch l r) = go oh (w / 2) l <> go (oh + w / 2) (w / 2) r

    fmtEventsTikz :: ITCEvent -> String
    fmtEventsTikz e0 = go tikzWidth 0 (-tikzLineHeight) e0
      where
        go w oh ov (ITCEventLeaf n) = tikzRect Nothing (oh, ov + (fromIntegral n * tikzLineHeight)) (w, fromIntegral n * tikzLineHeight)
        go w oh ov (ITCEventBranch n l r) =
            go w oh ov (ITCEventLeaf n)
                <> go (w / 2) oh (ov + (fromIntegral n * tikzLineHeight)) l
                <> go (w / 2) (oh + w / 2) (ov + (fromIntegral n * tikzLineHeight)) r
