{-# LANGUAGE DeriveGeneric #-}

{- Author: Arne Winter
   Date: 01/07/2022
   Description: Haskell implementation of interval tree clocks, as per the paper
   Almeida, Paulo & Baquero, Carlos & Fonte, Victor. (2008). Interval Tree Clocks: A Logical Clock for Dynamic Systems. 5401. 259-274. 10.1007/978-3-540-92221-6_18.
-}

{- | An interval tree clock implementation as per Interval Tree Clocks: A Logical Clock for Dynamic Systems.
   Paper by Almeida, Paulo & Baquero, Carlos & Fonte, Victor.
   This implementation by Arne Winter.
-}
module Data.Clock.IntervalTree (
    -- * Types
    Stamp (..),
    ITCId (..),
    ITCEvent (..),

    -- * Seed
    seed,

    -- * Operations
    fork,
    join,
    peek,
    event,

    -- * Comparison
    happenedBefore,
    StampComparison (..),
    stampCompare,
) where

import GHC.Generics

data Stamp = Stamp ITCId ITCEvent deriving (Eq, Show, Generic)

data ITCId
    = ITCIdBranch ITCId ITCId
    | ITCId !Bool
    deriving (Eq, Show)

data ITCEvent
    = ITCEventBranch !Integer ITCEvent ITCEvent
    | ITCEventLeaf !Integer
    deriving (Eq, Show)

{- | The seed stamp.
   The first peer is to use this, then fork others.
-}
seed :: Stamp
seed = Stamp iT (ITCEventLeaf 0)

{- | use to register new peers.
   one stamp must be consecutively owned be the forking peer.
   the other stamp owned by host.
-}
fork :: Stamp -> (Stamp, Stamp)
fork (Stamp i e) = let (i1, i2) = split i in (Stamp i1 e, Stamp i2 e)

-- | inverse of fork. s = uncurry join (fork s)
--   Note that the internal call to `sumId` may be partial, if the ITC Stamp was constructed through direct constructor usage.
--   Using only `fork` and `join` as the safe API to create stamps will not lead to this inconsistency.
join :: Stamp -> Stamp -> Stamp
join (Stamp i1 e1) (Stamp i2 e2) = Stamp (sumId i1 i2) (joinEv e1 e2)

{- | Anonymizes this stamp.
   Useful when sending messages or logging debug stamps.
-}
peek :: Stamp -> Stamp
peek (Stamp _ e) = Stamp iF e

{- | when something happened on this peer.
   use the new stamp afterwards.
-}
event :: Stamp -> Stamp
event s@(Stamp i e) =
    Stamp i $
        if potentiallyNew /= e
            then potentiallyNew
            else e'
  where
    potentiallyNew = fill' i e
    (e', _) = grow' s

data StampComparison
    = -- | A happened before B.
      Before
    | -- | A happened after B.
      After
    | -- | A happened concurrent to B in logical time.
      Concurrent
    deriving (Eq, Show)

{- | Compare two stamps.
   Note that stamp is not an instance of Ord because a <= b and b <= a does not imply a = b:
   rather that a happened concurrent to b per logical time.
-}
stampCompare :: Stamp -> Stamp -> StampComparison
stampCompare s1 s2
    | happenedBefore s1 s2 = Before
    | happenedBefore s2 s1 = After
    | otherwise = Concurrent

{- | the `happened before` relation for two stamps. (Note that I am unsure whether this is really the same relation as described by Lamport).
   Use this to examine causility of stamps.
-}
happenedBefore :: Stamp -> Stamp -> Bool
(Stamp _ e1) `happenedBefore` (Stamp _ e2) =
    (e1 `evLeq` e2) && not (e2 `evLeq` e1)

-- | some utility functions because the constructors are so verbose.
iF, iT :: ITCId
iF = ITCId False
iT = ITCId True

iB :: ITCId -> ITCId -> ITCId
iB = ITCIdBranch

normId :: ITCId -> ITCId
normId (ITCIdBranch (ITCId False) (ITCId False)) = iF
normId (ITCIdBranch (ITCId True) (ITCId True)) = iT
normId leaf = leaf

sumId :: ITCId -> ITCId -> ITCId
sumId (ITCId False) i = i
sumId i (ITCId False) = i
sumId (ITCIdBranch l1 r1) (ITCIdBranch l2 r2) = normId (ITCIdBranch (sumId l1 l2) (sumId r1 r2))
sumId _ _ = error "internal consistency error. Create ID's only by means of fork and join."

split :: ITCId -> (ITCId, ITCId)
split (ITCId False) = (iF, iF)
split (ITCId True) = (iB iT iF, iB iF iT)
split (ITCIdBranch (ITCId False) i) = (iB iF i1, iB iF i2) where (i1, i2) = split i
split (ITCIdBranch i (ITCId False)) = (iB i1 iF, iB i2 iF) where (i1, i2) = split i
split (ITCIdBranch l r) = (iB l iF, iB iF r)

fill' :: ITCId -> ITCEvent -> ITCEvent
fill' (ITCId False) e = e
fill' (ITCId True) e = ITCEventLeaf $ maxEv e
fill' _ n@(ITCEventLeaf _) = n
fill' (ITCIdBranch (ITCId True) ir) (ITCEventBranch n l r) =
    normEv $
        ITCEventBranch
            n
            (ITCEventLeaf (max (maxEv l) (minEv r')))
            r'
  where
    r' = fill' ir r
fill' (ITCIdBranch il (ITCId True)) (ITCEventBranch n l r) =
    normEv $
        ITCEventBranch
            n
            l'
            (ITCEventLeaf (max (maxEv r) (minEv l')))
  where
    l' = fill' il l
fill' (ITCIdBranch il ir) (ITCEventBranch n l r) = normEv $ ITCEventBranch n (fill' il l) (fill' ir r)

newtype Cost = Cost Integer deriving (Eq, Ord)

ltCost :: Cost -> Cost -> Bool
ltCost (Cost c1) (Cost c2) = c1 < c2

addCost :: Cost -> Cost -> Cost
addCost (Cost c1) (Cost c2) = Cost $ c1 + c2

grow' :: Stamp -> (ITCEvent, Cost)
grow' (Stamp (ITCId True) (ITCEventLeaf n)) =
    (ITCEventLeaf $ n + 1, Cost 0)
grow' (Stamp i (ITCEventLeaf n)) =
    (e', c `addCost` largeCost)
  where
    largeCost = Cost 1000
    (e', c) = grow' $ Stamp i (ITCEventBranch n (ITCEventLeaf 0) (ITCEventLeaf 0))
grow' (Stamp (ITCIdBranch (ITCId False) i) (ITCEventBranch n l r)) =
    (ITCEventBranch n l r', cr `addCost` (Cost 1))
  where
    (r', cr) = grow' $ Stamp i r
grow' (Stamp (ITCIdBranch i (ITCId False)) (ITCEventBranch n l r)) =
    ((ITCEventBranch n l' r), cl `addCost` (Cost 1))
  where
    (l', cl) = grow' $ Stamp i l
grow' (Stamp (ITCIdBranch il ir) (ITCEventBranch n l r))
    | costL `ltCost` costR = ((ITCEventBranch n l' r), costL `addCost` (Cost 1))
    | otherwise = ((ITCEventBranch n l r'), costR `addCost` (Cost 1))
  where
    (l', costL) = grow' $ Stamp il l
    (r', costR) = grow' $ Stamp ir r

-- | event comparison.
evLeq :: ITCEvent -> ITCEvent -> Bool
(ITCEventLeaf n1) `evLeq` (ITCEventLeaf n2) = n1 <= n2
(ITCEventLeaf n1) `evLeq` (ITCEventBranch n2 _ _) = n1 <= n2
(ITCEventBranch n1 l1 r1) `evLeq` (ITCEventLeaf n2) =
    and $
        [ n1 <= n2
        , (l1 `liftEv` n1) `evLeq` (ITCEventLeaf n2)
        , (r1 `liftEv` n1) `evLeq` (ITCEventLeaf n2)
        ]
(ITCEventBranch n1 l1 r1) `evLeq` (ITCEventBranch n2 l2 r2) =
    and $
        [ n1 <= n2
        , (l1 `liftEv` n1) `evLeq` (l2 `liftEv` n2)
        , (r1 `liftEv` n1) `evLeq` (r2 `liftEv` n2)
        ]

liftEv, sinkEv :: ITCEvent -> Integer -> ITCEvent
liftEv (ITCEventLeaf n) m = ITCEventLeaf $ n + m
liftEv (ITCEventBranch n e1 e2) m = ITCEventBranch (n + m) e1 e2
sinkEv e m = liftEv e (- m)

joinEv :: ITCEvent -> ITCEvent -> ITCEvent
joinEv (ITCEventLeaf n1) (ITCEventLeaf n2) = ITCEventLeaf (max n1 n2)
joinEv (ITCEventLeaf n1) b@(ITCEventBranch _ _ _) = joinEv (ITCEventBranch n1 (ITCEventLeaf 0) (ITCEventLeaf 0)) b
joinEv b@(ITCEventBranch _ _ _) (ITCEventLeaf n1) = joinEv b (ITCEventBranch n1 (ITCEventLeaf 0) (ITCEventLeaf 0))
joinEv b1@(ITCEventBranch n1 _ _) b2@(ITCEventBranch n2 _ _) | n1 > n2 = joinEv b2 b1
joinEv (ITCEventBranch n1 l1 r1) (ITCEventBranch n2 l2 r2) =
    normEv $
        ITCEventBranch
            n1
            (joinEv l1 $ liftEv l2 $ n2 - n1)
            (joinEv r1 $ liftEv r2 $ n2 - n1)

normEv :: ITCEvent -> ITCEvent
normEv n@(ITCEventLeaf _) = n
normEv (ITCEventBranch n (ITCEventLeaf m) (ITCEventLeaf m'))
    | m == m' = ITCEventLeaf (n + m)
normEv (ITCEventBranch n e1 e2) = ITCEventBranch (n + m) (sinkEv e1 m) (sinkEv e2 m)
  where
    m = min (minEv e1) (minEv e2)

minEv :: ITCEvent -> Integer
minEv (ITCEventLeaf n) = n
minEv (ITCEventBranch n e1 e2) = n + min (minEv e1) (minEv e2)

maxEv :: ITCEvent -> Integer
maxEv (ITCEventLeaf n) = n
maxEv (ITCEventBranch n e1 e2) = n + max (maxEv e1) (maxEv e2)
