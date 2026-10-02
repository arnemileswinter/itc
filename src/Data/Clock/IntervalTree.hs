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
    sameHistory,
    StampComparison (..),
    stampCompare,
) where

import GHC.Generics

data Stamp = Stamp ITCId ITCEvent deriving (Eq, Show, Generic)

data ITCId
    = ITCIdBranch ITCId ITCId
    | ITCIdOff
    | ITCIdOn
    deriving (Eq, Show, Generic)

data ITCEvent
    = ITCEventBranch !Integer ITCEvent ITCEvent
    | ITCEventLeaf !Integer
    deriving (Eq, Show, Generic)

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

{- | inverse of fork. s = uncurry join (fork s)

   Precondition: the identities of both stamps must be disjoint (or anonymous).
   The paper requires identities to be kept globally unique, and leaves the sum of overlapping identities undefined.
   This holds as long as every stamp is consumed at most once: after @fork s@, do not use @s@ again;
   after @join a b@, do not use @a@ or @b@ again. Haskell does not enforce this, so e.g. @join s s@ or
   joining a stamp with one of its own forks calls 'error'. The same applies to stamps built through direct constructor usage.
   Joining with an anonymous stamp (see 'peek') is always safe.
-}
join :: Stamp -> Stamp -> Stamp
join (Stamp i1 e1) (Stamp i2 e2) = Stamp (sumId i1 i2) (joinEv e1 e2)

{- | Anonymizes this stamp.
   Useful when sending messages or logging debug stamps.
-}
peek :: Stamp -> Stamp
peek (Stamp _ e) = Stamp iF e

{- | when something happened on this peer.
   use the new stamp afterwards.

   Precondition: the stamp must not be anonymous, i.e. must not come from 'peek'.
   An anonymous stamp owns no part of the interval and thus cannot register events (section 5.3.4 of the paper).
   Calling 'event' on an anonymous stamp calls 'error'.
-}
event :: Stamp -> Stamp
event (Stamp ITCIdOff _) = error "event: cannot register an event on an anonymous stamp. Do not call event on the result of peek."
event s@(Stamp i e) =
    Stamp i $
        if potentiallyNew /= e
            then potentiallyNew
            else e'
  where
    potentiallyNew = fill' i e
    (e', _) = grow' s

data StampComparison
    = -- | A happened before B: the causal history of A is strictly included in that of B.
      Before
    | -- | A happened after B: the causal history of B is strictly included in that of A.
      After
    | -- | A and B have seen exactly the same events. E.g. a stamp and itself, or the two halves of a 'fork'.
      Equivalent
    | -- | A happened concurrent to B in logical time: each has seen events the other has not.
      Concurrent
    deriving (Eq, Show)

{- | Compare two stamps by the partial order of the paper (section 5.3.1) on their event components.
   With a <= b meaning "the causal history of a is included in that of b":

   * a <= b and not b <= a: 'Before'
   * b <= a and not a <= b: 'After'
   * a <= b and b <= a: 'Equivalent'
   * neither: 'Concurrent'

   Note that 'Stamp' is not an instance of Ord, because the order is partial ('Concurrent' stamps are incomparable),
   and because 'Equivalent' stamps are not necessarily equal: their identities may differ.
-}
stampCompare :: Stamp -> Stamp -> StampComparison
stampCompare (Stamp _ e1) (Stamp _ e2) =
    case (e1 `evLeq` e2, e2 `evLeq` e1) of
        (True, False) -> Before
        (False, True) -> After
        (True, True) -> Equivalent
        (False, False) -> Concurrent

{- | whether two stamps have seen exactly the same events, regardless of their identities.
   E.g. replicas with the same history are in sync, whereas 'Concurrent' replicas are in conflict.
-}
sameHistory :: Stamp -> Stamp -> Bool
(Stamp _ e1) `sameHistory` (Stamp _ e2) =
    (e1 `evLeq` e2) && (e2 `evLeq` e1)

{- | the `happened before` relation for two stamps.
   This is the strict version of the partial order from section 5.3.1 of the paper, which characterizes causality:
   @a `happenedBefore` b@ iff the causal history of @a@ is strictly included in that of @b@.
   Use this to examine causality of stamps.
-}
happenedBefore :: Stamp -> Stamp -> Bool
(Stamp _ e1) `happenedBefore` (Stamp _ e2) =
    (e1 `evLeq` e2) && not (e2 `evLeq` e1)

-- | some utility functions because the constructors are so verbose.
iF, iT :: ITCId
iF = ITCIdOff
iT = ITCIdOn

iB :: ITCId -> ITCId -> ITCId
iB = ITCIdBranch

normId :: ITCId -> ITCId
normId (ITCIdBranch ITCIdOff ITCIdOff) = iF
normId (ITCIdBranch ITCIdOn ITCIdOn) = iT
normId leaf = leaf

sumId :: ITCId -> ITCId -> ITCId
sumId ITCIdOff i = i
sumId i ITCIdOff = i
sumId (ITCIdBranch l1 r1) (ITCIdBranch l2 r2) = normId (ITCIdBranch (sumId l1 l2) (sumId r1 r2))
sumId _ _ = error "internal consistency error. Create ID's only by means of fork and join."

split :: ITCId -> (ITCId, ITCId)
split ITCIdOff = (iF, iF)
split ITCIdOn = (iB iT iF, iB iF iT)
split (ITCIdBranch ITCIdOff i) = (iB iF i1, iB iF i2) where (i1, i2) = split i
split (ITCIdBranch i ITCIdOff) = (iB i1 iF, iB i2 iF) where (i1, i2) = split i
split (ITCIdBranch l r) = (iB l iF, iB iF r)

fill' :: ITCId -> ITCEvent -> ITCEvent
fill' ITCIdOff e = e
fill' ITCIdOn e = ITCEventLeaf $ maxEv e
fill' _ n@(ITCEventLeaf _) = n
fill' (ITCIdBranch ITCIdOn ir) (ITCEventBranch n l r) =
    normEv $
        ITCEventBranch
            n
            (ITCEventLeaf (max (maxEv l) (minEv r')))
            r'
  where
    r' = fill' ir r
fill' (ITCIdBranch il ITCIdOn) (ITCEventBranch n l r) =
    normEv $
        ITCEventBranch
            n
            l'
            (ITCEventLeaf (max (maxEv r) (minEv l')))
  where
    l' = fill' il l
fill' (ITCIdBranch il ir) (ITCEventBranch n l r) = normEv $ ITCEventBranch n (fill' il l) (fill' ir r)

{- | Cost of a grow: number of leaf expansions first, then number of tree levels descended.
   The paper encodes this as a single integer with a "large constant" N per expansion, which is only correct
   while trees stay shallower than N, and notes that a pair under lexicographic order avoids the constant.
   The derived Ord instance is that lexicographic order.
-}
data Cost = Cost !Integer !Integer deriving (Eq, Ord)

ltCost :: Cost -> Cost -> Bool
ltCost = (<)

addCost :: Cost -> Cost -> Cost
addCost (Cost x1 d1) (Cost x2 d2) = Cost (x1 + x2) (d1 + d2)

noCost, expansionCost, descentCost :: Cost
noCost = Cost 0 0
expansionCost = Cost 1 0
descentCost = Cost 0 1

grow' :: Stamp -> (ITCEvent, Cost)
grow' (Stamp ITCIdOn (ITCEventLeaf n)) =
    (ITCEventLeaf $ n + 1, noCost)
grow' (Stamp i (ITCEventLeaf n)) =
    (e', c `addCost` expansionCost)
  where
    (e', c) = grow' $ Stamp i (ITCEventBranch n (ITCEventLeaf 0) (ITCEventLeaf 0))
grow' (Stamp (ITCIdBranch ITCIdOff i) (ITCEventBranch n l r)) =
    (ITCEventBranch n l r', cr `addCost` descentCost)
  where
    (r', cr) = grow' $ Stamp i r
grow' (Stamp (ITCIdBranch i ITCIdOff) (ITCEventBranch n l r)) =
    ((ITCEventBranch n l' r), cl `addCost` descentCost)
  where
    (l', cl) = grow' $ Stamp i l
grow' (Stamp (ITCIdBranch il ir) (ITCEventBranch n l r))
    | costL `ltCost` costR = ((ITCEventBranch n l' r), costL `addCost` descentCost)
    | otherwise = ((ITCEventBranch n l r'), costR `addCost` descentCost)
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
