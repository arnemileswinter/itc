module Data.Clock.IntervalTreeSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Control.Exception (evaluate)
import Control.Monad (foldM)

import Data.Clock.IntervalTree
import Data.Clock.IntervalTree.Format

newtype ArbitraryITC = ArbitraryITC Stamp

{- | The arbitrary instance for ITC.
   Note the newtype wrapper to not pollute library code with test dependency.
   Note that this applies `event` and `fork` on the seed stamp,
   both operations must be tested before usage.
-}
instance Arbitrary ArbitraryITC where
    arbitrary = fmap ArbitraryITC $ do
        let initial = seed
        let genOp = elements [event, fst . fork, snd . fork] :: (Gen (Stamp -> Stamp))
        i <- chooseInt (0, 200)
        foldM (fmap . flip id) initial $ replicate i genOp

{- | A whole system of live stamps with pairwise disjoint ids, evolved from the seed
   by random `event`, `fork` and `join` steps. Each stamp is consumed at most once,
   so that, unlike in `ArbitraryITC`, joined histories are exercised.
   Invariant: joining all stamps of the system yields the full id.
-}
newtype ArbitrarySystem = ArbitrarySystem [Stamp]

instance Arbitrary ArbitrarySystem where
    arbitrary = fmap ArbitrarySystem $ do
        n <- chooseInt (0, 200)
        foldM (const . step) [seed] $ replicate n ()
      where
        pick xs = do
            k <- chooseInt (0, length xs - 1)
            pure (xs !! k, take k xs <> drop (k + 1) xs)
        step :: [Stamp] -> Gen [Stamp]
        step stamps = do
            (s, rest) <- pick stamps
            op <- chooseInt (0, if null rest then 1 else 2 :: Int)
            case op of
                0 -> pure $ event s : rest
                1 -> let (a, b) = fork s in pure $ a : b : rest
                _ -> do
                    (s', rest') <- pick rest
                    pure $ join s s' : rest'

instance Show ArbitrarySystem where
    show (ArbitrarySystem ss) = "ArbitrarySystem " <> show (map fmtStamp ss)

flipped :: StampComparison -> StampComparison
flipped Before = After
flipped After = Before
flipped c = c

-- | Runs the expectation on the first two stamps of a system, discarding systems of only one stamp.
withTwo :: [Stamp] -> (Stamp -> Stamp -> Expectation) -> Property
withTwo (a : b : _) f = property $ f a b
withTwo _ _ = property Discard

{- | Show instance for test subjects.
   Note that this is for human-friendly output and violates Read.
-}
instance Show ArbitraryITC where
    show (ArbitraryITC i) = "ArbitraryITC " <> (fmtStamp i)

spec :: Spec
spec = do
    describe "stampCompare" $ do
        prop "is equivalent in self-comparison" $ \(ArbitraryITC stamp) ->
            (stamp `stampCompare` stamp) `shouldBe` Equivalent
        prop "agrees with happenedBefore and sameHistory" $ \(ArbitrarySystem stamps) ->
            sequence_
                [ case a `stampCompare` b of
                    Before -> (a `happenedBefore` b, b `happenedBefore` a, a `sameHistory` b) `shouldBe` (True, False, False)
                    After -> (a `happenedBefore` b, b `happenedBefore` a, a `sameHistory` b) `shouldBe` (False, True, False)
                    Equivalent -> (a `happenedBefore` b, b `happenedBefore` a, a `sameHistory` b) `shouldBe` (False, False, True)
                    Concurrent -> (a `happenedBefore` b, b `happenedBefore` a, a `sameHistory` b) `shouldBe` (False, False, False)
                | a <- take 6 stamps
                , b <- take 6 stamps
                ]
        prop "flips Before and After when the arguments are swapped" $ \(ArbitrarySystem stamps) ->
            sequence_
                [ (b `stampCompare` a) `shouldBe` flipped (a `stampCompare` b)
                | a <- take 6 stamps
                , b <- take 6 stamps
                ]
        prop "is Before in comparison to evented" $ \(ArbitraryITC stamp) ->
            (stamp `stampCompare` (event stamp)) `shouldBe` Before
        prop "is After in comparison to evented" $ \(ArbitraryITC stamp) ->
            ((event stamp) `stampCompare` stamp) `shouldBe` After
    describe "fork" $ do
        prop "forks are unequal to their parent" $ \(ArbitraryITC stamp) ->
            let (s1, s2) = fork stamp
             in s1 /= stamp && s2 /= stamp
        prop "forked stamps are unequal to each other" $ \(ArbitraryITC stamp) ->
            let (s1, s2) = fork stamp
             in s1 /= s2
        prop "forks are equivalent to their parent" $ \(ArbitraryITC stamp) ->
            let (s1, s2) = fork stamp
             in sequence_ $
                    fmap (`shouldBe` Equivalent) $
                        [ s1 `stampCompare` stamp
                        , stamp `stampCompare` s1
                        , s2 `stampCompare` stamp
                        , stamp `stampCompare` s2
                        ]
        prop "forks are equivalent to each other" $ \(ArbitraryITC stamp) ->
            let (s1, s2) = fork stamp
             in sequence_ $
                    fmap (`shouldBe` Equivalent) $
                        [ s1 `stampCompare` s2
                        , s2 `stampCompare` s1
                        ]

    describe "event" $ do
        prop "increases causality" $ \(ArbitraryITC stamp) ->
            let (s1, s2) = fork stamp
                s2' = event s2
             in sequence_ $
                    [ s1 `stampCompare` s2' `shouldBe` Before
                    , stamp `stampCompare` s2' `shouldBe` Before
                    , s2 `stampCompare` s2' `shouldBe` Before
                    ]
        prop "makes forked trees concurrent if both evented" $ \(ArbitraryITC stamp) ->
            let (s1, s2) = fork stamp
                s1' = event s1
                s2' = event s2
             in s1' `stampCompare` s2' `shouldBe` Concurrent
    describe "join" $ do
        prop "is inverse to fork" $ \(ArbitraryITC stamp) ->
            (uncurry join $ fork stamp) `shouldBe` stamp
        prop "keeps causality" $ \(ArbitraryITC stamp) ->
            let (s1, s2) = fork stamp
                s2' = event s2
                stamp' = join s1 s2'
             in sequence_
                    [ stamp `stampCompare` stamp' `shouldBe` Before
                    , s1 `stampCompare` stamp' `shouldBe` Before
                    , s2' `stampCompare` stamp' `shouldBe` Equivalent
                    , s2' `stampCompare` (event stamp') `shouldBe` Before
                    ]
        prop "of a whole system has the full id and dominates every member" $ \(ArbitrarySystem stamps) ->
            let joined@(Stamp i _) = foldr1 join stamps
             in sequence_ $
                    (i `shouldBe` ITCIdOn) :
                    [(s `stampCompare` joined) `shouldNotBe` After | s <- stamps]
        prop "of two members is not before either of them" $ \(ArbitrarySystem stamps) ->
            withTwo stamps $ \a b ->
                let j = join a b
                 in sequence_
                        [ (j `stampCompare` a) `shouldNotBe` Before
                        , (j `stampCompare` b) `shouldNotBe` Before
                        , (a `stampCompare` event j) `shouldBe` Before
                        , (b `stampCompare` event j) `shouldBe` Before
                        ]
        prop "of a stamp with its own anonymous copy is equivalent to it" $ \(ArbitrarySystem stamps) ->
            sequence_ [(join s (peek s) `stampCompare` s) `shouldBe` Equivalent | s <- stamps]
        prop "with an anonymous stamp keeps the id" $ \(ArbitrarySystem stamps) ->
            withTwo stamps $ \a@(Stamp i _) b ->
                let Stamp i' _ = join a (peek b)
                 in i' `shouldBe` i
        it "errors on overlapping ids" $
            evaluate (let Stamp i _ = join seed seed in i) `shouldThrow` anyErrorCall
    describe "event on joined histories" $ do
        prop "strictly advances every member of a system" $ \(ArbitrarySystem stamps) ->
            sequence_ [(s `stampCompare` event s) `shouldBe` Before | s <- stamps]
        prop "is concurrent to events of other members" $ \(ArbitrarySystem stamps) ->
            withTwo stamps $ \a b ->
                (event a `stampCompare` event b) `shouldBe` Concurrent
        it "errors on anonymous stamps" $
            evaluate (event (peek seed)) `shouldThrow` errorCall "event: cannot register an event on an anonymous stamp. Do not call event on the result of peek."
    describe "happenedBefore" $ do
        prop "is false for self-comparison" $ \(ArbitraryITC stamp) ->
            not $ stamp `happenedBefore` stamp
        prop "is true when applied to evented after join" $ \(ArbitraryITC stamp) ->
            stamp `shouldSatisfy` (`happenedBefore` (event stamp))

printTikzExample :: IO ()
printTikzExample =
    let a = seed
        (b, h) = fork a
        c = event b
        i = event h
        (d, j) = fork c
        e = event d
        k = event i
        l = join j k
        (m, n) = fork l
        f = join e m
        g = event f

        tikzpic letter itc = "\\newcommand{\\itc" <> [letter] <> "}{" <> fmtStampTikz itc <> "}\n"
     in do
            putStrLn $ concat $ zipWith (tikzpic) ['a' ..] [a, b, c, d, e, f, g, h, i, j, k, l, m, n]
