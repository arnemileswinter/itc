module Data.Clock.IntervalTreeSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

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
        i <- chooseInt (0, 10)
        foldM (fmap . flip id) initial $ replicate i genOp

{- | Show instance for test subjects.
   Note that this is for human-friendly output and violates Read.
-}
instance Show ArbitraryITC where
    show (ArbitraryITC i) = "ArbitraryITC " <> (fmtStamp i)

spec :: Spec
spec = do
    describe "stampCompare" $ do
        prop "is concurrent in self-comparison" $ \(ArbitraryITC stamp) ->
            (stamp `stampCompare` stamp) `shouldBe` Concurrent
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
        prop "forks are concurrent to their parent" $ \(ArbitraryITC stamp) ->
            let (s1, s2) = fork stamp
             in sequence_ $
                    fmap (`shouldBe` Concurrent) $
                        [ s1 `stampCompare` stamp
                        , stamp `stampCompare` s1
                        , s2 `stampCompare` stamp
                        , stamp `stampCompare` s2
                        ]
        prop "forks are concurrent to each other" $ \(ArbitraryITC stamp) ->
            let (s1, s2) = fork stamp
             in sequence_ $
                    fmap (`shouldBe` Concurrent) $
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
                    , s2' `stampCompare` stamp' `shouldBe` Concurrent
                    , s2' `stampCompare` (event stamp') `shouldBe` Before
                    ]
    describe "happenedBefore" $ do
        prop "is false for self-comparison" $ \(ArbitraryITC stamp) ->
            not $ stamp `happenedBefore` stamp
        prop "is true when applied to evented after join" $ \(ArbitraryITC stamp) ->
            stamp `shouldSatisfy` (`happenedBefore` (event stamp))

printTikzExample :: IO ()
printTikzExample =
  let
    a = seed
    (b,h) = fork a
    c = event b
    i = event h
    (d,j) = fork c
    e = event d
    k = event i
    l = join j k
    (m,n) = fork l
    f = join e m
    g = event f

    tikzpic letter itc = "\\newcommand{\\itc"<>[letter]<>"}{" <> fmtStampTikz itc <> "}\n"
  in do
    putStrLn $ concat $ zipWith (tikzpic) ['a'..] [a,b,c,d,e,f,g,h,i,j,k,l,m,n]
