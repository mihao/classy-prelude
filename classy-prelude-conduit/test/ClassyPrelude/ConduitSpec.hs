{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module ClassyPrelude.ConduitSpec where

import ClassyPrelude.Conduit
import Test.Hspec
import Data.Functor.Identity (runIdentity)
import qualified Data.Conduit.List as CL

skip :: Monad m => m a -> m ()
skip _ = return ()

naiveDrop :: (Monad m, ConduitChunk c) => Int -> Consumer c m ()
naiveDrop = error "FIXME" -- unwrapInput . CL.drop

spec :: Spec
spec = do
    describe "enumFromTo + fold" $ do
        it "Identity" $ do
            res <- conEnumFromTo 1 (10 :: Int) $$ asIdentitySink (conFold (+) 0)
            res `shouldBe` sum [1..10]
        it "ByteString" $ do
            res <- conEnumFromTo 1 10 $$ asByteStringSink (conFold (+) 0)
            res `shouldBe` sum [1..10]
    describe "leftovers" $ do
        it "Identity" $ do
            res <- wrapOutput (mapM_ yield ("Hello World Again" :: String)) $$ asIdentitySink (do
                conDrop 6
                consume)
            map runIdentity res `shouldBe` "World Again"
        it "ByteString" $ do
            res <- (yield "Hello World" >> yield " Again") $$ do
                conDrop 6
                consume
            fromChunks res `shouldBe` asLByteString "World Again"
        skip $ it "naive ByteString" $ do -- FIXME
            res <- (yield "Hello World" >> yield " Again") $$ do
                naiveDrop 6
                consume
            fromChunks res `shouldBe` asLByteString "World Again"
