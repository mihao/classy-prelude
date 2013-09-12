{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module ClassyPrelude.Conduit
    ( -- * Re-export
      module ClassyPrelude
    , module Data.Conduit
    , module Data.Conduit.List
      -- * This module
    , module ClassyPrelude.Conduit
    ) where

import ClassyPrelude

import Data.Conduit
import Data.Conduit.List (consume, sourceNull, sinkNull)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Data.Functor.Identity (Identity (..))

class MonoFoldable c => ConduitChunk c where
    type MultiChunk c

    wrapOutput :: Monad m => ConduitM i (Element c) m r -> ConduitM i c m r

    --unwrapInput :: Monad m => ConduitM (Element c) o m r -> ConduitM c o m r -- FIXME this needs to be leftover-safe

    conUnfold :: Monad m => (b -> Maybe (Element c, b)) -> b -> Producer m c
    conUnfold f b = wrapOutput (CL.unfold f b)

    conIterate :: Monad m => (Element c -> Element c) -> Element c -> Producer m c
    conIterate f a = wrapOutput (CL.iterate f a)

    conFold :: Monad m => (b -> Element c -> b) -> b -> Consumer c m b

    conFoldMap :: (Monad m, Monoid b) => (Element c -> b) -> Consumer c m b
    conFoldMap f = conFold (\m e -> m <> f e) mempty

    conTake :: Monad m => Int -> Consumer c m (MultiChunk c)

    conDrop :: Monad m => Int -> Consumer c m ()
    --conDrop i = unwrapInput (CL.drop i)

    conHead :: Monad m => Consumer c m (Maybe (Element c))
    conPeek :: Monad m => Consumer c m (Maybe (Element c))
    conConsume :: Monad m => Consumer c m (MultiChunk c)

    conFoldM :: Monad m => (b -> Element c -> m b) -> b -> Consumer c m b
    conMapM_ :: Monad m => (Element c -> m ()) -> Consumer c m ()
    conMapM_ = CL.mapM_ . mapM_

instance ConduitChunk (Identity a) where
    type MultiChunk (Identity a) = [a]

    wrapOutput = mapOutput Identity
    conFold f b = unwrapInput (CL.fold f b)
    conTake = unwrapInput . CL.take
    conDrop i = unwrapInput (CL.drop i)
    conHead = unwrapInput await
    conPeek = unwrapInput CL.peek
    conConsume = unwrapInput CL.consume
    conFoldM f = unwrapInput . CL.foldM f

    -- needs more thought conMap :: 

unwrapInput :: Monad m => ConduitM i o m r -> ConduitM (Identity i) o m r
unwrapInput = mapInput runIdentity (Just . Identity)

asIdentitySink :: Sink (Identity a) m b -> Sink (Identity a) m b
asIdentitySink = id

instance ConduitChunk ByteString where
    type MultiChunk ByteString = LByteString

    wrapOutput = mapOutput singleton

    conFold f = CL.fold (foldl' f)

    conTake = CB.take
    conDrop = CB.drop
    conHead = CB.head
    conPeek =
        loop
      where
        loop = await >>= maybe (return Nothing) go
        go bs =
            case uncons bs of
                Nothing -> loop
                Just (w, _) -> leftover bs >> return (Just w)
    conConsume = fmap fromChunks CL.consume
    conFoldM f = CL.foldM (foldM f)

asByteStringSink :: Sink ByteString m b -> Sink ByteString m b
asByteStringSink = id

conEnumFromTo :: (Enum (Element c), Eq (Element c), Monad m, ConduitChunk c)
              => Element c
              -> Element c
              -> Producer m c
conEnumFromTo a b = wrapOutput (CL.enumFromTo a b)
