{-# LANGUAGE LambdaCase #-}

-- |
-- Module      :  Data.Attoparsec.Machine
-- Copyright   :  Lodvær 2015
-- License     :  BSD3
--
-- Maintainer  :  Lodvær <lodvaer@gmail.com>
-- Stability   :  provisional
-- Portability :  unknown
--
-- Attoparsec machine fittings.
module Data.Attoparsec.Machine
    ( ParserAutomaton(..)
    , Result(..)
    , ignoreParserError
    , stopOnParserError
    ) where

import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.Internal.Types as AI
import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString as B
import Data.Machine
import qualified Data.Text as T

-- $setup
-- >>> import Control.Applicative
-- >>> import Data.Attoparsec.ByteString
-- >>> import Data.Attoparsec.ByteString.Char8
-- >>> :set -XOverloadedStrings


-- | @'AI.Parser' -> 'Process'@ conversion.
class ParserAutomaton i where
    -- | Treats the input likes a continuous stream.
    --
    -- When the parser starves it asks for more from upstream. When it finishes
    -- it restarts. If the parser fails or succeeds consuming no input it will
    -- yield results forever.
    --
    -- Examples:
    --
    -- >>> let input = supply ["hel", "lo world"]
    -- >>> runT . input $ autoStreamParser (string "hello") ~> taking 4
    -- [Success "hello",Fail [] "string",Fail [] "string",Fail [] "string"]
    -- >>> runT . input $ autoStreamParser (many1 anyChar)
    -- [Success "hello world"]
    --
    autoStreamParser :: Monad m
                     => AI.Parser i a
                     -> ProcessT m i (Result a)

    -- | Treats the input stream like packets and produces only a single parse
    -- result per upstream 'Yield'.
    --
    -- When there are leftovers in the current packet after the parse is done,
    -- they are dropped. Include it in your parse result if you want it.
    --
    -- Examples:
    --
    -- >>> let input = supply ["hello", "", "world", "!"]
    -- >>> runT . input $ autoPacketParser anyChar
    -- [Success 'h',Fail [] "not enough input",Success 'w',Success '!']
    -- >>> runT . input $ autoPacketParser ((,) <$> anyChar <*> takeByteString)
    -- [Success ('h',"ello"),Fail [] "not enough input",Success ('w',"orld"),Success ('!',"")]
    -- >>> runT . input $ autoPacketParser takeByteString
    -- [Success "hello",Success "",Success "world",Success "!"]
    --
    autoPacketParser :: Monad m
                     => AI.Parser i a
                     -> ProcessT m i (Result a)

instance ParserAutomaton B.ByteString where
    autoStreamParser = auxStream . AB.parse
    autoPacketParser = auxPacket . AB.parse

instance ParserAutomaton T.Text where
    autoStreamParser = auxStream . AT.parse
    autoPacketParser = auxPacket . AT.parse

-- | The result of an @'AI.Parser'@.
data Result a
    = Fail [String] String
      -- ^ A list of contexts where the parse went wrong and an error message,
      -- if any.
    | Success a
      deriving (Read, Show, Eq, Ord)

instance Functor Result where
    fmap f (Success x) = Success $ f x
    fmap _ (Fail a b) = Fail a b

-- | Ignores 'Fail' and requests the next one.
ignoreParserError :: Monad m => ProcessT m (Result a) a
ignoreParserError = repeatedly $ await >>= \case Fail _ _ -> return ()
                                                 Success a -> yield a

-- | Stops the parser when it fails.
--
-- >>> runT . supply ["hia"] $ autoStreamParser (string "hi") ~> stopOnParserError
-- [Success "hi",Fail [] "string"]
--
stopOnParserError :: Monad m => ProcessT m (Result a) (Result a)
stopOnParserError = repeatedly $ await >>= \f -> do
    yield f
    case f of Fail _ _ -> stop
              _ -> return()

auxStream :: (Eq i, Monoid i, Monad m)
          => (i -> AI.IResult i a) -> ProcessT m i (Result a)
auxStream f = encased . Await (go . f) Refl . go $ f mempty
  where
    go = encased . \case
        AI.Fail _ _ "not enough input" ->
            Stop
        AI.Fail i ctxs err ->
            Yield (Fail ctxs err) (go $ f i)
        AI.Partial f' ->
            Await (go . f') Refl (go $ f' mempty)
        AI.Done i r ->
            Yield (Success r) $ if i == mempty then auxStream f else go $ f i

auxPacket :: (Eq i, Monoid i, Monad m)
          => (i -> AI.IResult i a) -> ProcessT m i (Result a)
auxPacket f = encased $ Await (go . f) Refl stopped
  where
    go = \case
        AI.Fail _ ctxs err ->
            encased . Yield (Fail ctxs err) $ auxPacket f
        AI.Partial f' ->
            go (f' mempty)
        AI.Done _ r ->
            encased . Yield (Success r) $ auxPacket f
