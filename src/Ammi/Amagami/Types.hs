{-# LANGUAGE OverloadedStrings #-}

module Ammi.Amagami.Types where

import Data.Text (Text)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

-- | Script compiler monad.
type Compiler a = StateT CompileState (ExceptT Text IO) a

-- | Script text compiler monad.
--
-- IO is required for text encoding conversion via the ICU library (the Haskell
-- one works via bindings).
type TextCompiler a = StateT TextCompileState (ExceptT Text IO) a

-- | Command compiler monad.
--
-- Barebones. In particular, removed IO.
type CommandCompiler a = StateT Integer (Except Text) a

data CompileState = CompileState
  { cmpStByteCount :: Integer
  } deriving (Eq, Ord, Show)

data TextCompileState = TextCompileState
  { tCmpStByteCount :: Integer
  , tCmpStLine      :: Integer
  , tCmpStCol       :: Integer
  } deriving (Eq, Ord, Show)

{-
-- | Error encountered while compiling an Ammi text block to ASCF.
data TextBlockErrBlock
  -- | Total byte count for the text block is too large (> 255).
  --
  -- ASCF text blocks have a header which includes a single byte indicating the
  -- content length. Thus, text block content must be <=255 bytes total.
  -- (Unlikely to be an issue, due to commands being pretty short and textboxes
  -- only supporting 3x44 halfwidth characters.)
  = TBEBByteCountTooLarge Integer
    deriving (Eq, Ord, Show)

-- | Element error encountered while compiling an Ammi text block to Amagami
--   script format.
data TextBlockErrElement
  -- | Unprintable character at given 'TextPos'.
  --
  -- ASCF text blocks use Shift JIS, except the ASCII range isn't printed and is
  -- read as commands instead.
  = TBEEUnsupportedChar TextPos Char
    deriving (Eq, Ord, Show)
-}
