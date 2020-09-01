{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LiberalTypeSynonyms #-}

module Ammi.Compile where

import Ammi.Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Builder
import Data.Text (Text)
import Data.Text.Encoding
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

-- | Inner compiler monad for formats, providing access to format state @s@ and
--   exception type @e@.
type FmtCompiler s e a = StateT s (Except e) a

-- | Compiler monad with format state @s@ and exception type @e@. Holds a
--   'FmtCompiler' (seemingly not able to do in type synonyms).
type Compiler s e a = ReaderT TargetAttr (StateT s (Except e)) a

-- | Run format compiler with given initial state.
runFmtCompiler :: s -> FmtCompiler s e a -> Either e (a, s)
runFmtCompiler s = runExcept . flip runStateT s

-- | Run compiler with given 'TargetAttr' and initial state.
--
-- TODO: state should be format state + index, current output
runCompiler :: TargetAttr -> s -> Compiler s e a -> Either e (a, s)
runCompiler tAttr s = runFmtCompiler s . flip runReaderT tAttr

-- | Compile a text block to a given format.
compile
    :: TargetAttr
    -> s
    -> a
    -> (TextBlockElement -> FmtCompiler (s, a) (Either b e) a)
    -> TextBlock
    -> Either (TextBlockErr b e) (a, s)
compile tAttr s _ _ _ = runCompiler tAttr s $ do
    compileErr $ TBErrCommonElement 0 $ TBECELineTooLong 0

compileErr :: e -> Compiler s e a
compileErr = lift . lift . throwE

exTB :: TextBlock
exTB = []

exTBEFunc :: TextBlockElement -> FmtCompiler (s, Integer) (Either b e) Integer
exTBEFunc _ = return 0

exTA :: TargetAttr
exTA = TargetAttr
  { tAttrLines = 3
  , tAttrCols  = 44
  }
