{-# LANGUAGE OverloadedStrings #-}

module Ammi.Types where

import Data.Text (Text)
import qualified Data.ByteString as BS

type TextPos = Integer

type TextBlock = [TextBlockElement]

data TextBlockElement
  -- | Text to be displayed.
  = TBEText Text

  -- | Text command (e.g. pause, play voice line).
  | TBECmd TextBlockCommand

  -- | Arbitrary bytestring to insert in the text block.
  --
  -- Intended for non-decoded text commands, and hacks.
  | TBEBytes BS.ByteString
    deriving (Eq, Ord, Show)

data TextBlockCommand
  -- | Pause for a given number of milliseconds.
  --
  -- Note that the Amagami script format seems to only allow 0-990 milliseconds,
  -- in steps of 10 (e.g. 150, 160).
  = TBCPause Integer
    deriving (Eq, Ord, Show)

-- | Text block compilation error, with format-specific block error type @b@ and
-- element error type @e@.
--
-- An error may be either supported by Ammi's base feature set and thus common
-- to all formats, or format-specific. In the format-specific case, the format
-- does its own error handling (with some Ammi annotations).
--
-- An error may be block-scope or element-scope. This adds some complexity:
-- there are a total of 4 error classes which Ammi manages. A little over the
-- top, but it's all in the name of good typing.
--
-- Note that this error is in the context of the compilation target's
-- 'TargetAttr' - they should be kept/used/exported together.
data TextBlockErr b e
  -- | Block error common to all Ammi formats.
  = TBErrCommonBlock           TextBlockErrCommonBlock

  -- | Element error common to all Ammi formats.
  | TBErrCommonElement Integer TextBlockErrCommonElement

  -- | Block error specific to target format, not considered related to Ammi.
  --
  -- e.g. block byte count too high (not a constraint in Ammi)
  | TBErrFormatBlock b

  -- | Element error specific to target format, not considered related to Ammi.
  --
  -- e.g. unsupported char (Ammi doesn't particularly restrict printing any
  -- Unicode)
  | TBErrFormatElement e
    deriving (Eq, Ord, Show) -- TODO: will this work? lol

-- | Common/known text block compilation error.
data TextBlockErrCommonBlock
  -- | Format does not support that many lines.
  = TBECBTooManyLines Integer
    deriving (Eq, Ord, Show)

-- | Common/known text element compilation error.
data TextBlockErrCommonElement
  -- | A line has overflowed the format's max column.
  = TBECELineTooLong TextPos

  -- | Format does not support given 'TextBlockElement'.
  | TBECEUnsupported TextBlockElement
    deriving (Eq, Ord, Show)

data TargetAttr = TargetAttr
  { tAttrLines :: Integer
  , tAttrCols  :: Integer
  } deriving (Eq, Ord, Show)
