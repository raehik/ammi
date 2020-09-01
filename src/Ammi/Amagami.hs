{-# LANGUAGE OverloadedStrings #-}

module Ammi.Amagami where

import Ammi.Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Builder
import Data.Text (Text)
import Data.Text.Encoding
import Control.Monad.Trans.Except

type Compiler a = StateT CompileState (Except Text) a

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

-- | Amagami attributes.
--
-- Needs testing. Currently for halfwidth, default text size, no weird shit.
attrs :: TargetAttr
attrs = TargetAttr
  { tAttrLines = 3
  , tAttrCols  = 44
  }

-- | Try to compile an Ammi text block into an ASCF text block.
--
-- TODO: want to tidy this up...
compileTextBlock :: TextBlock -> Compiler Either Text BL.ByteString
compileTextBlock tblk =
    case compileTextBlockContent tblk of
        Left  err     -> err
        Right content -> toLazyByteString . decorateTextBlock $ content

-- | Decorate an ASCF text block with its header.
--
-- Header is 0x05 LENGTH 0x00.
decorateTextBlock :: (Builder, Word8) -> Builder
decorateTextBlock (content, len) = header <> content
  where
    header      = mconcat . map word8 $ headerBytes
    headerBytes = [0x05, len, 0x00]

data CompileState = CompileState
  { cmpStBEIdx  :: Integer
  , cmpStCursor :: Integer, Integer
  } deriving (Eq, Ord, Show)

cmpStDef = CompileState
  { cmpStBEIdx  = 0
  , cmpStCursor = (0, 0)
  , cmpSt
  }

compileTextBlockContent :: TextBlock -> Either Text (Builder, Word8)
compileTextBlockContent tblk =
    case runExcept . flip runStateT cmpStDef f of


    runExcept $ do
    (content, len) <- compileTextBlockContent tblk
    let header = word8 0x05 <> int8 (fromIntegral len) <> word8 0x00
    return . toLazyByteString $ header <> content

encodeAmagami' :: Integer -> Integer -> Builder -> TextBlock -> Except TextBlockErrAmagami (Builder, Integer)
encodeAmagami' idx bytes builder block
  -- Amagami script format stores text block length in a single byte, and the
  -- header takes up 3 bytes
  | bytes > (255-3) = err $ TBEBByteCountTooLarge bytes
  | otherwise =
        case block of
            []     -> return (builder, bytes)
            (p:ps) -> do
                partBytes <- wrapPartErr $ renderPart p
                encodeAmagami' (idx+1) (bytes+fromIntegral (BS.length partBytes)) (builder <> byteString partBytes) ps
  where
    err e = throwE (idx, e)
    wrapPartErr = withExcept $ \e -> (idx, e)

renderPart :: TextBlockElement -> Except a BS.ByteString
renderPart (TBEText   t) = return $ encodeUtf8 t
renderPart (TBEBytes bs) = return bs
renderPart _ = throwE $ TBEBByteCountTooLarge 0
