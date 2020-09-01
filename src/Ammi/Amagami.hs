{-# LANGUAGE OverloadedStrings #-}

module Ammi.Amagami where

import Ammi.Types

import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Builder
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

type Compiler a = StateT CompileState (Except Text) a

data CompileState = CompileState
  { cmpStBEIdx     :: Integer
  , cmpStCursor    :: (Integer, Integer)
  , cmpStByteCount :: Integer
  } deriving (Eq, Ord, Show)

cmpStDef :: CompileState
cmpStDef = CompileState
  { cmpStBEIdx     = 0
  , cmpStCursor    = (0, 0)
  , cmpStByteCount = 0
  }

attrs :: TargetAttr
attrs = TargetAttr
  { tAttrLines = 3
  , tAttrCols  = 44
  }

runCompiler :: Compiler a -> Either Text a
runCompiler = runExcept . flip evalStateT cmpStDef

-- | Try to compile an Ammi text block into an ASCF text block.
compileTextBlock :: TextBlock -> Compiler BL.ByteString
compileTextBlock tblk = do
    -- compile content
    content <- compileTextBlockContent tblk

    -- decorate with header
    -- TODO: we assume the previous function ensured <=255
    bytes <- gets cmpStByteCount
    let blk = decorateTextBlock (content, fromIntegral bytes)

    -- convert to bytes
    return $ toLazyByteString blk

-- | Decorate an ASCF text block content with its header.
--
-- Header is @0x05 LENGTH 0x00@.
decorateTextBlock :: (Builder, Word8) -> Builder
decorateTextBlock (content, len) = header <> content
  where
    header      = mconcat . map word8 $ headerBytes
    headerBytes = [0x05, len, 0x00]

compileTextBlockContent :: TextBlock -> Compiler Builder
compileTextBlockContent = flip foldM mempty $ \block el -> do
    -- compile element
    elCompiled <- compileTextBlockElement el

    -- combine with rest of block
    return $ block <> elCompiled

compileTextBlockElement :: TextBlockElement -> Compiler Builder
compileTextBlockElement (TBEText    t) =
    -- cheat: just encode and pass to bytestring handler (b/c identical for now)
    compileTextBlockElement $ TBEBytes $ encodeUtf8 t
compileTextBlockElement (TBEBytes  bs) = do
    addBytes $ fromIntegral $ BS.length bs
    return $ byteString bs
compileTextBlockElement (TBECmd   cmd) = compileTextBlockCommand cmd

compileTextBlockCommand :: TextBlockCommand -> Compiler Builder
compileTextBlockCommand (TBCPause ms) = compileTBCPause ms

-- originally wrote my own, then realised the Data.ByteString.Builder provides
-- one which is probably faster so. Safety not guaranteed through my own code,
-- though.
compileTBCPause :: Integer -> Compiler Builder
compileTBCPause ms = do
    let (value, digit1) = divMod ms 10
    throwIf (value > 99)  $ "cmd: pause: too large for ASCF: "    <> tshow ms
    throwIf (digit1 /= 0) $ "cmd: pause: unsupported precision: " <> tshow ms
    addBytes 3
    return $ char7 'W' <> integerDec value

addBytes :: Integer -> Compiler ()
addBytes n = modify $ \s -> s { cmpStByteCount = cmpStByteCount s + n }

tshow = T.pack . show

-- TODO: similar to @guardExceptT@ in from-sum
throwIf pred err = if pred then lift . throwE $ err else return ()

digs :: Integral x => x -> [x]
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)

{-
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
