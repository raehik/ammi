{-# LANGUAGE OverloadedStrings #-}

module Ammi where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Builder
import Data.Text (Text)
import Data.Text.Encoding
import Control.Monad.Trans.Except

-- | Error wrapper including the block index of the erroneous part.
type STBErr = (Integer, STBErr')
data STBErr'
  = STBErrTotalBytesTooLarge Integer
  | STBErrUnsupportedChar Char
  | STBErrPartRendererUnimplemented SText
    deriving (Eq, Ord, Show)

-- | A script text block.
type STBlock = [SText]

-- | A part in a script text block.
data SText
  -- | Text to be displayed.
  = STText Text

  -- | Command: Wait for a given number of milliseconds.
  --
  -- Note that the Amagami script format seems to only allow 0-990 milliseconds,
  -- in steps of 10 (e.g. 150, 160).
  | STCWait Integer

  -- | Arbitrary bytestring for non-decoded script text features and/or hacks.
  | STBytes BS.ByteString
    deriving (Eq, Ord, Show)

-- | Encode a script text block into the Amagami script format.
encodeAmagami :: STBlock -> Either STBErr BL.ByteString
encodeAmagami stblock = runExcept $ do
    (stbContent, length) <- encodeAmagami' 0 0 mempty stblock
    let stbHeader = word8 0x05 <> int8 (fromIntegral length) <> word8 0x00
    return . toLazyByteString $ stbHeader <> stbContent

encodeAmagami' :: Integer -> Integer -> Builder -> STBlock -> Except STBErr (Builder, Integer)
encodeAmagami' idx bytes builder block
  -- Amagami script format stores text block length in a single byte, and the
  -- header takes up 3 bytes
  | bytes > (255-3) = err $ STBErrTotalBytesTooLarge bytes
  | otherwise =
        case block of
            []     -> return (builder, bytes)
            (p:ps) -> do
                partBytes <- wrapPartErr $ renderPart p
                encodeAmagami' (idx+1) (bytes+fromIntegral (BS.length partBytes)) (builder <> byteString partBytes) ps
  where
    err e = throwE (idx, e)
    wrapPartErr = withExcept $ \e -> (idx, e)

renderPart :: SText -> Except STBErr' BS.ByteString
renderPart (STText   t) = return $ encodeUtf8 t
renderPart (STBytes bs) = return bs
renderPart p = throwE $ STBErrPartRendererUnimplemented p


{-
encodeAmagami stBlock =
    let bytes = foldr (BL.append . encodePart) BL.empty stBlock
     --in revprependBS bytes [0x05, BL.length bytes, 0x00]
     in revprependBS bytes $ BLC.pack [0x05, 0x00, 0x00]
  where
    encodePart :: SText -> BL.ByteString
    encodePart _ = BL.empty
    revprependBS bs1 bs2 = foldr (BL.cons) bs1 $ BLC.pack bs2
-}
