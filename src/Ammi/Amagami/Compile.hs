{-# LANGUAGE OverloadedStrings #-}

module Ammi.Amagami.Compile where

import Ammi.Types
import Ammi.Amagami.Types

import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Builder
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.Text.Encoding
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import qualified Data.Text.ICU.Convert as ICU

tshow :: Show a => a -> Text
tshow = T.pack . show

cmpStDef :: CompileState
cmpStDef = CompileState
  { cmpStByteCount = 0
  }

tCmpStDef :: TextCompileState
tCmpStDef = TextCompileState
  { tCmpStByteCount = 0
  , tCmpStLine      = 0
  , tCmpStCol       = 0
  }

attrs :: TargetAttr
attrs = TargetAttr
  { tAttrLines = 3
  , tAttrCols  = 44
  }

runCompiler :: Compiler a -> IO (Either Text a)
runCompiler = runExceptT . flip evalStateT cmpStDef

runTextCompiler :: TextCompiler a -> IO (Either Text a)
runTextCompiler = runExceptT . flip evalStateT tCmpStDef

-- TODO: shouldn't be using BL.toStrict, should use lazy the whole way instead
-- this is *probably* a temporary function so that's OK, but... lol
pprintASCF :: BL.ByteString -> Text
pprintASCF bs = T.intercalate " " $ BL.foldr f [] bs
  where
    f byte = (:) $ T.toUpper . decodeUtf8 . BL.toStrict . toLazyByteString . word8HexFixed $ byte

compileAndPrint :: Compiler Builder -> IO ()
compileAndPrint cmp = do
    out <- runCompiler cmp
    case out of
        Left err -> T.putStrLn err
        Right t -> T.putStrLn . pprintASCF . toLazyByteString $ t

compileTextAndPrint :: TextCompiler Builder -> IO ()
compileTextAndPrint cmp = do
    out <- runTextCompiler cmp
    case out of
        Left err -> T.putStrLn err
        Right t -> T.putStrLn . pprintASCF . toLazyByteString $ t

-- | Do a monadic fold over a data type, compile elements with the given
--   function and building up a ByteString.
foldCompile :: (Foldable t, Monad m) => (a -> m Builder) -> t a -> m Builder
foldCompile compileElement = flip foldM mempty $ \block el -> do
    -- compile element
    elCompiled <- compileElement el

    -- combine with rest of block
    return $ block <> elCompiled

compile :: Script -> Compiler Builder
compile = foldCompile compileScriptElement

compileScriptElement :: ScriptElement -> Compiler Builder
compileScriptElement (ScriptElementTextBlock tblk) = do
    -- compile content
    (content, st) <- lift . flip runStateT tCmpStDef $ compileTextBlock tblk
    let len = tCmpStByteCount st

    -- decorate with header
    threeByteLengthHeader 0x05 content (fromIntegral len)
compileScriptElement (ScriptElementCommand cmd) = do
    -- compile content
    -- lol: I unwrap the full stack, then wrap the plain Except in ExceptT e IO
    (content, len) <- lift . except . runExcept . flip runStateT 0 $ compileCommand cmd

    -- decorate with header
    threeByteLengthHeader 0x07 content (fromIntegral len)
compileScriptElement (ScriptElement03String bs) = do
    addBytes 5
    return $ byteString bs

threeByteLengthHeader :: Word8 -> Builder -> Word8 -> Compiler Builder
threeByteLengthHeader firstByte content len = do
    let header = encodeBytes [firstByte, len, 0x00]
    addBytes $ toInteger len + 3
    return $ header <> content

encodeBytes :: [Word8] -> Builder
encodeBytes = byteString . BS.pack

compileCommand :: Command -> CommandCompiler Builder
compileCommand _ = return mempty

compileTextBlock :: TextBlock -> TextCompiler Builder
compileTextBlock = foldCompile compileTextBlockElement

compileTextBlockElement :: TextBlockElement -> TextCompiler Builder
compileTextBlockElement (TBText    t) = do
    -- TODO: XXX: FUCK: WILL throw exception on non-Shift JIS-able input!!! WILL
    -- THROW SHIT ABOUT!!!!!!!!!
    -- text-icu USES TONS OF unsafePerformIO too, ICU.fromUnicode is very impure
    converter <- lift . lift $ ICU.open "Shift JIS" Nothing
    let sjisBytes = ICU.fromUnicode converter t
    compileTextBlockElement $ TBBytes sjisBytes
compileTextBlockElement (TBBytes  bs) = do
    addBytesT $ fromIntegral $ BS.length bs
    return $ byteString bs
compileTextBlockElement (TBCmd   cmd) = compileTextBlockCommand cmd

compileTextBlockCommand :: TextBlockCommand -> TextCompiler Builder
compileTextBlockCommand (TBCPause ms) = compileTBCPause ms
compileTextBlockCommand (TBCVoice id) = compileTBCVoice id
compileTextBlockCommand (TBCUndecoded cmd args) = do
    cmdBytes <- compileTextBlockElement $ TBBytes cmd
    argsBytes <- compileTextBlockElement $ TBBytes args
    return $ cmdBytes <> argsBytes
compileTextBlockCommand TBCNewline    =
    -- 0x815e = Shift JIS for fullwidth forward slash
    compileTextBlockElement $ TBBytes $ BS.pack [0x81, 0x5e]

-- originally wrote my own, then realised the Data.ByteString.Builder provides
-- one which is probably faster so. Safety not guaranteed through my own code,
-- though.
compileTBCPause :: Integer -> TextCompiler Builder
compileTBCPause ms = do
    let (value, digit1) = divMod ms 10
    throwIf (value > 99)  $ "cmd: pause: too large for ASCF: "    <> tshow ms
    throwIf (digit1 /= 0) $ "cmd: pause: unsupported precision: " <> tshow ms
    addBytesT 3
    return $ char7 'W' <> integerDec value

compileTBCVoice :: Integer -> TextCompiler Builder
compileTBCVoice id = do
    throwIf (id > 9999) $ "cmd: pause: voice id too large for ASCF: " <> tshow id
    addBytesT 5
    return $ char7 'V' <> integerDec id

-- | Increment byte count by the given amount.
addBytes :: Integer -> Compiler ()
addBytes n = modify $ \s -> s { cmpStByteCount = cmpStByteCount s + n }

-- | Increment byte count by the given amount.
addBytesT :: Integer -> TextCompiler ()
addBytesT n = modify $ \s -> s { tCmpStByteCount = tCmpStByteCount s + n }

-- TODO: similar to @guardExceptT@ in from-sum (but with a lift)
throwIf :: Bool -> Text -> TextCompiler ()
throwIf True  err = lift . throwE $ err
throwIf False _   = return ()
