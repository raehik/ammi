module Lib where

import qualified Data.ByteString as BS
import Data.Text
import Control.Monad.Trans.Reader

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Error
  = ErrSStringExceedsByteLimit Int Int

-- Data type defining values used in script type checking.
data Env = Env
  { envMaxLines :: Int
  , envLineFullWidthMax :: Int
  }

envAmagami :: Env
envAmagami = Env
  { envMaxLines = 3
  , envLineFullWidthMax = 22
  }

type SString = BS.ByteString
data ASString = ASString
  { asString :: SString
  , asStringLineFullWidthCounts :: [Int]
  }

-- Convert a 'Text' into a 'SString'.
textToSString :: Text -> SString
textToSString _ = BS.empty

-- Annotate a 'SString'.
--annotateSString :: SString -> ASString
--annotateSString _ = 

data Context = Context
  { ctxErrs :: [Error]
  , ctxBytePtr :: Int
  }

defCtx = Context
  { ctxErrs = []
  , ctxBytePtr = 0
  }

type Checker a = ReaderT Env (State Context) a

tc :: Env -> -> [ScriptTypeError]

-- Type check an annotated 'ASString' according to the given 'ScriptContext'.
typecheckASString :: ScriptContext -> ASString -> [ScriptTypeError]
typecheckASString sc s = sth sc s
  [ lengthIsValid
  ]
  where
    lengthIsValid _ s =
        if   BS.length s > 256
        then Just $ SStringExceedsByteLimit $ BS.length s
        else Nothing

sth
    :: ScriptContext -> ASString
    -> [(ScriptContext -> ASString -> Maybe ScriptTypeError)]
    -> Maybe ScriptTypeError
sth sc s []     = Nothing
sth sc s (f:fs) =
    case f sc s of
        Just err -> Just err
        Nothing -> sth sc s fs
