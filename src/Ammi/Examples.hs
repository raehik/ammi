{-# LANGUAGE OverloadedStrings #-}

module Ammi.Examples where

import Ammi.Types

import Data.Text (Text)
import qualified Data.ByteString as BS

exATBNothing :: TextBlock
exATBNothing = []

exATBSomething :: TextBlock
exATBSomething =
  [ TBEText "4567891234"
  , TBECmd $ TBCPause 450
  ]
