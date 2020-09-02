{-# LANGUAGE OverloadedStrings #-}

module Ammi.Examples where

import Ammi.Types
import Data.ByteString as BS
import Data.Word

bs :: [Word8] -> BS.ByteString
bs = BS.pack

tFWTilde :: TextBlockElement
tFWTilde = TBBytes $ bs [0x81, 0x60]

-- Compiles down to the original ASCF text box!
exATBAmagamiFirstTextbox :: TextBlock
exATBAmagamiFirstTextbox =
  [ TBText "多分気のせいだとは思うんだけど、"
  , TBCmd $ TBCPause 150
  , TBCmd $ TBCNewline
  , TBText "その日の夕焼け空は特別まぶしい気がした……。"
  ]

-- Compiles down to a section of the original ASCF! But text conversion is
-- broken, and I haven't written code for many text commands yet.
exAmagamiIntroFromMiya :: Script
exAmagamiIntroFromMiya =
  [ ScriptElementTextBlock
      [ TBText "「"
      , TBCmd $ TBCVoice 1858
      , TBCmd $ TBCUndecoded (bs [0x54, 0x73]) (bs [0x31])
      , TBText "にぃに"
      , tFWTilde
      , TBText "。朝だよ"
      , tFWTilde
      , TBText "」"
      ]
  , ScriptElementTextBlock
      [ TBText "（……"
      , TBCmd $ TBCUndecoded (bs [0x52]) BS.empty
      , TBText "美也"
      , TBCmd $ TBCUndecoded (bs [0x52]) (bs [0x32])
      , TBText "みや？）"
      ]
  , ScriptElement03String $ bs [0x03, 0x00, 0x00, 0x70, 0x42]
  ]
