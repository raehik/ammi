{-# LANGUAGE OverloadedStrings #-}

module Ammi.Examples where

import Ammi.Types

exATBNothing :: TextBlock
exATBNothing = []

exATBSomething :: TextBlock
exATBSomething =
  [ TBText "First line"
  , TBCmd $ TBCNewline
  , TBCmd $ TBCPause 450
  , TBText "Second line"
  ]

-- Compiles down to the original ASCF text box!
exATBAmagamiFirstTextbox :: TextBlock
exATBAmagamiFirstTextbox =
  [ TBText "多分気のせいだとは思うんだけど、"
  , TBCmd $ TBCPause 150
  , TBCmd $ TBCNewline
  , TBText "その日の夕焼け空は特別まぶしい気がした……。"
  ]
