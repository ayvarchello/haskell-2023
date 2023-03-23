{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Example where

import Polysemy (makeSem, Member, Sem, Embed, interpret, embed)

data Teletype m a where
  ReadTTY :: Teletype m String
  WriteTTY :: String -> Teletype m ()

makeSem ''Teletype

readString :: Member Teletype r => Sem r String
readString = readTTY

teletypeToIO :: Member (Embed IO) r => Sem (Teletype ': r) a -> Sem r a
teletypeToIO = interpret $ \case
  ReadTTY -> embed getLine
  WriteTTY s -> embed (putStrLn s)

-- Sem '[Embed m] a -> m a





