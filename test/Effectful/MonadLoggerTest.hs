{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Effectful.MonadLoggerTest where

import Effectful
import Effectful.MonadLogger
import Test.Tasty
import Test.Tasty.HUnit

test_execLoggerWriter :: TestTree
test_execLoggerWriter =
  testCase "execLoggerWriter emits in order" $
    let m :: (Logger :> es) => Eff es ()
        m = do
          $(logInfo) "foo"
          $(logInfo) "bar"
          pure ()
     in ["foo", "bar"]
          @=? map (\(_, _, _, msg) -> msg) (runPureEff (execLoggerWriter m))
