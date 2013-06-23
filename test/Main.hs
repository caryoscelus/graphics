module Main where

import Test.Framework (defaultMain)

import qualified Control.Monad.Trans.Writer.Stricter as Writer

main :: IO ()
main = defaultMain [Writer.test]
