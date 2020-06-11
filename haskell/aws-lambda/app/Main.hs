{-# LANGUAGE TemplateHaskell #-}
module Main where

import Aws.Lambda

import qualified Lib

generateLambdaDispatcher
