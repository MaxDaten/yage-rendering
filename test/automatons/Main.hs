{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where
import Test.Hspec (hspec)
import TransformationTest

main :: IO ()
main = do
    hspec $ do
        transformationProps
