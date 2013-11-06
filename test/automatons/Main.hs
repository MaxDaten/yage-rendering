{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where
import Test.Hspec (hspec, describe)
import StorableVertexTest

main :: IO ()
main = do
    hspec $ do
        describe "storable vertex" $ do
            vertexSpecs
