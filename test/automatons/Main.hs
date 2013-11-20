{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where
import Test.Hspec (hspec, describe)
import StorableVertexTest
import VertexAttribTest

main :: IO ()
main = do
    hspec $ do
        describe "storable vertex" $ do
            storableVertexSpecs

        describe "vertex attribute specs" $ do
            vertexAttribSpec
