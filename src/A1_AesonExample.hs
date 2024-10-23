{-# LANGUAGE TemplateHaskell #-}

module A1_AesonExample where

import Data.Aeson (decode, encode)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import qualified Data.ByteString.Lazy.Char8 as BL

data BTree a = Leaf | Node (BTree a) a (BTree a)

$(deriveJSON defaultOptions ''BTree)


testTree :: BTree String
testTree = Node (Node Leaf "l" (Node Leaf "lr" Leaf)) "root" (Node (Node Leaf "rl" Leaf) "r" (Node Leaf "rr" Leaf))

encodedTree :: BL.ByteString
encodedTree = encode testTree

decodedTree :: Maybe (BTree String)
decodedTree = decode encodedTree