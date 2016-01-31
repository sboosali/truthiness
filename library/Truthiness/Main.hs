{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Truthiness.Main where
import Truthiness

main = do
 unlessB [] $ do 
     print "Truthiness"

