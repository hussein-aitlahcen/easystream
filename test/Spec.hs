-- Spec.hs ---

-- Copyright (C) 2018 Hussein Ait-Lahcen

-- Author: Hussein Ait-Lahcen <hussein.aitlahcen@gmail.com>

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import           EasyStream
import           Test.Hspec

main :: IO ()
main = hspec $ do

  describe "Serial streaming" $ do
    it "produce desired output" $
      testSerial >>= shouldBe [(1, 1), (1, 2), (1, 3), (2, 1), (2, 2), (2, 3), (3, 1), (3, 2), (3, 3)]

  describe "Coserial streaming" $ do
    it "produce desired output" $
      testCoserial >>= shouldBe [(1, 1), (2, 1), (3, 1), (1, 2), (2, 2), (3, 2), (1, 3), (2, 3), (3, 3)]

testSerial :: IO [(Int, Int)]
testSerial = toList stream
  where
    stream = source :: SerialT IO (Int, Int)

testCoserial :: IO [(Int, Int)]
testCoserial = toList stream
  where
    stream = source :: CoserialT IO (Int, Int)

source :: (IsStream t, Monad m) => t m (Int, Int)
source = do
  x <- flow
  y <- flow
  pure (x, y)

flow :: (IsStream t, Monad m) => t m Int
flow = fromStream $ 1 >: 2 >: 3 >: snil
