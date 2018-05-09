-- EasyStream.hs ---

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

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module EasyStream where

import           Control.Monad  (ap)
import           Data.Kind      (Type)
import           Data.Semigroup (Semigroup (..))

snil :: IsStream t => t m a
snil = fromStream $ Stream ($ Stop)

srepeat :: IsStream t => a -> t m a
srepeat x = let s = Stream ($ Continue x s)
            in fromStream s

singleton :: IsStream t => a -> t m a
singleton x = fromStream $ Stream ($ Singleton x)

scons :: IsStream t => a -> t m a -> t m a
scons x s = fromStream $ Stream ($ Continue x (toStream s))

(>:) :: IsStream t => a -> t m a -> t m a
(>:) = scons

infixr 3 >:

data Step :: (Type -> Type) -> Type -> Type where
  Stop :: Step m a
  Singleton :: a -> Step m a
  Continue :: a -> Stream m a -> Step m a

data Stream :: (Type -> Type) -> Type -> Type where
  Stream :: { unStream :: forall r. ((Step m a -> m r) -> m r) } -> Stream m a

class IsStream (t :: (Type -> Type) -> Type -> Type) where
  fromStream :: Stream m a -> t m a
  toStream :: t m a -> Stream m a
  point :: a -> t m a
  mu :: t m (Stream m a) -> t m a

instance IsStream Stream where
  fromStream = id
  toStream = id
  point = singleton
  mu = sjoin

instance {-# OVERLAPPING #-} IsStream t => Semigroup (t m a) where
  (<>) s s' = fromStream $ go (toStream s)
    where
      go (Stream f) = Stream $ \yld ->
        let run (Stream g) = g yld
        in f $ \case
          Stop            -> run (toStream s')
          (Singleton a)   -> yld (Continue a (toStream s'))
          (Continue a as) -> yld (Continue a (go as))

instance {-# OVERLAPPING #-} IsStream t => Monoid (t m a) where
  mempty = snil
  mappend = (<>)

instance {-# OVERLAPPING #-} IsStream t => Functor (t m) where
  fmap f = fromStream . go . toStream
    where
      go (Stream g) = Stream $ \yld -> g $ \case
        Stop            -> yld Stop
        (Singleton a)   -> yld (Singleton (f a))
        (Continue a s') -> yld (Continue (f a) (go s'))

instance (IsStream t, Monad m) =>  Applicative (t m) where
  pure = point
  (<*>) = ap

instance (IsStream t, Monad m) => Monad (t m) where
  s >>= f = mu (fmap (toStream . f) s)

sjoin :: Stream m (Stream m a) -> Stream m a
sjoin (Stream f) = Stream $ \yld ->
  let run (Stream g) = g yld
  in f $ \case
    Stop             -> yld Stop
    (Singleton s)    -> run s
    (Continue s ss) -> run (s <> sjoin ss)

scojoin :: Stream m (Stream m a) -> Stream m a
scojoin (Stream f) = Stream $ \yld ->
  let run (Stream g) = g yld
  in f $ \case
    Stop                     -> yld Stop
    (Singleton s)            -> run s
    (Continue (Stream h) ss) -> run $ Stream $ \yld1 -> h $ \case
      Stop           -> yld1 Stop
      (Singleton a)  -> yld1 (Continue a (scojoin ss))
      (Continue a s) -> yld1 (Continue a (scojoin $ ss <> singleton s))

newtype SerialT m a = SerialT { unSerialT  :: Stream m a }

instance IsStream SerialT where
  fromStream = SerialT
  toStream = unSerialT
  point = SerialT . singleton
  mu = SerialT . sjoin . unSerialT

newtype CoserialT m a = CoserialT { unCoserialT :: Stream m a }

instance IsStream CoserialT where
  fromStream = CoserialT
  toStream = unCoserialT
  point = CoserialT . singleton
  mu = CoserialT . scojoin . unCoserialT

foldrM :: (IsStream t, Monad m) => (a -> b -> m b) -> b -> t m a -> m b
foldrM step acc = go . toStream
  where
    go (Stream f) = f $ \case
      Stop           -> pure acc
      (Singleton a)  -> step a acc
      (Continue a s) -> step a =<< go s

toList :: (IsStream t, Monad m) => t m a -> m ([] a)
toList = foldrM (\x xs -> pure (x : xs)) []

