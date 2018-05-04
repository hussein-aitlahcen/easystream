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

module EasyStream where

import           Control.Monad  (ap)
import           Data.Kind      (Type)
import           Data.Semigroup (Semigroup (..))

snil :: IsStream t => t m a
snil = fromStream $ Stream ($ Stop)

singleton :: IsStream t => a -> t m a
singleton a = fromStream $ Stream ($ Singleton a)

scons :: IsStream t => a -> t m a -> t m a
scons a s = fromStream $ Stream ($ Continue a (toStream s))

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
  mu :: t m (t m a) -> t m a

instance IsStream Stream where
  fromStream = id
  toStream = id
  point = singleton
  mu = sjoin

instance {-# OVERLAPS #-} IsStream t => Semigroup (t m a) where
  (<>) s s' = fromStream $ go (toStream s)
    where
      go (Stream f) = Stream $ \yld ->
        let run s'' = unStream s'' yld
        in f $ \case
          Stop            -> run (toStream s')
          (Singleton a)   -> yld (Continue a (toStream s'))
          (Continue a as) -> yld (Continue a (go as))

instance {-# OVERLAPS #-} IsStream t => Monoid (t m a) where
  mempty = snil
  mappend = (<>)

instance {-# OVERLAPS #-} IsStream t => Functor (t m) where
  fmap f s = fromStream $ go $ toStream s
    where
      go (Stream g) = Stream $ \yld -> g $ \case
        Stop           -> yld Stop
        (Singleton a)  -> yld (Singleton (f a))
        (Continue a s') -> yld (Continue (f a) (go s'))

instance {-# OVERLAPS #-} (IsStream t, Monad m) =>  Applicative (t m) where
  pure = point
  (<*>) = ap

instance {-# OVERLAPS #-} (IsStream t, Monad m) => Monad (t m) where
  s >>= f = mu (fmap f s)

sjoin :: Stream m (Stream m a) -> Stream m a
sjoin ss = Stream $ \yld ->
  let run s = unStream s yld
  in unStream ss $ \case
    Stop             -> yld Stop
    (Singleton s)    -> run s
    (Continue s ss') -> run (s <> sjoin ss')

scojoin :: Stream m (Stream m a) -> Stream m a
scojoin ss = Stream $ \yld ->
  let run s = unStream s yld
  in unStream ss $ \case
      Stop             -> yld Stop
      (Singleton s)    -> run s
      (Continue s ss') -> run $ Stream $ \yld1 -> unStream s $ \case
        Stop            -> yld1 Stop
        (Singleton a)   -> yld1 (Continue a (scojoin ss'))
        (Continue a s') -> yld1 (Continue a (scojoin $ ss' <> singleton s'))

newtype SerialT m a = SerialT { unSerialT  :: Stream m a }

instance IsStream SerialT where
  fromStream = SerialT
  toStream = unSerialT
  point = SerialT . singleton
  mu = SerialT . sjoin . unSerialT . fmap unSerialT

newtype CoserialT m a = CoserialT { unCoserialT :: Stream m a }

instance IsStream CoserialT where
  fromStream = CoserialT
  toStream = unCoserialT
  point = CoserialT . singleton
  mu = CoserialT . scojoin . unCoserialT . fmap unCoserialT

foldrM :: (IsStream t, Monad m) => (a -> b -> m b) -> b -> t m a -> m b
foldrM step acc s = go (toStream s)
  where
    go (Stream f) = f $ \case
      Stop            -> pure acc
      (Singleton a)   -> step a acc
      (Continue a s') -> go s' >>= step a

toList :: (IsStream t, Monad m) => t m a -> m ([] a)
toList = foldrM (\x xs -> pure (x : xs)) []

