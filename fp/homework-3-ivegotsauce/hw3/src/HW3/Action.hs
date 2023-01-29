{-# LANGUAGE DerivingVia #-}

module HW3.Action
  ( HiPermission(..)
  , PermissionException(..)
  , HIO(..)
  ) where

import Control.Exception
import Control.Monad.Reader
import Data.Set

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data PermissionException =
  PermissionRequired HiPermission
  deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
  deriving (Functor, Applicative, Monad) via (ReaderT (Set HiPermission) IO)
