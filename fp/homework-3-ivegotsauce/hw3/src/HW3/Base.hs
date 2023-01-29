{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf         #-}

module HW3.Base
  ( HiFun(..)
  , HiValue(..)
  , HiExpr(..)
  , HiError(..)
  , HiAction(..)
  , HiMonad(..)
  ) where

import Codec.Serialise (Serialise)
import Control.Exception (throwIO)
import qualified Data.ByteString as B
import Data.Map hiding (fromList, member)
import Data.Sequence
import Data.Set hiding (fromList)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Time
import GHC.Generics (Generic)
import HW3.Action
import System.Directory
import System.Random

data HiFun =
    HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Serialise)

data HiValue =
    HiValueNull
  | HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueString T.Text
  | HiValueList (Seq HiValue)
  | HiValueBytes B.ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise)

data HiExpr =
    HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving stock (Show, Eq, Ord)

data HiError =
    HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise)

data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath B.ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho T.Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise)

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

instance HiMonad HIO where
  runAction action = HIO $ \ps -> case action of
    HiActionRead p -> do
      if | member AllowRead ps -> do
           fileExist <- doesFileExist p
           directoryExist <- doesDirectoryExist p
           if | fileExist -> do
                bytes <- B.readFile p
                case decodeUtf8' bytes of
                  Left _  -> return $ HiValueBytes bytes
                  Right s -> return $ HiValueString s
              | directoryExist -> do
                d <- listDirectory p
                return $ HiValueList $ fromList (HiValueString . T.pack <$> d)
              | otherwise -> return HiValueNull
         | otherwise -> throwIO $ PermissionRequired AllowRead
    HiActionWrite f b -> do
      if | member AllowWrite ps -> do
           B.writeFile f b
           return HiValueNull
         | otherwise -> throwIO  $ PermissionRequired AllowWrite
    HiActionMkDir p -> do
      if | member AllowWrite ps -> do
           createDirectory p
           return HiValueNull
         | otherwise -> throwIO $ PermissionRequired AllowWrite
    HiActionChDir p -> do
      if | member AllowRead ps -> do
           setCurrentDirectory p
           return HiValueNull
         | otherwise -> throwIO $ PermissionRequired AllowRead
    HiActionCwd -> do
      if | member AllowRead ps -> HiValueString . T.pack <$> getCurrentDirectory
         | otherwise           -> throwIO $ PermissionRequired AllowRead
    HiActionNow -> do
      if | member AllowTime ps -> HiValueTime <$> getCurrentTime
         | otherwise           -> throwIO $ PermissionRequired AllowTime
    HiActionRand x y -> do
      HiValueNumber . toRational <$> getStdRandom (uniformR (x, y))
    HiActionEcho t -> do
      if | member AllowWrite ps -> do
           putStrLn $ T.unpack t
           return HiValueNull
         | otherwise -> throwIO $ PermissionRequired AllowWrite
