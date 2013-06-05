
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module MarketData where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Int

--import Data.Serialize (Serialize)
--import qualified Data.Serialize as S

import Control.Applicative

import Test.QuickCheck

--import Control.DeepSeq

data MarketData = MarketData {
  ticker :: Text,
  price  :: Double,
  volume :: Int32,
  time   :: Int64 
} deriving (Eq, Show)

mkTicker :: Int -> Text
mkTicker i = T.justifyLeft 5 'A' i' 
  where i' = T.pack ("S" ++ show i)


--instance Serialize MarketData where
--  put m = do
--    S.putByteString . T.encodeUtf16BE . ticker $ m
--    S.putFloat64be . price $ m
--    S.put . volume $ m
--    S.put . time $ m

--  get = MarketData <$> (T.decodeUtf16BE <$> S.getByteString 8)
--                   <*> S.getFloat64be
--                   <*> S.get
--                   <*> S.get

instance Arbitrary MarketData where
  arbitrary = MarketData <$> (mkTicker <$> choose (0,499)) 
                         <*> (getPositive <$> arbitrary)
                         <*> (getPositive <$> arbitrary) 
                         <*> (getPositive <$> arbitrary) 


--instance NFData MarketData where
  
