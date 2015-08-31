{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Data.TZworld.Internal.Types  where

import Data.Aeson
import qualified Data.Binary as DB
import Control.Monad
import GHC.Generics
import qualified Data.ByteString.Lazy as BL
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Control.Applicative

type Longitude = Double
type Latitude = Double

data TZWorldField = TZWorldField Int BL.ByteString deriving (Show)

instance FromRow TZWorldField where
  fromRow = TZWorldField <$> field <*> field

instance ToRow TZWorldField where
  toRow (TZWorldField id_ blob) = toRow (id_, blob)
  


{- Defines a TZ World structure -}
data TZWorld = TZWorld {
    tztype::String
  , crs::CRS
  , features::[TZFeature]
  }deriving (Show)
            
instance FromJSON TZWorld where
  parseJSON (Object v) = TZWorld <$>
                         v .: "type" <*>
                         v .: "crs"  <*>
                         v .: "features"
  parseJSON _          = mzero
  

data CRS = CRS {
    crsType::String
  , crsProperties::CRSProperty
  }deriving (Show)
instance FromJSON CRS where
  parseJSON (Object v) = CRS <$>
                         v .: "type" <*>
                         v .: "properties"
  parseJSON _          = mzero
  
data CRSProperty = CRSProperty {
    name::String
  } deriving (Show)
instance FromJSON CRSProperty where
  parseJSON (Object v) = CRSProperty <$>
                         v .: "name"
  parseJSON _          = mzero

{- Represents a polygon associated with a time zone.  A time zone region can have many polygons -}  
--data TZPoly = TZPoly {
--    tzname::String
--  , tzcoords::[(Double,Double)]
--  } deriving (Show,Read,Eq, Ord,Generic)
--instance DB.Binary TZPoly

{- A polygon that has a collection of indexed polygon coordinates  -}
data TZPoly = TZPoly {
  tzbinname::String
  , tzbincoords::[((Double,Double),(Double,Double))]
  , tzbinminlong::Double
  , tzbinmaxlong::Double
  } deriving (Show, Read,Generic,Eq,Ord)
instance DB.Binary TZPoly

{- Collection of indexed polygons -}
data TZPolys = TZPolys { tzbins::[TZPoly]} deriving (Show,Read, Generic)
instance DB.Binary TZPolys

data TZFeature = TZFeature {
    fType::String
  , fproperties::TZID
  , geometry::Geometry
  }deriving (Show)

instance FromJSON TZFeature where
  parseJSON (Object v) = TZFeature <$>
                         v .: "type" <*>
                         v .: "properties" <*>
                         v .: "geometry"
  parseJSON _          = mzero

data TZID = TZID {
    tzid::String
  } deriving (Show)

instance FromJSON TZID where
  parseJSON (Object v) = TZID <$>
                         v .: "TZID"
  parseJSON _          = mzero
  

data Geometry = Geometry {
    gType::String
  , coordinates::[[[Double]]]
  } deriving (Show)

instance FromJSON Geometry where
  parseJSON (Object v) = Geometry <$>
                         v .: "type" <*>
                         v .: "coordinates"
  parseJSON _          = mzero

