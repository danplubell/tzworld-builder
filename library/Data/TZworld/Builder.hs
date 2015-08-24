{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.TZworld.Builder (module Data.TZworld.Builder) where
import Data.TZworld.Internal.Types
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as DB
import qualified Data.IntMap.Lazy as DM
import qualified Data.Aeson as DA
import qualified Data.Ord as DO
import Data.Maybe
import qualified Data.Set as DS
import qualified Data.List as DL
import Control.Monad
import qualified Data.Text as T
import Paths_tzworld_builder

{-Parse the TZWorld geoJason file into an SQLite database-}
main :: IO ()
main = do
  db <-open "tzworld.db"
  execute_ db "CREATE TABLE IF NOT EXISTS tzworld (id INTEGER PRIMARY KEY, bucketbytes BLOB)"
  tzbs <- buildTZPolys --parse json from file
  let buckets = buildBuckets (tzbins tzbs) DM.empty -- load buckets
  let bucketbllist = map DB.encode (DM.elems buckets) -- create list of bytestrings for each bucket

  let bllisttup = zip [(0::Int)..] bucketbllist --index the buckets
   
  let actionlist = map (insertBucket db) bllisttup -- build list of actions to insert

  sequence_ actionlist --execute list
    
  close db --close the database

{-Insert each bucket into database -}
insertBucket:: Connection -> (Int,BL.ByteString) -> IO()
insertBucket db bl = execute db "INSERT OR REPLACE INTO tzworld (id,bucketbytes) VALUES (?,?)"
                       (uncurry TZWorldField bl )

{-Driver for loading json, decoding json, and making polygons -}
buildTZPolys ::IO TZPolys
buildTZPolys = do
  tzworld <- loadTZWorld
  let tzbs = makeTZPolys tzworld
  return tzbs

{-Make polygon structures from TZWorld -}
makeTZPolys::TZWorld->TZPolys
makeTZPolys w = TZPolys (map makeTZPoly (features w))
  where
    makeTZPoly::TZFeature -> TZPoly
    makeTZPoly f = TZPoly (tzid $ fproperties f)
                    (pnpolyidx (mkVertices f) (mkVertices f))
                    (snd(DL.minimumBy (DO.comparing snd) (mkVertices f)))
                    (snd(DL.maximumBy (DO.comparing snd) (mkVertices f)))
    mkVertices f'  = maketuples . concat . coordinates $ geometry f'

{-Load and decode GeoJson into TZWorld data structure -}
loadTZWorld:: IO TZWorld
loadTZWorld = do
  bs <- BL.readFile "tz_world.json"
  let tzworld = fromMaybe (error "The json structure is not a tzworld json file") (DA.decode bs::Maybe TZWorld)
      
  return tzworld

{-Convert pairs of doubles in list to tuples  -}
maketuples::[[Double]] -> [(Longitude,Latitude)]
maketuples  = map maketuple 
  where
    {-Make an individual tuple.  Flip Longitude and Latitude-}
    maketuple::[Double] -> (Latitude,Longitude)
    maketuple (lon:lat:_) = (lat,lon)
    maketuple [] = error "empty list"
    maketuple [_] = error "not enough elements"

{-Build indexed list of tuples for the PNPOLY algorithm-}
pnpolyidx:: [a]->[a]-> [(a,a)]
pnpolyidx i j  = (head i,last j):zip (tail i) j 

{-Time Zone polygons are inserted into buckets according to the minimum and maximum longitude of the polygon.
A Time Zone polygon that straddles buckets will be included in each bucket that it straddles. -}
buildBuckets::[TZPoly] -> DM.IntMap (DS.Set TZPoly) -> DM.IntMap  (DS.Set TZPoly)
buildBuckets l m = DL.foldl' insertInBuckets m l
  where
        insertInBuckets:: DM.IntMap (DS.Set TZPoly)-> TZPoly -> DM.IntMap (DS.Set TZPoly)
        insertInBuckets polyset poly = foldr (insertInBucket poly) polyset [tzbinminlong poly,tzbinmaxlong poly]

        insertInBucket:: TZPoly -> Longitude -> DM.IntMap (DS.Set TZPoly) -> DM.IntMap (DS.Set TZPoly)
        insertInBucket p a'  = DM.insertWith DS.union (calcBucket a')  (DS.singleton p) 

{- Calculate bucket for a longitude. Buckets are 15 degrees of longitude -}
calcBucket ::Longitude -> Int
calcBucket c = floor ( c/15.00)::Int

