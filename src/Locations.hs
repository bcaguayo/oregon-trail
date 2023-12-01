module Locations where

{-
Hits:
Independence, Missouri  0 miles
Missouri River          10 miles
Fort Kearney            325 miles
Fort Laramie            600 miles
Fort Bridger            975 miles
Fort Boise              1500 miles
The Dalles              1780 miles
Oregon City, Oregon     2000 miles
-}

data LocationType = Town | River | Fort | Road deriving (Eq, Ord, Show)

-- can only hunt on the Road
-- can travel always
-- can only rest in a Town and Fort

getOptions :: LocationType -> [String]
getOptions = undefined

locations :: [String]
locations = [ "Independence, Missouri"
            , "Missouri River"
            , "Fort Kearney"
            , "Fort Laramie"
            , "Fort Bridger"
            , "Fort Boise"
            , "The Dalles"
            , "Oregon City, Oregon"
            ]

