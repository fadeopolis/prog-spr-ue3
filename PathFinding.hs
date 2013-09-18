module PathFinding where

import System.IO hiding (readFile)
import System.Directory
import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.List hiding (insert)
import Data.Monoid
import Data.Maybe

import Db

_INFINITY = 999999

neighbors city =   trains                                      -- get all trains
               >>> map (cities . train_route)                  -- get city list from train
               >>> filter (elem city)                          -- filter out routes containing out city
               >>> map (dropWhile (/= city))                   -- skip predecessors of city
               >>> mapMaybe cdar                               -- get successor of city
    where
    	cdar (_:b:_) = Just b
    	cdar _       = Nothing

unvisited_neighbors city unvisited =   neighbors city
                                   >>> filter (\c -> elem c unvisited)


--dijsktra :: City -> Db -> 
dijsktra city db = dijsktra' city 0 distances all_others db
	where
		all_cities = trains >>> concatMap (cities . train_route) $ db
		all_others = delete city all_cities

		-- assign distance infinity too all nodes but start node,
		-- start node gets distance 0
		distances  = (city, 0) : zip all_others (repeat _INFINITY)


--dijsktra' city distance distances []        db = distances
dijsktra' city distance distances unvisited db = 
	if not (null unvisited') then 
		dijsktra' next distance' distances' unvisited' db
	else
		distances'
	where
		neighbors  = unvisited_neighbors city unvisited db
		distances' = foldl (\n -> update_distance n (distance + 1) distances) neighbors
		unvisited' = delete city unvisited

		(next, distance') = min_distance distances' unvisited'

min_distance distances unvisited = minimumBy (\a b -> compare (snd a) (snd b)) candiates
	where
		candiates = filter (\(city, _) -> elem city unvisited) distances

update_distance :: City -> Int -> [(City, Int)] -> [(City, Int)]
update_distance city new_distance []                     = [(city, new_distance)]
update_distance city new_distance (c:cs) | fst c == city = (fst c, min (snd c) new_distance) : cs
                                         | otherwise     = update_distance city new_distance cs
