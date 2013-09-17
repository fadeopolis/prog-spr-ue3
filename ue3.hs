{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns    #-}

import Db
import Parser
import Trains

import Data.List
import System.IO hiding (readFile)
import System.Directory
import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Maybe

import Debug.Trace

data TODO = TODO

todo      = error "TODO"
todo' msg = error ("TODO: " ++ msg)

{- a action parsed from user input,
 - takes a Db, produces a new Db and some output to print 
 - and a flag telling us if we are done
-}
type Command = DbFn ()

commands = Commands {
	show_all_trains  = todo,
	show_all_routes  = todo,

	show_train       = todo,
	show_train_car   = todo,
	show_route       = todo,
	show_city        = todo,
	show_reservation = todo,

	unknown_command  = db_error
}

-- | Get train for id
train_for_id :: TrainId -> Db -> Maybe Train
train_for_id id =   trains
                >>> filter ((== id) . train_id)
                >>> listToMaybe


-- | Get train car for id
train_car_for_id :: TrainCarId -> Db -> Maybe TrainCar
train_car_for_id id =   trains
                    >>> concatMap train_cars
                    >>> filter ((== id) . train_car_id)
                    >>> listToMaybe

-- | Get train car from train by index.
-- | Start counting at 1.
-- | So a train has cars [1 .. num-cars]
get_nth_car_of_train :: Train -> Int -> Maybe TrainCar
get_nth_car_of_train train n = do
	guard (n >= 1)
	guard (length (train_cars train) >= n)

	return (train_cars train !! (n - 1))

-- | Compute all trains that pass a given station
transfers :: City -> Db -> [Train]
transfers city =   trains                                      -- get all trains
               >>> filter (elem city . cities . train_route)   -- filter out trains for city


-- Mindestanzahl der freien und maximale Anzahl der durch Reservierung belegten Plätze pro Zug
-- und Waggon zwischen je zwei Stationen (wobei sich Minimum und Maximum darauf beziehen, 
-- dass Reservierungen möglicherweise nur auf Teilen der abgefragten Strecke existieren); 

-- Input:  Eine List von Stationen
-- Output: Für jeden Wagon jeden Zuges, Anzahl der freien und reservierten Plätze zwischen den Stationen.

query2 :: [City] -> Db -> [(Train, [(TrainCar, Int, Int)])]
query2 route db = do
	train <- trains db

	let cars = do 
		traincar <- train_cars train
	
		-- filter out reservations for this train, car and route
		let rs = do
			r <- reservations db

			guard (reservation_train    r == train_id train)         -- filter for this train
			guard (reservation_traincar r == train_car_id traincar)  -- filter for this traincar
			guard (routes_overlap route (reservation_route r))       -- filter for this route

			return r

		let free     = num_free_seats_for_car     traincar (map reservation_slot rs)
		let reserved = num_reserved_seats_for_car traincar (map reservation_slot rs)

		return (traincar, free, reserved)

	return (train, cars)


-- Reservierungen für einen bestimmten Platz in einem Zug, wobei das Ergebnis die Stationen angibt,
-- zwischen denen Reservierungen bestehen; 

-- Input:  Ein Zug, ein Wagon, eine Sitznummer
-- Output: Alle Routen für die diser Sitz reserviert wurde.

query3 :: Train -> TrainCar -> Int -> Db -> [[City]]
query3 train traincar seat =   reservations                                               -- get reservations from dbb
                           >>> filter ((train_id     train    ==) . reservation_train)     -- filter for this train
                           >>> filter ((train_car_id traincar ==) . reservation_traincar)  -- filter for this train
                           >>> filter ((slot_contains_seat seat)  . reservation_slot)      -- filter for `seat'
	                       >>> map reservation_route                                       -- get cities of reservation


-- Mindestanzahl der zwischen zwei Stationen freien und der noch reservierbaren Plätze sowie die maximale Gruppengröße
-- (= maximale Zahl der Personen pro Reservierung) für einen Zug oder mehrere gegebene Züge (wenn Umsteigen nötig ist). 

-- Input:  Eine Liste von (Zug, Startstation, Endstation) Tupeln
-- Output: Für jeden Tupel, Anzahl der freien und reservierbaren Plätze, sowie Länge des größten freien Bereichs in einem Wagon, 
--         zwischen den Stationen.

query4 :: [(Train, City, City)] -> Db -> [(Train, Int, Int, Int)]
query4 trains db = do
	(train, start, stop) <- trains

	-- get route for start and end station
	route <- maybeToList (route_from_start_stop start stop db)

	let cars = do
		traincar <- train_cars train
	
		-- filter out reservations for this train, car and route
		let rs = do
			r <- reservations db

			guard (reservation_train    r == train_id train)         -- filter for this train
			guard (reservation_traincar r == train_car_id traincar)  -- filter for this traincar
			guard (routes_overlap route (reservation_route r))       -- filter for this route

			return r

		let free           = num_free_seats_for_car     traincar (map reservation_slot rs)
		let max_group_size = max_group_size_for_car     traincar (map reservation_slot rs)

		return (free, max_group_size)

--	traceShow cars [1]

	let free           = sum (map fst cars)                    -- add up free seats from all cars
	let reservable     = free - (train_res_free_seats train)   -- cannot reserve seats in free quota
	let max_group_size = maximum (map snd cars)                -- get biggest free slot

	return (train, free, reservable, max_group_size)


-- | Checks if two routes overlap
routes_overlap :: [City] -> [City] -> Bool
routes_overlap a b = intersect a b /= [] 

-- | Computes the number of free seats for a traincar
num_free_seats_for_car :: TrainCar -> [Slot] -> Int
num_free_seats_for_car traincar =   train_car_status traincar           -- compute free/reserved bitmap
	                            >>> filter (==Free)                     -- filter for `free' bits
                                >>> length                              -- count `free' bits

-- | Computes the number of reserved seats for a traincar
num_reserved_seats_for_car :: TrainCar -> [Slot] -> Int
num_reserved_seats_for_car traincar =   train_car_status traincar       -- compute free/reserved bitmap
                                    >>> filter (==Reserved)             -- folter for `reserved' bits
                                    >>> length                          -- count `reserved' bits

-- | Computes the size of the biggest free slot in a train car
max_group_size_for_car :: TrainCar -> [Slot] -> Int
max_group_size_for_car traincar =   train_car_status traincar     -- create free/reserved bitmap
                                >>> run_length_encode             -- run length encode
                                >>> filter (\(a,_) -> a==Free)    -- discard reserved slots
                                >>> map snd                       -- discard Free tags
                                >>> maximum                       -- get maximum
	where
		run_length_encode :: Eq a => [a] -> [(a, Int)]
		run_length_encode []     = []
		run_length_encode (l:ls) = encode l 0 (l:ls)
			where
				encode a n []     = [(a,n)]
				encode a n (l:ls) | a == l    = encode a (n+1) ls
		    		              | otherwise = (a,n) : encode l 0 (l:ls)

-- | A bitmap that shows which seats of a TrainCar are reserved or free
type TrainCarStatus = [SeatStatus]
data SeatStatus     = Free | Reserved deriving (Show, Eq)

-- | Computes a bitmap telling us which seats are free or reserved from a list of reservations
-- | Works by computing a bitmap for each slot and just `or'ing together the bitmaps
train_car_status :: TrainCar -> [Slot] -> TrainCarStatus
train_car_status traincar reservations = foldl (zipWith or) initial_status (map slot_to_status reservations)
	where
		-- initially all seats are free
		initial_status   = times Free (train_car_num_seats traincar)
		-- create bitmaps from reservation
		slot_to_status s = times Free (slot_first_seat s - 1) ++ times Reserved (slot_num_seats s) ++ repeat Free

		or Free Free = Free
		or _    _    = Reserved


-- | Create a list that contains element `a' `n' times.
times :: a -> Int -> [a]
times a n = take n (repeat a)

-- | Try to find a Route in the database for the given start and stop stations.
--route_from_start_stop
route_from_start_stop :: City -> City -> Db -> Maybe [City]
route_from_start_stop start stop =   routes                                 -- get all routes from db
                                  >>> map cities                             -- get cities for route
                                  >>> map (try_find_subroute start stop)     -- try to find a subroute from route
                                  >>> mconcat                                -- return first success

-- | Given a route try to get the subroute starting at station `start' and ending at station `stop'
try_find_subroute :: City -> City -> [City] -> Maybe [City]
try_find_subroute start stop cities = do
	a <- findIndex (start==) cities
	b <- findIndex (stop ==) cities

	guard (a < b)

	return (drop a (take b cities))


-- | Turn a function into a database action
runQuery :: (Db -> a) -> DbFn a
runQuery fn = do
	db <- db_get id
	return (fn db)


main :: IO ()
main = do
	let db_path = "zug.db"

	putStrLn ">> STARTING APP"

	db_exists <- doesFileExist db_path

	when (not db_exists) $ do
		putStrLn ">> CREATING DATABASE"
		writeDb db_path db

	putStrLn ">> READING DATABASE"
	db' <- readDb db_path
	putStrLn ">> READ DATABASE"

	db'' <- mainLoop db'

	putStrLn ">> WRITING DATABASE"
	writeDb db_path db'' -- process changes in DB
	putStrLn ">> WROTE DATABASE"

	putStrLn ">> DONE"

mainLoop :: Db -> IO Db
mainLoop db = do
	eof <- isEOF

	if eof then
		return db
	else do
		input            <- getLine
		cmd              <- return (parse_command commands input)
		(_, output, db') <- return (runDbFn db cmd)

		putStrLn output
		mainLoop db'

readDb :: FilePath -> IO Db
readDb path = do
	text  <- readFile path
	text' <- return (force text) -- make sure file is fully read before we return

	return (read text')

writeDb :: FilePath -> Db -> IO ()
writeDb file db = writeFile file (show db)

-- | Force evaluation of a lazy value
force :: a -> a
force !a = a
