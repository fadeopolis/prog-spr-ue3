{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings  #-}

import Db
import Parser
import Trains
import Result

import Data.List
import System.IO
import System.Directory
import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.Monoid
import Data.Maybe

import Debug.Trace

todo = error "TODO"

{- a action parsed from user input,
 - takes a Db, produces a new Db and some output to print 
 - and a flag telling us if we are done
-}
type Command = (Db -> (Db, String))

commands = Commands {
	show_all_trains  = todo,
	show_all_routes  = todo,

	show_train       = \id -> executeQuery (find_train_by_id id)    (tell . show),
	show_traincar    = \id -> executeQuery (find_traincar_by_id id) (tell . show),
	show_route       = \id -> executeQuery (find_route_by_id id)    (tell . show),
	show_city        = todo,
	show_reservation = \id -> executeQuery (find_reservation_by_id id) (tell . show),

	cmd_query2 = \start stop     -> executeQuery (query2_query start stop)     query2_printer,
	cmd_query3 = \train car seat -> executeQuery (query3_query train car seat) query3_printer,
	cmd_query4 = \trains         -> executeQuery (query4_query trains)         query4_printer,

	unknown_command = \cmd db -> (db, "Unknown command: '" ++ cmd ++ "'")
}

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
transfers city =   db_trains                                         -- get all trains
               >>> filter (elem city . route_cities . train_route)   -- filter out trains for city

executeQuery :: (Db -> Result a) -> (a -> Writer String b) -> Db -> (Db, String)
executeQuery query printer db = case query db of
	(Ok  a)   -> (db, snd (runWriter (printer a)))
	(Err msg) -> (db, "Error: " ++ msg)

train_printer train = tell (show train)

-- Mindestanzahl der freien und maximale Anzahl der durch Reservierung belegten Plätze pro Zug
-- und Waggon zwischen je zwei Stationen (wobei sich Minimum und Maximum darauf beziehen, 
-- dass Reservierungen möglicherweise nur auf Teilen der abgefragten Strecke existieren); 

-- Input:  Eine List von Stationen
-- Output: Für jeden Wagon jeden Zuges, Anzahl der freien und reservierten Plätze zwischen den Stationen.

query2_query :: City -> City -> Db -> Result [(TrainId, [(TrainCarId, Int, Int)])]
query2_query start stop db = do
	route <- route_from_endpoints start stop db

	-- actual query runs in list monad
	return $ do
		train <- db_trains db

		let cars = do 
			traincar <- train_cars train
	
			-- filter out reservations for this train, car and route
			let rs = do
				r <- db_reservations db

				guard (reservation_train    r == train_id train)         -- filter for this train
				guard (reservation_traincar r == traincar_id traincar)  -- filter for this traincar
				guard (routes_overlap route (reservation_route r))       -- filter for this route

				return r

			let free     = num_free_seats_for_car     traincar (map reservation_slot rs)
			let reserved = num_reserved_seats_for_car traincar (map reservation_slot rs)

			return (traincar_id traincar, free, reserved)

		return (train_id train, cars)

query2_printer :: [(TrainId, [(TrainCarId, Int, Int)])] -> Writer String ()
query2_printer trains = do
	tell ("QUERY 2 FOUND " ++ show (length trains) ++ " RECORD(S)")

	forM trains $ \(train, cars) -> do
		tell ("TRAIN: "        ++ tId train ++ "\n")

		forM cars $ \(traincar, free_seats, reserved_seats) -> do
			tell ("\tTRAIN CAR: "        ++ tcId traincar               ++ "\n")
			tell ("\t\tFREE SEATS:     " ++ show free_seats             ++ "\n")
			tell ("\t\tRESERVED SEATS: " ++ show reserved_seats         ++ "\n")

	return ()


-- Reservierungen für einen bestimmten Platz in einem Zug, wobei das Ergebnis die Stationen angibt,
-- zwischen denen Reservierungen bestehen; 

-- Input:  Ein Zug, ein Wagon, eine Sitznummer
-- Output: Alle Routen für die diser Sitz reserviert wurde.

--query3_query :: Train -> TrainCar -> Int -> Db -> Result [[City]]
query3_query train_id traincar_id seat db = do
	train    <- find_train_by_id    train_id    db -- validate train
	traincar <- find_traincar_by_id traincar_id db -- validate traincar

	return $   db_reservations                                          -- get reservations from db
           >>> filter ((train_id    ==) . reservation_train)            -- filter for this train
           >>> filter ((traincar_id ==) . reservation_traincar)         -- filter for this train
           >>> filter ((slot_contains_seat seat)  . reservation_slot)   -- filter for `seat'
           >>> map reservation_route                                    -- get cities of reservation
           $   db

query3_printer routes = do
	tell ("QUERY 3 FOUND " ++ show (length routes) ++ " RECORD(S)")

	forM routes $ \route -> do
		tell (concat (intersperse ", " (map city_name route)))
		tell "\n"

	return ()


-- Mindestanzahl der zwischen zwei Stationen freien und der noch reservierbaren Plätze sowie die maximale Gruppengröße
-- (= maximale Zahl der Personen pro Reservierung) für einen Zug oder mehrere gegebene Züge (wenn Umsteigen nötig ist). 

-- Input:  Eine Liste von (Zug, Startstation, Endstation) Tupeln
-- Output: Für jeden Tupel, Anzahl der freien und reservierbaren Plätze, sowie Länge des größten freien Bereichs in einem Wagon, 
--         zwischen den Stationen.

query4_query :: [(TrainId, City, City)] -> Db -> Result [(TrainId, Int, Int, Int)]
query4_query trains db = do
	forM trains $ \(train_id, start, stop) -> do
		train <- find_train_by_id train_id db
		route <- route_from_endpoints start stop db

		let cars = do
			traincar <- train_cars train
	
			-- filter out reservations for this train, car and route
			let rs = do
				r <- db_reservations db

				guard (reservation_train    r == train_id)               -- filter for this train
				guard (reservation_traincar r == traincar_id traincar)   -- filter for this traincar
				guard (routes_overlap route (reservation_route r))       -- filter for this route

				return r

			let free           = num_free_seats_for_car     traincar (map reservation_slot rs)
			let max_group_size = max_group_size_for_car     traincar (map reservation_slot rs)

			return (free, max_group_size)

		let free           = sum (map fst cars)                    -- add up free seats from all cars
		let reservable     = free - (train_res_free_seats train)   -- cannot reserve seats in free quota
		let max_group_size = maximum (map snd cars)                -- get biggest free slot

		return (train_id, free, reservable, max_group_size)

query4_printer trains = do
	tell ("QUERY 4 FOUND " ++ show (length trains) ++ " RECORD(S)")

	forM trains $ \(train, free, reservable, max_group_size) -> do
		tell ("\tTRAIN " ++ show (tId train) ++ "\n")
		tell ("\t\tFREE SEATS:         " ++ show free           ++ "\n")
		tell ("\t\tRESERVABLE SEATS:   " ++ show reservable     ++ "\n")
		tell ("\t\tMAXIMUM GROUP SIZE: " ++ show max_group_size ++ "\n")

	return ()

num_free_seats_for_train train db = do
	let rs = db_reservations db

	let free = do
		car <- train_cars train

		let rs'   = filter (\r -> reservation_traincar r == traincar_id car) rs
		let slots = map reservation_slot rs'

		return (num_free_seats_for_car car slots)

	return (sum free)


-- | Computes the number of free seats for a traincar
num_free_seats_for_car :: TrainCar -> [Slot] -> Int
num_free_seats_for_car traincar =   traincar_status traincar           -- compute free/reserved bitmap
	                            >>> filter (==Free)                     -- filter for `free' bits
                                >>> length                              -- count `free' bits

-- | Computes the number of reserved seats for a traincar
num_reserved_seats_for_car :: TrainCar -> [Slot] -> Int
num_reserved_seats_for_car traincar =   traincar_status traincar       -- compute free/reserved bitmap
                                    >>> filter (==Reserved)             -- folter for `reserved' bits
                                    >>> length                          -- count `reserved' bits

-- | Computes the size of the biggest free slot in a train car
max_group_size_for_car :: TrainCar -> [Slot] -> Int
max_group_size_for_car traincar =   traincar_status traincar     -- create free/reserved bitmap
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
traincar_status :: TrainCar -> [Slot] -> TrainCarStatus
traincar_status traincar reservations = foldl (zipWith or) initial_status (map slot_to_status reservations)
	where
		-- initially all seats are free
		initial_status   = times Free (traincar_num_seats traincar)
		-- create bitmaps from reservation
		slot_to_status s = times Free (slot_first_seat s - 1) ++ times Reserved (slot_num_seats s) ++ repeat Free

		or Free Free = Free
		or _    _    = Reserved


-- | Create a list that contains element `a' `n' times.
times :: a -> Int -> [a]
times a n = take n (repeat a)

-- | Given a route try to get the subroute starting at station `start' and ending at station `stop'
try_find_subroute :: City -> City -> [City] -> Maybe [City]
try_find_subroute start stop cities = do
	a <- findIndex (start==) cities
	b <- findIndex (stop ==) cities

	guard (a < b)

	return (drop a (take b cities))

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

_PROMPT = "$ "

mainLoop :: Db -> IO Db
mainLoop db = do
	eof <- isEOF

	if eof then
		return db
	else do
		input         <- getLine
		cmd           <- return (parse_command commands (id $! input))
		(db', output) <- return (cmd db)

		putStrLn output
		mainLoop db'

readDb :: FilePath -> IO Db
readDb path = do
	text  <- readFile path

	return $! read $! text -- $! makes sure file is fully read

writeDb :: FilePath -> Db -> IO ()
writeDb file db = writeFile file (show db)
