{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards    #-}

module Db(
	-- we don't export the Db constructor,
	-- _TEST_DB as starting point
	-- can only be altered by db_add_reservation and db_remove_reservation
	Db,
	reservations, trains,

	-- need to be public for parser
	TrainId(..), TrainCarId(..), RouteId(..), ReservationId(..),

	Train(..), TrainCar(..), Route(..), Reservation,

	--TODO should we make these puplic?
	FreeSeatQuota(..), TrainCarNumSeats(..), City(..),

	-- we don't export the Reservation constructor,
	-- can only be safely created by db_add_reservation
	reservation_id, train, traincar, first_seat, num_seats, reservation_route_id, reservation_route,
	
	--reservation_start, reservation_end,

	runDbFn,
	db_add_reservation, db_remove_reservation,
	db_print, db_println, db_error,

	_TEST_DB,

	db_empty, 
	db_add_trains, 
	db_add_routes,

	reservation_id_gen,
	db_get,
	evalDbFn, evalDbFn',

	DbFn,
) where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Data.List
import Debug.Trace

-- TODO 
data TODO = TODO

todo      = error "TODO"
todo' msg = error ("TODO: " ++ msg)
-- TODO

-- database id types
newtype TrainId          = TrainId          String  deriving (Show, Read, Eq)
newtype FreeSeatQuota    = FreeSeatQuota    Int     deriving (Show, Read, Eq)

newtype TrainCarId       = TrainCarId       String  deriving (Show, Read, Eq)
type    TrainCarNumSeats = Int

newtype RouteId          = RouteId          String  deriving (Show, Read, Eq)
newtype City             = City             String  deriving (Show, Read, Eq)

newtype ReservationId    = ReservationId    Int     deriving (Show, Read, Eq)


-- a train (choo-choo)
data Train = Train {
	train_id             :: TrainId,      -- unique train identification
	train_route          :: Route,      -- route this train drives on
	train_cars           :: [TrainCar], -- list of all traincars of this train
	train_res_free_seats :: FreeSeatQuota -- quota of free seats which are not reservable
}
	deriving (Read, Eq)

--deriving instance Show Train

instance Show Train where
	show Train{..} = "Train " ++ show train_id

-- one car in a train
data TrainCar = TrainCar {
	train_car_id        :: TrainCarId,      -- unique traincar identification
	train_car_num_seats :: TrainCarNumSeats -- number of seats available in a traincar
}
	deriving (Show, Read, Eq)



-- a route from town A to town B, with stops etc.
data Route = Route {
        route_id  :: RouteId,
        cities    :: [City]
}
	deriving (Show, Read, Eq)

data Reservation = Reservation {
	reservation_id       :: ReservationId, -- must be unique

	train                :: TrainId,         -- train passengers ride in
	traincar             :: TrainCarId,      -- train car passengers ride in
	first_seat           :: Int,           -- seat in train car where reservation starts
	num_seats            :: Int,           -- number of seats reserved
	
	reservation_route_id :: RouteId,       -- the route this reservation is on
	reservation_route    :: [City]         -- start and end city as head and tail
	--reservation_start :: City,          -- must be in route
	--reservation_end   :: City           -- must be in route
}
	deriving (Show, Read, Eq)

{- Holds all trains, reservations, routes, etc. 
-- the state of the app so to say.
-- This is what we write to the file
--}
data Db = Db {
	-- fixed part (red from file at startup)
	trains             :: [Train],      -- and traincars
	routes             :: [Route],      -- and cities

	-- changing through time
	reservations       :: [Reservation],
	reservation_id_gen :: Int            -- used to generate unique reservation ids
}
	deriving (Show, Read, Eq)

-- hard coded db for testing
_TEST_DB = Db {
	trains = [], --[Train (TrainId "U1") (RouteId "11"), Train (TrainId "U2") (RouteId "21"), Train (TrainId "U4") (RouteId "41")],
	routes = [], --[Route (RouteId "11") []],

	reservations       = [],
	reservation_id_gen = 0
}

-- computation that may alter the database or print some output
-- DbFn a ~= Db -> (a, Db, String)
newtype DbFn a = DbFn { unDbFn :: (StateT Db (Writer String) a) }

db_trace :: Show a => a -> DbFn ()
db_trace a = trace ("\n>> DEBUG: " ++ show a ++ "\n") (return ())

-- run db function
runDbFn :: Db -> DbFn a -> (a, String, Db)
runDbFn db (DbFn state) = (a, output, db')
	where
		((a, db'), output) = runWriter $ runStateT state db

evalDbFn ::  Db -> DbFn a -> a
evalDbFn db (DbFn state) = a
	where
		((a, db'), output) = runWriter $ runStateT state db

evalDbFn' ::  Db -> DbFn a -> Db
evalDbFn' db (DbFn state) = db'
	where
		((a, db'), output) = runWriter $ runStateT state db


db_list_reservations :: DbFn [Reservation]
db_list_reservations = db_get reservations

-- add reservation to db
-- returns (Just Reservation) if reservation was added
-- returns Nothing if reservation is invalid
db_add_reservation :: TrainId -> TrainCarId -> Int -> Int -> RouteId -> [City] -> DbFn (Maybe Reservation)
db_add_reservation train traincar first_seat num_seats route_id cities = do
	id <- new_reservation_id

	db_add_reservation' (Reservation id train traincar first_seat num_seats route_id cities)

-- remove all reservations with given id
db_remove_reservation :: ReservationId -> DbFn ()
db_remove_reservation reservation_id = do
	db               <- db_get id
	all_reservations <- db_get reservations

	let all_reservations' = filter (has_id reservation_id) all_reservations

	db_put (db { reservations = all_reservations' })

-- print output
db_print :: String -> DbFn ()
db_print = DbFn . lift . tell

-- print line of output 
db_println :: String -> DbFn ()
db_println s = db_print (s ++ "\n")

-- print error message
db_error :: String -> DbFn ()
db_error err = db_println ("Error: " ++ err)

-- reservation collides when same cities and same seats were already taken 
-- computes all Reservations that collide with a given reservation
colliding_reservations :: Reservation -> [Reservation] -> [Reservation]
colliding_reservations r rs = filter (colliding_reservation r) rs

colliding_reservation :: Reservation -> Reservation -> Bool
colliding_reservation r a = (check_city r (reservation_route a)) && (check_seats r a)

-- check if Reservation contains same Cities
check_city :: Reservation -> [City] -> Bool
check_city r r1 = any (uncurry (==)) [(a,b) | a <- reservation_route r, b <- r1]

--elem (head (reservation_route r)) r1

--check_city r null = False
--check_city r r2 = error "checking city"

--printCity :: Reservation -> String
--printCity r = print r

-- if reservation_first_seat > (first_seat + num_seats) -> true, reservation possible
-- else false 
-- Reservierung1: first_seat 4, num_seats 5 -> 4 - 9 reserved.
-- Reservierung2: first_seat 1, num_seats 5 -> 1 - 5 reserved.
-- check if Reservation contains same seats
check_seats :: Reservation -> Reservation -> Bool
check_seats r r1 =
	(((first_seat r) + (num_seats r) - 1) < (first_seat r1) || (first_seat r) > ((first_seat r1) + (num_seats r1) - 1))

-- FOR CREATING DB ------------------------------------------------------

-- | An empty database
db_empty :: Db
db_empty = Db {
	trains = [],
	routes = [],

	reservations       = [],
	reservation_id_gen = 0
}

db_add_trains :: [Train] -> DbFn Db
db_add_trains ts = do
	db <- db_get id
	db_put (db { trains = ts ++ trains db })
	db_get id

db_add_routes :: [Route] -> DbFn Db
db_add_routes rs =  do
	db <- db_get id
	db_put (db { routes = rs ++ routes db })
	db_get id

-- INTERNALS ------------------------------------------------------------

instance Monad DbFn where
	return = DbFn . return
	fail   = DbFn . fail

	dbfn >>= f = DbFn $ do
		a <- unDbFn dbfn
		b <- unDbFn (f a)
		return b

-- add reservation if it has no collisions
db_add_reservation' :: Reservation -> DbFn (Maybe Reservation)
db_add_reservation' r = do
	db               <- db_get id
	all_reservations <- db_get reservations

	db_trace r

	let collisions = colliding_reservations r all_reservations

	if notNull collisions
	then
		return Nothing
	else do
		db_put (db { reservations = r : all_reservations })
		return (Just r)

-- create a new unique reservation id
new_reservation_id :: DbFn ReservationId
new_reservation_id = do
	-- get current value of counter
	new_id <- db_get reservation_id_gen

	-- increment counter
	db     <- db_get id
	db_put (db { reservation_id_gen = new_id + 1 })

	-- done
	return (ReservationId new_id)

-- get state of database
db_get :: (Db -> a) -> DbFn a
db_get = DbFn . gets

-- update state of database
db_put :: Db -> DbFn ()
db_put = DbFn . put

-- check if a given reservation has a given id
has_id id r = reservation_id r == id

-- check if list is not empty
notNull :: [a] -> Bool
notNull = not . null
