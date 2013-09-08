module Db(
	-- we don't export the Db constructor,
	-- _TEST_DB as starting point
	-- can only be altered by db_add_reservation and db_remove_reservation
	Db,

	--TrainId(..), TrainCarId(..), RouteId(..), ReservationId(..),

	--TODO should we make these puplic?
	--FreeSeatQuota(..), TrainCarNumSeats(..), City(..),

	Train(..), TrainCar(..), Route(..),

	-- we don't export the Reservation constructor,
	-- can only be safely created by db_add_reservation
	reservation_id, train, traincar, first_seat, num_seats, reservation_start, reservation_end,

	runDbFn,
	db_add_reservation, db_remove_reservation,
	db_print, db_println, db_error,

	_TEST_DB
) where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Data.List

-- TODO 
data TODO = TODO

todo      = error "TODO"
todo' msg = error ("TODO: " ++ msg)
-- TODO

-- database id types
newtype TrainId          = TrainId          String  deriving (Show, Read, Eq)
newtype FreeSeatQuota    = FreeSeatQuota    Int     deriving (Show, Read, Eq)

newtype TrainCarId       = TrainCarId       String  deriving (Show, Read, Eq)
newtype TrainCarNumSeats = TrainCarNumSeats Int     deriving (Show, Read, Eq)

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
	deriving (Show, Read, Eq)

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
	reservation_id    :: ReservationId, -- must be unique

	train             :: TrainId,         -- train passengers ride in
	traincar          :: TrainCarId,      -- train car passengers ride in
	first_seat        :: Int,           -- seat in train car where reservation starts
	num_seats         :: Int,           -- number of seats reserved

	reservation_start :: City,          -- must be in route
	reservation_end   :: City           -- must be in route
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
newtype DbFn a = DbFn { unDbFn :: (StateT Db (Writer String) a) }

-- run db function
runDbFn :: DbFn a -> Db -> (a, String, Db)
runDbFn (DbFn state) db = (a, output, db')
	where
		((a, db'), output) = runWriter $ runStateT state db

-- add reservation to db
-- returns (Just Reservation) if reservation was added
-- returns Nothing if reservation is invalid
db_add_reservation :: TrainId -> TrainCarId -> Int -> Int -> City -> City -> DbFn (Maybe Reservation)
db_add_reservation train traincar first_seat num_seats start stop = do
	id <- new_reservation_id

	db_add_reservation' (Reservation id train traincar first_seat num_seats start stop)

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

-- computes all Reservations that collide with a given reservation
colliding_reservations :: Reservation -> [Reservation] -> [Reservation]
colliding_reservations r rs = todo

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
