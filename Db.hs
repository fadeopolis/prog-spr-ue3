{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards    #-}

module Db(
	-- we don't export the Db constructor,
	-- _TEST_DB as starting point
	-- can only be altered by db_add_reservation and db_remove_reservation

	Db,
	reservations, trains, routes,

	-- need to be public for parser
	TrainId(..), TrainCarId(..), RouteId(..), ReservationId(..), Seat,

	Train(..), TrainCar(..), Route(..), Reservation,
	train_num_seats,

	Slot(..),
	slot_num_seats, slot_seats,
	slots_overlap, slot_contains_seat, slot_contains_slot,

	--TODO should we make these puplic?
	FreeSeatQuota(..), TrainCarNumSeats(..), City(..),

	-- we don't export the Reservation constructor,
	-- can only be safely created by db_add_reservation
	reservation_id, reservation_train, reservation_traincar, reservation_slot, reservation_route,
	
	--reservation_start, reservation_end,

	runDbFn,
	db_add_reservation, db_remove_reservation,
	db_print, db_println, db_error,

	_TEST_DB,

	db_empty, 
	db_add_trains, 
	db_add_routes,
	routes_overlap,

	reservation_id_gen,
	db_get,
	evalDbFn, evalDbFn',

	DbFn,
) where

import Control.Applicative
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
type    FreeSeatQuota    = Int
type    Seat             = Int

newtype TrainCarId       = TrainCarId       String  deriving (Show, Read, Eq)
type    TrainCarNumSeats = Int

newtype RouteId          = RouteId          String  deriving (Show, Read, Eq)
newtype City             = City             String  deriving (Show, Read, Eq, Ord)

newtype ReservationId    = ReservationId    Int     deriving (Show, Read, Eq)


-- a train (choo-choo)
data Train = Train {
	train_id             :: TrainId,      -- unique train identification
	train_route          :: Route,      -- route this train drives on
	train_cars           :: [TrainCar], -- list of all traincars of this train
	train_res_free_seats :: FreeSeatQuota -- quota of free seats which are not reservable
}
	deriving (Read, Eq)

deriving instance Show Train

train_num_seats Train{..} = foldl (+) 0 (map train_car_num_seats train_cars)

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

	reservation_train    :: TrainId,       -- train passengers ride in
	reservation_traincar :: TrainCarId,    -- train car passengers ride in
	reservation_slot     :: Slot,          -- the seats reserved in this reservation

	reservation_route    :: [City]         -- cities on one route
}
	deriving (Show, Read, Eq)


-- | A number of seats in a train car
data Slot = Slot {
	slot_first_seat :: Int,
	slot_last_seat  :: Int
}
	deriving (Show, Read, Eq, Ord)

slot_num_seats Slot{..} = slot_last_seat - slot_first_seat + 1

slot_seats Slot{..} = [slot_first_seat .. slot_last_seat]

-- | Check if slot contains a given seat
slot_contains_seat seat Slot{..} = slot_first_seat <= seat && seat <= slot_last_seat

-- | Check if a slot is fully contained in another slot
slot_contains_slot big small = slot_first_seat big <= slot_first_seat small && slot_last_seat small <= slot_last_seat big

-- | Checks if two Slots have any seats in common
slots_overlap a b = intersect (slot_seats a) (slot_seats b) /= []


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
db_add_reservation :: TrainId -> TrainCarId -> Slot -> [City] -> DbFn (Maybe Reservation)
db_add_reservation train traincar slot cities = do
	id <- new_reservation_id

	db_add_reservation' (Reservation id train traincar slot cities)

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
colliding_reservation a b =  check_city a b
--                          && check_reservation_seats_possible (a b
                          && check_reservation_car (reservation_train a) (reservation_traincar a) (reservation_train b) (reservation_traincar b)
                          && check_seats a b

get_train :: TrainId -> DbFn (Maybe Train)
get_train t = do
	ts <- db_get trains
	return (find (\t2 -> train_id t2 == t) ts)


-- check if it is the same Train and same CarId
check_reservation_car :: TrainId -> TrainCarId -> TrainId -> TrainCarId -> Bool
check_reservation_car tid tcid tid1 tcid1 = (tid == tid1) && (tcid == tcid1)

-- check if Reservation contains same Cities
check_city :: Reservation -> Reservation -> Bool
check_city a b = intersect (reservation_route a) (reservation_route b) /= []

check_reservation_seats_possible :: Int -> Int -> Bool
check_reservation_seats_possible max_free_seats num_seats = max_free_seats <= num_seats

check_seats :: Reservation -> Reservation -> Bool
check_seats a b = slots_overlap (reservation_slot a) (reservation_slot b)
--	(((first_seat r) + (num_seats r) - 1) < (first_seat r1) || (first_seat r) > ((first_seat r1) + (num_seats r1) - 1))


-- | Checks if two routes overlap
routes_overlap :: [City] -> [City] -> Bool
routes_overlap a b = intersect a b /= [] 

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

instance Applicative DbFn where
	pure = return

	mf <*> ma = do
		f <- mf
		a <- ma
		return (f a)

instance Functor DbFn where
	fmap f ma = do
		a <- ma
		return (f a)

-- add reservation if it has no collisions
db_add_reservation' :: Reservation -> DbFn (Maybe Reservation)
db_add_reservation' r = do
	db               <- db_get id
	all_reservations <- db_get reservations

	db_trace r

	let collisions = colliding_reservations r all_reservations
	
	db_trace collisions
	
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
