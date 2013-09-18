{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}

module Db(
	-- ** DATABASE FUNCTIONS
	Db,
	db_reservations, db_trains, db_routes,

	empty_db,

	db_add_trains,
	db_add_routes,

	db_add_reservation,
	db_remove_reservation,

	-- ** DB ENTITIES
	Train(..),
	TrainId(..),
	train_num_seats,

	Reservation,
	ReservationId(..),
	reservation_id,
	reservation_train,
	reservation_traincar,
	reservation_slot,
	reservation_route,

	TrainCar(..),
	TrainCarId(..),

	Route(..),
	RouteId(..),
	route_from_endpoints,

	City(..),
	routes_overlap,

	find_train_by_id,
	find_traincar_by_id,
	find_reservation_by_id,
	find_route_by_id,

	-- ** HELPERS
	Seat,

	Slot(..),
	slot_num_seats, slot_seats,
	slots_overlap, slot_contains_seat, slot_contains_slot,

	FreeSeatQuota, 
	TrainCarNumSeats, 

	IsString
) where

import Result

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Data.List
import Data.Maybe
import Data.String
import Debug.Trace

todo = error "TODO"

-- database id types
newtype TrainId          = TrainId { tId :: String }        deriving (Show, Read, Eq)
type    FreeSeatQuota    = Int
type    Seat             = Int

newtype TrainCarId       = TrainCarId { tcId :: String }    deriving (Show, Read, Eq)
type    TrainCarNumSeats = Int

newtype RouteId          = RouteId { rId :: String }        deriving (Show, Read, Eq)
newtype City             = City { city_name :: String }     deriving (Show, Read, Eq, Ord)

newtype ReservationId    = ReservationId { rIdNum ::  Int } deriving (Show, Read, Eq)

-- make testing from the repl easier
instance IsString TrainId    where fromString = TrainId
instance IsString TrainCarId where fromString = TrainCarId
instance IsString RouteId    where fromString = RouteId
instance IsString City       where fromString = City

-- a train (choo-choo)
data Train = Train {
	train_id             :: TrainId,      -- unique train identification
	train_route          :: Route,      -- route this train drives on
	train_cars           :: [TrainCar], -- list of all traincars of this train
	train_res_free_seats :: FreeSeatQuota -- quota of free seats which are not reservable
}
	deriving (Read, Eq)

deriving instance Show Train

train_num_seats Train{..} = foldl (+) 0 (map traincar_num_seats train_cars)

-- one car in a train
data TrainCar = TrainCar {
	traincar_id        :: TrainCarId,      -- unique traincar identification
	traincar_num_seats :: TrainCarNumSeats -- number of seats available in a traincar
}
	deriving (Show, Read, Eq)



-- a route from town A to town B, with stops etc.
data Route = Route {
	route_id     :: RouteId,
	route_cities :: [City]
}
	deriving (Show, Read, Eq)

data Reservation = Reservation {
	reservation_id       :: ReservationId, -- must be unique

	reservation_train    :: TrainId,       -- train passengers ride in
	reservation_traincar :: TrainCarId,    -- train car passengers ride in
	reservation_slot     :: Slot,          -- the seats reserved in this reservation

	reservation_route    :: [City]         -- cities on one route
}
	deriving (Show, Read)

instance Eq Reservation where
	a == b = reservation_id a == reservation_id b


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
	db_trains             :: [Train],      -- and traincars
	db_routes             :: [Route],      -- and cities

	-- changing through time
	db_reservations       :: [Reservation],
	db_reservation_id_gen :: Int            -- used to generate unique reservation ids
}
	deriving (Show, Read, Eq)

-- hard coded db for testing
_TEST_DB = Db {
	db_trains = [], --[Train (TrainId "U1") (RouteId "11"), Train (TrainId "U2") (RouteId "21"), Train (TrainId "U4") (RouteId "41")],
	db_routes = [], --[Route (RouteId "11") []],

	db_reservations       = [],
	db_reservation_id_gen = 0
}

-- add reservation to db
-- returns (Just Reservation) if reservation was added
-- returns Nothing if reservation is invalid
db_add_reservation :: TrainId -> TrainCarId -> Slot -> City -> City -> Db -> Result (Db, Reservation)
db_add_reservation train_id traincar_id slot start stop db = do
	-- check if reservation is valid

	train    <- find_train_by_id     train_id    db  -- check train id
	traincar <- find_traincar_by_id  traincar_id db  -- check traincar id
	route    <- route_from_endpoints start stop  db  -- check start/stop station

	when (traincar_contains_slot traincar            slot)  (fail ("Train car" ++ show traincar_id ++ " does not contain slot " ++ show slot))
	when (is_subroute            (train_route train) route) (fail ("Train "    ++ show train_id    ++ " does not drive route "  ++ show route))

	let (db', id)   = new_reservation_id db
	let reservation = Reservation id train_id traincar_id slot route

	check_for_reservation_collisions reservation db'

	let db'' = db { db_reservations = reservation : db_reservations db }

	return (db'', reservation)

find_train_by_id       id db = findById "train"       id train_id        tId              db_trains                          db
find_traincar_by_id    id db = findById "train car"   id traincar_id     tcId             (concatMap train_cars . db_trains) db
find_reservation_by_id id db = findById "reservation" id reservation_id  (show . rIdNum)  db_reservations                    db
find_route_by_id       id db = findById "route"       id route_id        rId              db_routes                          db

findById name id get_id show_id get_all db = maybe (fail error) return value
	where
		value = find (\v -> id == get_id v) (get_all db)
		error = "Could not find " ++ name ++ " for id " ++ show_id id

traincar_contains_slot traincar slot = slot_seats slot `isInfixOf` traincar_seats traincar

route_from_endpoints :: City -> City -> Db -> Result [City]
route_from_endpoints start stop db = maybe (fail error) return (msum (map check_routes (db_routes db)))
	where
		error = "Invalid endpoints: " ++ city_name start ++ ", " ++ city_name stop

		check_routes candidate = do
			let cities = route_cities candidate

			startIndex <- findIndex (==start) cities
			stopIndex  <- findIndex (==stop)  cities

			guard (startIndex < stopIndex)

			return cities


traincar_seats TrainCar{..} = [1 .. traincar_num_seats]

is_subroute Route{..} r = r `isInfixOf` route_cities

check_for_reservation_collisions r db = mapM_ (check_for_reservation_collision r) (db_reservations db)

check_for_reservation_collision :: Reservation -> Reservation -> Result ()
check_for_reservation_collision new old = when rs_collide (fail err_msg)
	where
		rs_collide =  (reservation_train    new == reservation_train    old)
		           && (reservation_traincar new == reservation_traincar old)
		           && common_stops /= []
		           && common_seats /= []

		common_stops = intersect (reservation_route new)             (reservation_route old)
		common_seats = intersect (slot_seats (reservation_slot new)) (slot_seats (reservation_slot old))

		err_msg =  "Collision with reservation " ++ show (rIdNum (reservation_id old))
		        ++ " on seats "                  ++ show common_seats
		        ++ " between stops "             ++ concat (intersperse ", " (map city_name common_stops))

-- remove all reservations with given id
db_remove_reservation :: Reservation -> Db -> Db
db_remove_reservation reservation db = db { db_reservations = delete reservation (db_reservations db) }

-- | Checks if two routes overlap
routes_overlap :: [City] -> [City] -> Bool
routes_overlap a b = intersect a b /= [] 


-- FOR CREATING DB ------------------------------------------------------

-- | An empty database
empty_db :: Db
empty_db = Db {
	db_trains = [],
	db_routes = [],

	db_reservations       = [],
	db_reservation_id_gen = 0
}

db_add_trains :: [Train] -> Db -> Db
db_add_trains ts db = db { db_trains = ts ++ db_trains db }

db_add_routes :: [Route] -> Db -> Db
db_add_routes rs db = db { db_routes = rs ++ db_routes db }


-- INTERNALS ------------------------------------------------------------

-- create a new unique reservation id
new_reservation_id :: Db -> (Db, ReservationId)
new_reservation_id db = (new_db, new_id)
	where
		new_db = db { db_reservation_id_gen = db_reservation_id_gen db + 1 }
		new_id = ReservationId (db_reservation_id_gen db)

-- check if list is not empty
notNull :: [a] -> Bool
notNull = not . null
