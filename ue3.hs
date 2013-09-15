{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns    #-}

import Db
import Parser
import Trains

import Data.List
import System.IO hiding (readFile)
import System.Directory
import Control.Monad

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

-- Mindestanzahl der freien und maximale Anzahl der durch Reservierung belegten Plätze pro Zug
-- und Waggon zwischen je zwei Stationen (wobei sich Minimum und Maximum darauf beziehen, 
-- dass Reservierungen möglicherweise nur auf Teilen der abgefragten Strecke existieren); 



-- Mindestanzahl der zwischen zwei Stationen freien und der noch reservierbaren Plätze sowie die maximale Gruppengröße
-- (= maximale Zahl der Personen pro Reservierung) für einen Zug oder mehrere gegebene Züge (wenn Umsteigen nötig ist). 

-- | Input a 
{-
query_4 :: Train -> City -> City -> DbFn (Int, Int)
query_4 t start stop = do
	rs  <- db_get reservations
	rs' <- return (filter (\r -> train_id (train r) == train_id t) rs) -- we only care about reservations for the same train

	if 

-}

reserved_seats_for_car :: TrainCarId -> DbFn [(Int, Int)]
reserved_seats_for_car id = do
	rs <- db_reservations_for_traincar id

	return (map (\r -> (first_seat r, num_seats r)) rs)

-- | The number of reserved seats for a traincar
num_reserved_seats_for_car :: TrainCar -> DbFn Int
num_reserved_seats_for_car car = do
	rs <- db_reservations_for_traincar (train_car_id car)

	return (sum (map num_seats rs))

-- | The number of free seats for a traincar
num_free_seats_for_car :: TrainCar -> DbFn Int
num_free_seats_for_car car = do
	all      <- return (train_car_num_seats car)
	reserved <- num_reserved_seats_for_car car

	return (all - reserved)

-- | The total number of seats in a train
num_seats_for_train :: Train -> Int
num_seats_for_train t = sum (map train_car_num_seats (train_cars t))

-- | The number of reserved seats in a train
num_reserved_seats_for_train :: Train -> DbFn Int
num_reserved_seats_for_train t = do
	nums <- mapM num_reserved_seats_for_car (train_cars t)

	return (sum nums)

-- | The number of free seats in a train
num_free_seats_for_train :: Train -> DbFn Int
num_free_seats_for_train t = do
	all      <- return (num_seats_for_train t)
	reserved <- num_reserved_seats_for_train t

	return (all - reserved)


free_seats :: TrainCarId -> [(Int, Int)]
free_seats = todo

db_reservations_for_traincar :: TrainCarId -> DbFn [Reservation]
db_reservations_for_traincar t = do
	rs  <- db_get reservations
	
	return (filter (\r -> traincar r == t) rs)


main :: IO ()
main = do
	let db_path = "zug.db"

	putStrLn ">> STARTING APP"

	db_exists <- doesFileExist db_path

	when (not db_exists) $ do
		putStrLn ">> CREATING DATABASE"
		writeDb db_path db

	putStrLn ">> READING DATABASE"
	db'    <- readDb db_path
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
