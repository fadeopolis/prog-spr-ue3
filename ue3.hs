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


main :: IO ()
main = do
	let db_path = "zug.db"

	putStrLn ">> STARTING APP"

	db_exists <- doesFileExist db_path

	when (not db_exists) $ do
		putStrLn ">> CREATING DATABASE"
		writeDb db_path db

	putStrLn ">> READING DATABASE"
	db    <- readDb db_path
	putStrLn ">> READ DATABASE"

	db' <- mainLoop db

	putStrLn ">> WRITING DATABASE"
	writeDb db_path db' -- process changes in DB
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
