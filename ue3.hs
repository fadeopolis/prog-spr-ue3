{-# LANGUAGE RecordWildCards #-}

import Db
import Parser
import Trains

import Data.List
import System.IO.Strict(readFile)
import System.Directory
import Control.Monad

import Prelude hiding (readFile)

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

	input  <- getContents          -- get user input from stdin
	input' <- return (lines input) -- split input into lines

	db' <- foldM step db input' -- process user input in mainLoop

	putStrLn ">> WRITING DATABASE"
	writeDb db_path db' -- process changes in DB
	putStrLn ">> WROTE DATABASE"

	putStrLn ">> DONE"

step :: Db -> String -> IO Db
step db input = do
	let cmd = parse_command commands input

	let (_, output, db') = runDbFn db cmd

	putStrLn output
	return db'

readDb :: FilePath -> IO Db
readDb path = do
	text <- readFile path
	return (read text)

writeDb :: FilePath -> Db -> IO ()
writeDb file db = writeFile file (show db)
