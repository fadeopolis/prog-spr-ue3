{-# LANGUAGE RecordWildCards #-}

import Db
import Data.List

data TODO = TODO

todo      = error "TODO"
todo' msg = error ("TODO: " ++ msg)

{- a action parsed from user input,
 - takes a Db, produces a new Db and some output to print 
 - and a flag telling us if we are done
-}
type Command = Db -> (Db, String, Bool)

main :: IO ()
main = do
	putStrLn ">> STARTING APP"

	let filepath = "zug.db"

	putStrLn ">> READING DATABASE"
	db    <- readDb filepath
	putStrLn ">> READ DATABASE"

	input <- getContents -- get user input from stdin

	(db', _) <- mainLoop db input -- process user input in mainLoop

	putStrLn ">> WRITING DATABASE"
	writeDb filepath db' -- process changes in DB
	putStrLn ">> WROTE DATABASE"

	putStrLn ">> DONE"

mainLoop :: Db -> String -> IO (Db, String)
mainLoop db input = do
	(db', input', done) <- loopStep db input

	if done
	then
		return (db', input')
	else
		mainLoop db' input'

loopStep :: Db -> String -> IO (Db, String, Bool)
loopStep db input = do
	-- execute command
	let (cmd, input') = parseCommand input

	-- execute command
	let (db', output, done) = cmd db

	-- write output of command
	putStrLn output

	return (db', input', done)

readDb :: FilePath -> IO Db
readDb file = return _TEST_DB

writeDb :: FilePath -> Db -> IO ()
writeDb = todo' "writeDb"

parseCommand :: String -> (Command, String)
parseCommand str = f (lines str)
	where
		f []          = (\db -> (db, "INPUT EMPTY", True), [])
		f (line:rest) = (\db -> (db, "READ: " ++ line, False), unlines rest)
