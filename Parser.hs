{-# LANGUAGE RecordWildCards #-}

module Parser(
	Commands(..),

	parse_command
) where

import Control.Applicative
import Data.Char
import Data.List

import Db(City(..), TrainId(..), TrainCarId(..), RouteId(..), ReservationId(..))

-- | a collection of callbacks
data Commands a = Commands {
	show_all_trains  :: a,
	show_all_routes  :: a,

	show_train       :: TrainId               -> a,
	show_train_car   :: TrainId -> TrainCarId -> a,
	show_route       :: RouteId               -> a,
	show_city        :: City                  -> a,
	show_reservation :: ReservationId         -> a,

	-- called when the parser didn't know command you gave it
	unknown_command  :: String                -> a
}

-- for testing the parser
dummy_commands = Commands {
	show_all_trains  = "All trains",
	show_all_routes  = "All routes",

	show_train       = \train           -> "CHOO CHOO   " ++ show train,
	show_train_car   = \train train_car -> "TRAIN CAR   " ++ show train_car,
	show_route       = \route           -> "ROUTE       " ++ show route,
	show_city        = \city            -> "CITY        " ++ show city,
	show_reservation = \reservation     -> "RESERVATION " ++ show reservation,

	unknown_command  = \cmd -> "Unknown command: '" ++ cmd ++ "'"
}

-- | Takes a set of callbacks and a string to parse.
-- | Returns either the result of a callback or an error message
parse_command :: Commands a -> String -> Either String a
parse_command cmds str = unwrap (runParser (command_parser cmds) str)
	where
		unwrap (Success a   s') = Right a
		unwrap (Error   e a s') = Left  e
		unwrap (Commit  c)      = unwrap c 

-- ***** MAIN COMMAND PARSERS ************

command_parser :: Commands a -> Parser a
command_parser cmds = do
	cmd <- command cmds -- parse command
	spaces              -- skip whitespace at end of input
	eof                 -- check that all input was consumed
	return cmd
 
command Commands{..} = try [
	phrase "show all trains"  >> return show_all_trains,
	phrase "show all routes"  >> return show_all_routes,
	phrase "show train"       >> show_train       <$> train_id,
	phrase "show traincar"    >> show_train_car   <$> train_id <*> train_car_id,
	phrase "show route"       >> show_route       <$> route_id,
	phrase "show city"        >> show_city        <$> city,
	phrase "show reservation" >> show_reservation <$> reservation_id,
	unknown_command <$> remaining]

city :: Parser City
city = fmap City string

train_id :: Parser TrainId
train_id = fmap TrainId string 

train_car_id :: Parser TrainCarId
train_car_id = fmap TrainCarId string

route_id :: Parser RouteId
route_id = fmap RouteId string

reservation_id :: Parser ReservationId
reservation_id = fmap ReservationId number

-- ***** MAIN HELPER PARSERS ************

-- | Parser that takes a whitespace separated list of words and parses
-- | them with the `tokens' parser
phrase :: String -> Parser [String]
phrase = tokens . words

-- | Parser that parses a decimal number
number :: Parser Int
number = do
	spaces
	d <- digit
	commit " a number " $ do
		ds <- many digit
		return (read (d:ds))

-- | Parser that parses a non empty string that doesn't contain whitespace
string :: Parser String
string = spaces >> some any_char

-- | Tries one parser after the other, the first success is returned
try :: [Parser a] -> Parser a
try = foldl1 (<|>)

-- ***** SIMPLE PARSERS ************

-- | Parser that skips whitespace and then expects a whitespace separated list of word
tokens :: [String] -> Parser [String]
tokens = mapM token

-- | Parser that skips whitespace and then expects a word
token :: String -> Parser String
token w = spaces >> word w

-- | Parser that expects a given string
word = mapM char

-- | Parser that expects a given char,
-- | NOTE: This parser is case insensititve!
char :: Char -> Parser Char
char c = sat any_char (\a -> toUpper a == toUpper c) ("'" ++ [c] ++ "'")

-- | Parser that expects a decimal digit
digit :: Parser Char
digit = sat any_char isDigit "a digit"

-- | Parser that skips zero or more whitespace chars
spaces :: Parser ()
spaces = many space >> return ()

-- | Parser that skips one whitespace char
space :: Parser ()
space = sat any_char isSpace "whitespace" >> return ()

-- | Parser that consumes all remaining input
remaining :: Parser String
remaining = Parser (\cs -> Success cs [])

-- | Parser that fails if any input is left
eof :: Parser ()
eof = Parser f
	where
		f [] = Success ()                                                                       []
		f cs = Error   "end of input" ("Unexpected characters at end of input: '" ++ cs ++ "'") cs

-- ***** CORE PARSERS ************

-- | Parses a single char, fails if input is empty
any_char :: Parser Char
any_char = Parser f
	where
		f (c:cs) = Success c                            cs
		f []     = Error   "a character" "end of input" []

-- | Parser that checks result of another parser
sat :: Show a => Parser a -> (a -> Bool) -> String -> Parser a
sat p pred expected = Parser (bind . runParser p)
	where
		bind (Success a   s') | pred a    = Success a                 s'
		                      | otherwise = Error   expected (show a) s'
		bind (Error   _ a s')             = Error   expected a        s'
		bind (Commit  c)                  = Commit  (bind c)

commit :: String -> Parser a -> Parser a
commit expected p = Parser (Commit . runParser p)

expecting expected p = Parser (bind . runParser p)
	where
		bind (Success a   s') = Success a                 s'
		bind (Error   _ a s') = Error   expected a        s'
		bind (Commit  c)      = Commit  (bind c)

-- ***** PARSER TYPE 

data Parser a = Parser { runParser :: String -> ParseResult a }

-- internal type that parsers return
data ParseResult a = Success a             String
                   | Error   String String String  -- 'expected input' * 'error message' * 'remaining input'
                   | Commit  (ParseResult a)       -- can't backtrack accross commit
	deriving (Show)

instance Monad Parser where
	fail   e = Parser (Error   e "")
	return a = Parser (Success a)

	p >>= f = Parser (bind . runParser p)
		where
			bind (Success a   s') = runParser (f a) s' -- continue with next parser if p succeeds
			bind (Error   e a s') = Error e a s'       -- abort with error if p fails
			bind (Commit  c)      = Commit (bind c)    -- remember commit marker

instance Functor Parser where
	fmap f p = do
		a <- p
		return (f a)  

instance Applicative Parser where
	pure      = return
	pf <*> pa = do
		f <- pf
		a <- pa
		return (f a)

instance Alternative Parser where
	empty   = fail "Unexpected parsing error"
	p <|> q = Parser (\s -> bind s (runParser p s))
		where
			bind s (Success a   s') = Success a s'   -- ignore q if p succeeds
			bind s (Error   e a s') = runParser q s  -- if p fails, try q
			bind s (Commit  c)      = Commit c       -- if p commits, keep it's result, even if it's an error
