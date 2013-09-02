-- Database Utility connects to a MySQL DB
--
-- Programmiersprachen ue3
-- Author: Raunig Stefan

module DBUtil where

import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL
import Database.HDBC.Sqlite3

{-
-- connects to a mysql database running on localhost server
-- this is a lampp (xammp) socket use /var/lib/mysqld/mysqld.sock otherwise 
--
-}
{-
main = do
  rows <- withRTSSignalsBlocked $ do
     conn <- connectMySQL defaultMySQLConnectInfo {
              mysqlHost       = "localhost",
              mysqlDatabase   = "test",
              mysqlUser       = "root",
              mysqlPassword   = "",
              mysqlUnixSocket = "/opt/lampp/var/mysql/mysql.sock"
            }
     putStrLn "Connection established"          
     quickQuery conn "INSERT INTO test VALUES (1)" []
  forM_ rows $ \row -> putStrLn $ show row
-}

{-creates a DB file in the current working directory-}
testMain = do
  conn <- connectSqlite3 "train.db"
  putStrLn "Connecting to trains.db"

-- quick and dirty test

  quickQuery conn "CREATE TABLE test (id integer PRIMARY_KEY)" []
  quickQuery conn "INSERT INTO test VALUES (1)" []
  commit conn

{-view rows in test for testing purpose-}
testView = do
  conn <- connectSqlite3 "train.db"
  quickQuery conn "SELECT * FROM test" []

{-clean up DB for testing purpose-}
testClean = do
  conn <- connectSqlite3 "train.db"
  quickQuery conn "DROP TABLE test" []
  commit conn


{-creates a DB file in the current working directory and generates table structure-}
main = do
  conn <- connectSqlite3 "train.db"
  putStrLn "Connecting to trains.db"

--edit
  quickQuery conn "CREATE TABLE IF NOT EXISTS `Train`                  (`TrainId`       INTEGER PRIMARY KEY AUTOINCREMENT,                  `TrainCarsId`    int(11) NOT NULL,                  `MaxTrainCars`   int(11) NOT NULL,                  `FreeSeatsQuota` int(11) NOT NULL)" []

  quickQuery conn "INSERT INTO Train VALUES (1, 1, 1, 1)" []
  commit conn

--just copied because to lazy
newtype TrainId       = TrainId       String  deriving (Show, Read, Eq)
newtype TrainCarId    = TrainCarId    String  deriving (Show, Read, Eq)
newtype RouteId       = RouteId       String  deriving (Show, Read, Eq)
newtype ReservationId = ReservationId String  deriving (Show, Read, Eq)

data Train = Train {
  train_id     :: TrainId,
  train_car_id :: TrainCarId,
  train_route  :: RouteId  -- route this train drives on
}
  deriving (Show, Read, Eq)

{-
 - Helper func: convRowToString
 - converts the output of a sql query to a String
 -}
convRowToString :: [SqlValue] -> String
convRowToString [sqlId, sqlDesc] = 
  show intid ++ ": " ++ desc
  where intid = (fromSql sqlId)::Integer
        desc = case fromSql sqlDesc of
               Just x -> x
               Nothing -> "NULL"
convRowToString x = fail $ "Unexpected result: " ++ show x

{-
 - func: convert
 - 
 - fill raw db values into types
 -}
getTrainById :: Integer -> IO()
getTrainById input = do
  conn      <- connectSqlite3 "train.db"
  sqlResult <- quickQuery conn "SELECT TrainId, TrainCarsId FROM Train where TrainId == ?" [toSql (input::Integer)]

  let sqlString = map convRowToString sqlResult
-- Print the rows out
  mapM_ putStrLn sqlString

-- And disconnect from the database
  disconnect conn

{-
 - func: view
 - view rows in TRAIN for testing purpose
 -}
view = do
  conn <- connectSqlite3 "train.db"
  quickQuery conn "SELECT * FROM Train" []


{-
 - func: clean
 - clean up DB for testing purpose
 -}
clean = do
  conn <- connectSqlite3 "train.db"
  quickQuery conn "DROP TABLE train" []
  commit conn
