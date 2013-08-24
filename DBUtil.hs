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
main = do
  conn <- connectSqlite3 "train_res_prog_sp_ue3.db"
  putStrLn "Connecting to trains.db"

-- quick and dirty test

  quickQuery conn "CREATE TABLE test (id integer PRIMARY_KEY)" []
  quickQuery conn "INSERT INTO test VALUES (1)" []
  commit conn

{-view rows in test for testing purpose-}
view = do
  conn <- connectSqlite3 "train_res_prog_sp_ue3.db"
  quickQuery conn "SELECT * FROM test" []

{-clean up DB for testing purpose-}
clean = do
  conn <- connectSqlite3 "train_res_prog_sp_ue3.db"
  quickQuery conn "DROP TABLE test" []
  commit conn



