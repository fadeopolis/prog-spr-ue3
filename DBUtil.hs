-- Database Utility connects to a MySQL DB
--
-- Programmiersprachen ue3
-- Author: Raunig Stefan

module DBUtil where
 
import Database.HDBC
import Database.HDBC.MySQL
 
main =
   do conn <- connectMySQL defaultMySQLConnectInfo {
                  mysqlHost		= "localhost",
                  mysqlDatabase		= "train_res_prog_sp_ue3",
                  mysqlUser		= "root",
                  mysqlPassword 	= "",
                  mysqlUnixSocket	= "/var/run/mysqld/mysqld.sock" }

-- quick and dirty test
quickQuery conn "INSERT INTO test VALUES (1)" []
