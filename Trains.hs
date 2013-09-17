module Trains where

import Db

-- Data
u1_01 = TrainId "u1_01"
u1_02 = TrainId "u1_02"
--u1_03 = TrainId "u1_03"
--u1_04 = TrainId "u1_04"

u2_01 = TrainId "u2_01"
u2_02 = TrainId "u2_02"
--u2_03 = TrainId "u2_03"
--u2_04 = TrainId "u2_04"

u3_01 = TrainId "u3_01"
u3_02 = TrainId "u3_02"
--u3_03 = TrainId "u3_03"
--u3_04 = TrainId "u3_04"

u4_01 = TrainId "u4_01"
u4_02 = TrainId "u4_02"
--u4_03 = TrainId "u4_03"
--u4_04 = TrainId "u4_04"

u6_01 = TrainId "u6_01"
u6_02 = TrainId "u6_02"
--u6_03 = TrainId "u6_03"
--u6_04 = TrainId "u6_04"

quota_1 = 5
quota_2 = 10
quota_3 = 15
quota_4 = 20

car_id_01 = TrainCarId "car_01"
car_id_02 = TrainCarId "car_02"
car_id_03 = TrainCarId "car_03"
car_id_04 = TrainCarId "car_04"
car_id_05 = TrainCarId "car_05"

car_id_06 = TrainCarId "car_06"
car_id_07 = TrainCarId "car_07"
car_id_08 = TrainCarId "car_08"
car_id_09 = TrainCarId "car_09"
car_id_10 = TrainCarId "car_10"

car_id_11 = TrainCarId "car_11"
car_id_12 = TrainCarId "car_12"
car_id_13 = TrainCarId "car_13"
car_id_14 = TrainCarId "car_14"
car_id_15 = TrainCarId "car_15"

car_id_16 = TrainCarId "car_16"
car_id_17 = TrainCarId "car_17"
car_id_18 = TrainCarId "car_18"
car_id_19 = TrainCarId "car_19"
car_id_20 = TrainCarId "car_20"

num_seats_01 = 20
num_seats_02 = 30
num_seats_03 = 40
num_seats_04 = 50

--Cities
c_01 = City "Stadion"
c_02 = City "Krieau"
c_03 = City "Messe-Prater"
c_04 = City "Praterstern"
c_05 = City "Taborstrasse"
c_06 = City "Schottenring"
c_07 = City "Schottentor"
c_08 = City "Rathaus"
c_09 = City "Volkstheater"
c_10 = City "Museumsquartier"
c_11 = City "Karlsplatz"
c_12 = City "Stadtpark"
c_13 = City "Landstrasse"
c_14 = City "Schwedenplatz"
c_15 = City "RossauerLaende"
c_16 = City "Friedensbruecke"
c_17 = City "Nestroyplatz"
c_18 = City "Stephansplatz"
c_19 = City "Taubstummengasse"
c_20 = City "SuedtirolerPlatz"
c_21 = City "Herrengasse"
c_22 = City "Stubentor"
c_23 = City "Rochusgasse"
c_24 = City "KardinalNaglPlatz"

route_id_01 = RouteId "U1"
route_id_02 = RouteId "U2"
route_id_03 = RouteId "U3"
route_id_04 = RouteId "U4"
route_id_05 = RouteId "U6"

--Route
route_01 = Route route_id_01 [c_04, c_17, c_14, c_18, c_11, c_19, c_20]
route_02 = Route route_id_02 [c_01, c_02, c_03, c_04, c_05, c_06, c_07, c_08, c_09, c_10, c_11]
route_03 = Route route_id_03 [c_09, c_21, c_18, c_22, c_13, c_23, c_24]
route_04 = Route route_id_04 [c_15, c_06, c_14, c_13, c_12, c_11] --Rossauerlaende - Schottentor - Stephansplatz - Stadtpark - Rochusgasse
route_05 = Route route_id_05 [c_15, c_07, c_18, c_12, c_23]

--TrainCars
car_01 = TrainCar car_id_01 num_seats_01
car_02 = TrainCar car_id_02 num_seats_01
car_03 = TrainCar car_id_03 num_seats_02
car_04 = TrainCar car_id_04 num_seats_02
car_05 = TrainCar car_id_05 num_seats_03

car_06 = TrainCar car_id_06 num_seats_03
car_07 = TrainCar car_id_07 num_seats_04
car_08 = TrainCar car_id_08 num_seats_04
car_09 = TrainCar car_id_09 num_seats_01
car_10 = TrainCar car_id_10 num_seats_01

car_11 = TrainCar car_id_11 num_seats_02
car_12 = TrainCar car_id_12 num_seats_02
car_13 = TrainCar car_id_13 num_seats_03
car_14 = TrainCar car_id_14 num_seats_03
car_15 = TrainCar car_id_15 num_seats_04

car_16 = TrainCar car_id_16 num_seats_04
car_17 = TrainCar car_id_17 num_seats_01
car_18 = TrainCar car_id_18 num_seats_01
car_19 = TrainCar car_id_19 num_seats_02
car_20 = TrainCar car_id_20 num_seats_02

--Train
t01 = Train u1_01 route_01 [car_01, car_02] quota_1
t02 = Train u1_02 route_01 [car_03, car_04] quota_1

t03 = Train u2_01 route_02 [car_05, car_06] quota_2
t04 = Train u2_02 route_02 [car_07, car_08] quota_2

t05 = Train u3_01 route_03 [car_09, car_10] quota_1
t06 = Train u3_02 route_03 [car_11, car_12] quota_4

t07 = Train u4_01 route_04 [car_13, car_14] quota_1
t08 = Train u4_02 route_04 [car_15, car_16] quota_2

t09 = Train u6_01 route_05 [car_17, car_18] quota_2
t10 = Train u6_02 route_05 [car_19, car_20] quota_3

-- The actual database
db = third $ runDbFn db_empty $ do
	db_add_trains [t01, t02, t03, t04, t05, t06, t07, t08, t09, t10]
	db_add_routes [route_01, route_02, route_03, route_04, route_05]

third (_, _, c) = c
