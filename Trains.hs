module Trains where

-- Data
TrainId u1_01 = TrainId "U1_01"
TrainId u1_02 = TrainId "U1_02"
--TrainId u1_03 = TrainId "U1_03"
--TrainId u1_04 = TrainId "U1_04"

TrainId u2_01 = TrainId "U2_01"
TrainId u2_02 = TrainId "U2_02"
--TrainId u2_03 = TrainId "U2_03"
--TrainId u2_04 = TrainId "U2_04"

TrainId u3_01 = TrainId "U3_01"
TrainId u3_02 = TrainId "U3_02"
--TrainId u3_03 = TrainId "U3_03"
--TrainId u3_04 = TrainId "U3_04"

TrainId u4_01 = TrainId "U4_01"
TrainId u4_02 = TrainId "U4_02"
--TrainId u4_03 = TrainId "U4_03"
--TrainId u4_04 = TrainId "U4_04"

TrainId u6_01 = TrainId "U6_01"
TrainId u6_02 = TrainId "U6_02"
--TrainId u6_03 = TrainId "U6_03"
--TrainId u6_04 = TrainId "U6_04"

FreeSeatQuota quota_1 = FreeSeatQuota 5
FreeSeatQuota quota_2 = FreeSeatQuota 10
FreeSeatQuota quota_3 = FreeSeatQuota 15
FreeSeatQuota quota_4 = FreeSeatQuota 20

TrainCarId car_id_01 = TrainCarId "car_01"
TrainCarId car_id_02 = TrainCarId "car_02"
TrainCarId car_id_03 = TrainCarId "car_03"
TrainCarId car_id_04 = TrainCarId "car_04"
TrainCarId car_id_05 = TrainCarId "car_05"

TrainCarId car_id_06 = TrainCarId "car_06"
TrainCarId car_id_07 = TrainCarId "car_07"
TrainCarId car_id_08 = TrainCarId "car_08"
TrainCarId car_id_09 = TrainCarId "car_09"
TrainCarId car_id_10 = TrainCarId "car_10"

TrainCarId car_id_11 = TrainCarId "car_11"
TrainCarId car_id_12 = TrainCarId "car_12"
TrainCarId car_id_13 = TrainCarId "car_13"
TrainCarId car_id_14 = TrainCarId "car_14"
TrainCarId car_id_15 = TrainCarId "car_15"

TrainCarId car_id_16 = TrainCarId "car_16"
TrainCarId car_id_17 = TrainCarId "car_17"
TrainCarId car_id_18 = TrainCarId "car_18"
TrainCarId car_id_19 = TrainCarId "car_19"
TrainCarId car_id_20 = TrainCarId "car_20"

TrainCarNumSeats num_seats_01 = TrainCarNumSeats 20
TrainCarNumSeats num_seats_02 = TrainCarNumSeats 30
TrainCarNumSeats num_seats_03 = TrainCarNumSeats 40
TrainCarNumSeats num_seats_04 = TrainCarNumSeats 50

--Cities
City c_01 = City "TU Wien"
City c_02 = City "Uni Wien"
City c_03 = City "FH Technikum"
City c_04 = City "Boku Wien"
City c_05 = City "WU Wien"
City c_06 = City "Lauder Business School"
City c_07 = City "Akademie der Bildenden Kuenste"
City c_08 = City "Konservatorium"
City c_09 = City "FH bfi"
City c_10 = City "FH Technikum"
City c_11 = City "FH Campus"
City c_12 = City "FH Wien"

RouteId route_id_01 = RouteId "U1"
RouteId route_id_02 = RouteId "U2"
RouteId route_id_03 = RouteId "U3"
RouteId route_id_04 = RouteId "U4"
RouteId route_id_05 = RouteId "U6"

--Route
--TODO
Route route_01 = Route route_id_01 [c_01, c_02, c_03]
Route route_02 = Route route_id_02 [c_01, c_02, c_03]
Route route_03 = Route route_id_03 [c_01, c_02, c_03]
Route route_04 = Route route_id_04 [c_01, c_02, c_03]
Route route_05 = Route route_id_05 [c_01, c_02, c_03]

--TrainCars
TrainCar car_01 = TrainCar car_id_01 num_seats_01
TrainCar car_02 = TrainCar car_id_02 num_seats_01
TrainCar car_03 = TrainCar car_id_03 num_seats_02
TrainCar car_04 = TrainCar car_id_04 num_seats_02
TrainCar car_05 = TrainCar car_id_05 num_seats_03

TrainCar car_06 = TrainCar car_id_06 num_seats_03
TrainCar car_07 = TrainCar car_id_07 num_seats_04
TrainCar car_08 = TrainCar car_id_08 num_seats_04
TrainCar car_09 = TrainCar car_id_09 num_seats_01
TrainCar car_10 = TrainCar car_id_10 num_seats_01

TrainCar car_11 = TrainCar car_id_11 num_seats_02
TrainCar car_12 = TrainCar car_id_12 num_seats_02
TrainCar car_13 = TrainCar car_id_13 num_seats_03
TrainCar car_14 = TrainCar car_id_14 num_seats_03
TrainCar car_15 = TrainCar car_id_15 num_seats_04

TrainCar car_16 = TrainCar car_id_16 num_seats_04
TrainCar car_17 = TrainCar car_id_17 num_seats_01
TrainCar car_18 = TrainCar car_id_18 num_seats_01
TrainCar car_19 = TrainCar car_id_19 num_seats_02
TrainCar car_20 = TrainCar car_id_20 num_seats_02

--Train
Train t01 = Train u1_01 route_01 [car_01, car_02] quota_1
Train t02 = Train u1_02 route_01 [car_03, car_04] quota_1

Train t03 = Train u2_01 route_02 [car_05, car_06] quota_2
Train t04 = Train u2_02 route_02 [car_07, car_08] quota_2

Train t05 = Train u3_01 route_03 [car_09, car_10] quota_1
Train t06 = Train u3_02 route_03 [car_11, car_12] quota_4

Train t07 = Train u4_01 route_04 [car_13, car_14] quota_1
Train t08 = Train u4_02 route_04 [car_15, car_16] quota_2

Train t09 = Train u6_01 route_05 [car_17, car_18] quota_2
Train t10 = Train u6_02 route_05 [car_19, car_20] quota_3

-- The actual database
db = Db {
	trains = [t01, t02, t03, t04, t05, t06, t07, t08, t09, t10],
	routes = [route_01, route_02, route_03, route_04, route_05],

	reservations       = [],
	reservation_id_gen = 0
}
