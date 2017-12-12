install.packages("Hmisc")
library(Hmisc)
install.packages("randomForest")
library(randomForest)
install.packages("Matrix")
library(Matrix)
install.packages("plyr")
library(plyr)
install.packages("xgboost")
library(xgboost)
install.packages("readr")
library(readr)
install.packages("stringer")
library(stringr)
install.packages("caret")
library(caret)
install.packages("car")
library(car)
install.packages("C50")
library(C50)
install.packages("ipred")
library(ipred)
install.packages("e1071")
library(e1071)
#Set Working Directory
setwd("C:/Users/Amber/Desktop/kaggle")

#1-30473
train<-read.csv("russia_train.csv")
#30474-38135
test<-read.csv("russia_test.csv")

c <- rbind(train, test)

summary(c)

# remove variables of no use
c$sub_area<-NULL
c$timestamp<-NULL
c$ID_railroad_station_avto<-NULL
c$X0_13_all<-NULL
c$X0_13_female<-NULL
c$X0_13_male<-NULL
c$X0_17_all<-NULL
c$X0_17_female<-NULL
c$X0_17_male<-NULL
c$X0_6_female<-NULL
c$X0_6_male<-NULL
c$X0_6_all<-NULL
c$X0_16_29_female<-NULL
c$X0_16_29_all<-NULL
c$X0_16_29_male<-NULL
c$X0_7_14_female<-NULL
c$X0_7_14_male<-NULL
c$X0_7_14_all<-NULL
c$additiol_education_km<-NULL
c$additiol_education_raion<-NULL
c$area_m<-NULL
c$basketball_km<-NULL
c$big_church_count_1000<-NULL
c$big_church_count_1500<-NULL
c$big_church_count_500<-NULL
c$big_church_count_2000<-NULL
c$big_church_count_5000<-NULL
c$big_church_km<-NULL
c$big_market_km<-NULL
c$big_market_raion<-NULL
c$big_road1_1line<-NULL
c$big_road1_km<-NULL
c$big_road2_km<-NULL
c$build_count_1921.1945<-NULL
c$build_count_1946.1970<-NULL
c$build_count_1946.1970<-NULL
c$build_count_1971.1995<-NULL
c$build_count_after_1995<-NULL
c$build_count_before_1920<-NULL
c$build_count_block<-NULL
c$build_count_brick<-NULL
c$build_count_foam<-NULL
c$build_count_frame<-NULL
c$build_count_mix<-NULL
c$build_count_monolith<-NULL
c$build_count_panel<-NULL
c$build_count_slag<-NULL
c$build_count_wood<-NULL

c$bulvar_ring_km<-NULL
c$bus_termil_avto_km<-NULL
c$cafe_avg_price_1000<-NULL
c$cafe_avg_price_1500<-NULL
c$cafe_avg_price_2000<-NULL
c$cafe_avg_price_3000<-NULL
c$cafe_avg_price_500<-NULL
c$cafe_avg_price_5000<-NULL
c$cafe_count_1000<-NULL
c$cafe_count_1000_price_1500<-NULL
c$cafe_count_1000_price_2500<-NULL
c$cafe_count_1000_price_4000<-NULL
c$cafe_count_1000_price_500<-NULL
c$cafe_count_1000_price_high<-NULL
c$cafe_count_1500<-NULL
c$cafe_count_1500__price<-NULL
c$cafe_count_1500__price_1000<-NULL
c$cafe_count_1500__price_1500<-NULL
c$cafe_count_1500__price_2500<-NULL
c$cafe_count_1500__price_4000<-NULL
c$cafe_count_1500__price_500<-NULL
c$cafe_count_1500__price_high<-NULL
c$cafe_count_2000<-NULL
c$cafe_count_2000__price<-NULL
c$cafe_count_2000__price_1000<-NULL
c$cafe_count_2000__price_1500<-NULL
c$cafe_count_2000__price_2500<-NULL
c$cafe_count_2000__price_4000<-NULL
c$cafe_count_2000__price_500<-NULL
c$cafe_count_2000__price_high<-NULL
c$cafe_count_3000<-NULL
c$cafe_count_3000__price<-NULL
c$cafe_count_3000__price_1000<-NULL
c$cafe_count_3000__price_1500<-NULL
c$cafe_count_3000__price_2500<-NULL
c$cafe_count_3000__price_4000<-NULL
c$cafe_count_3000__price_500<-NULL
c$cafe_count_3000__price_high<-NULL
c$cafe_count_500<-NULL
c$cafe_count_500_price<-NULL
c$cafe_count_500_price_1000<-NULL
c$cafe_count_500_price_1500<-NULL
c$cafe_count_500_price_2500<-NULL
c$cafe_count_500_price_4000<-NULL
c$cafe_count_500_price_500<-NULL
c$cafe_count_500_price_high<-NULL
c$cafe_count_5000<-NULL
c$cafe_count_5000_price<-NULL
c$cafe_count_5000_price_1000<-NULL
c$cafe_count_5000_price_1500<-NULL
c$cafe_count_5000_price_2500<-NULL
c$cafe_count_5000_price_4000<-NULL
c$cafe_count_5000_price_500<-NULL
c$cafe_count_5000_price_high<-NULL
c$cafe_sum_1000_max_price_avg<-NULL
c$cafe_sum_1000_min_price_avg<-NULL
c$cafe_sum_1500_max_price_avg<-NULL
c$cafe_sum_1500_min_price_avg<-NULL
c$cafe_sum_2000_max_price_avg<-NULL
c$cafe_sum_2000_min_price_avg<-NULL
c$cafe_sum_3000_max_price_avg<-NULL
c$cafe_sum_3000_min_price_avg<-NULL
c$cafe_sum_5000_max_price_avg<-NULL
c$cafe_sum_5000_min_price_avg<-NULL
c$cafe_sum_500_max_price_avg<-NULL
c$cafe_sum_500_min_price_avg<-NULL
c$catering_km<-NULL
c$cemetery_km<-NULL
c$children_preschool<-NULL
c$children_school<-NULL
c$church_count_1000<-NULL
c$church_count_1500<-NULL
c$church_count_3000<-NULL
c$church_count_2000<-NULL
c$church_count_500<-NULL
c$church_count_5000<-NULL
c$church_sygogue_km<-NULL
c$culture_objects_top_25<-NULL
c$detention_facility_raion<-NULL
c$ekder_all<-NULL
c$ekder_female<-NULL
c$ekder_male<-NULL
c$exhibition_km<-NULL
c$female_f<-NULL
c$fitness_km<-NULL
c$full_all<-NULL
c$green_part_1000<-NULL
c$green_part_1500<-NULL
c$green_part_2000<-NULL
c$green_part_3000<-NULL
c$green_part_5000<-NULL
c$green_part_500<-NULL
c$green_zone_km<-NULL
c$green_zone_part<-NULL
c$healthcare_centers_raion<-NULL
c$hospice_morgue_km<-NULL
c$hospital_beds_raion<-NULL
c$ice_rink_km<-NULL
c$incineration_km<-NULL
c$incineration_raion<-NULL
c$industrial_km<-NULL

#Find factor variables and levels
is.fact <- sapply(c, is.factor)
factors.df <- c[, is.fact]
lapply(factors.df, levels)


# find missing values in each variable
apply(is.na(c),2,sum)


# impute
c$build_year<-with(c, impute(build_year, mean))
c$ID_railroad_station_walk<-with(c, impute(ID_railroad_station_walk, mean))
c$floor<-with(c, impute(floor, mean))
c$kitch_sq<-with(c, impute(kitch_sq, mean))
c$life_sq<-with(c, impute(life_sq, mean))
c$material<-with(c, impute(material, mean))
c$max_floor<-with(c, impute(max_floor, mean))
c$metro_km_walk<-with(c, impute(metro_km_walk, mean))
c$metro_min_walk<-with(c, impute(metro_min_walk, mean))
c$num_room<-with(c, impute(num_room, mean))
c$preschool_quota<-with(c, impute(preschool_quota, mean))
c$prom_part_5000<-with(c, impute(prom_part_5000, mean))
c$railroad_station_walk_km<-with(c, impute(railroad_station_walk_km, mean))
c$railroad_station_walk_min<-with(c, impute(railroad_station_walk_min, mean))
c$raion_build_count_with_builddate_info<-with(c, impute(raion_build_count_with_builddate_info, mean))
c$raion_build_count_with_material_info<-with(c, impute(raion_build_count_with_material_info, mean))
c$school_quota<-with(c, impute(school_quota, mean))
c$state<-with(c, impute(state, mean))
c$build_count_1921.1945<-with(c, impute(build_count_1921.1945, mean))
c$build_count_1946.1970<-with(c, impute(build_count_1946.1970, mean))
c$build_count_1971.1995<-with(c, impute(build_count_1971.1995, mean))
c$green_part_2000<-with(c, impute(green_part_2000, mean))
c$cafe_sum_3000_min_price_avg<-with(c, impute(cafe_sum_3000_min_price_avg, mean))
summary(c)

# transformations
c$public_healthcare_km<-log(c$public_healthcare_km+1)
c$full_sq<-log(c$full_sq+1)
c$leisure_count_1000<-log(c$leisure_count_1000+1)
c$cafe_count_1000__price<-log(c$cafe_count_1000__price+1)
c$office_count_500<-log(c$office_count_500+1)
c$cafe_count_1000_price_1000<-log(c$cafe_count_1000_price_1000+1)
c$big_church_count_3000<-log(c$big_church_count_3000+1)
c$power_transmission_line_km<-log(c$power_transmission_line_km+1)

revalue(c$ecology, c("excellent"=4, "good"=3, "satisfactory"=2, "poor"=1, "no data"=0))
c$ecology <- as.integer(c$ecology)
revalue(c$railroad_1line, c("yes"=1,"no"=0))
c$railroad_1line<-as.integer(c$railroad_1line)
revalue(c$water_1line, c("yes"=1,"no"=0))
c$water_1line<-as.integer(c$water_1line)
revalue(c$nuclear_reactor_raion, c("yes"=1,"no"=0))
c$nuclear_reactor_raion<-as.integer(c$nuclear_reactor_raion)
revalue(c$railroad_termil_raion, c("yes"=1,"no"=0))
c$railroad_termil_raion<-as.integer(c$railroad_termil_raion)
revalue(c$thermal_power_plant_raion, c("yes"=1,"no"=0))
c$thermal_power_plant_raion<-as.integer(c$thermal_power_plant_raion)
revalue(c$oil_chemistry_raion, c("yes"=1,"no"=0))
c$oil_chemistry_raion<-as.integer(c$oil_chemistry_raion)
revalue(c$radiation_raion, c("yes"=1,"no"=0))
c$radiation_raion<-as.integer(c$radiation_raion)
revalue(c$product_type,c("Investment"=1, "OwnerOccupier"=2, " "=0))
c$product_type<-as.integer(c$product_type)

# seperate into training and test sets
#1-30473
a<-c[(c$id<=30473),]
b<-c[(c$id>30473),]

# remove ID for model building
a<-(a[,(-1)])

# remove price_doc column for test data
b<-b[,(-170)]

apply(is.na(a),2,sum)

# C5.0 Boosting
set.seed(1234)
model<-train(price_doc~., data=a, method="treebag", metric="Accuracy", trControl=trainControl(method = "repeatedcv", number=10, repeats=3))

#Predict for test data
pred2 <- predict(xgb, data.matrix(b))

predictions <- data.frame(id=b$id, price_doc=pred2)

write.csv(predictions, "rforestrussia.csv")
