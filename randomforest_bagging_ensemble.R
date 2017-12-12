# Column price_doc was added to test data set in excel to match columns for rbind
# NA were replaced with blank spaces in excel for both the training and test data sets
# format of time stamp was changed for easier manipulation to MM/DD/YY format

install.packages("Hmisc")
install.packages("Metrics")
install.packages("randomForest")
install.packages("dplyr")
library(Hmisc)
library(randomForest)
library(Metrics)
library(dplyr)

#Set Working Directory
setwd("C:/Users/Amber/Desktop/kaggle")

#1-30473
train<-read.csv("train_without_noise_date.csv")
#30474-38135
test<-read.csv("russia_test_date.csv")

c <- rbind(train, test)

macro<-read.csv("macro_date.csv")
macro<-macro[complete.cases(macro),]

c <- merge(c, macro, by=("timestamp"),all.x = TRUE)


summary(c)

# remove variables of no use
c$sub_area<-NULL
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
c$overdue_wages_per_cap<-NULL
#c$additiol_education_km<-NULL
#c$additiol_education_raion<-NULL
#c$area_m<-NULL
#c$basketball_km<-NULL
#c$big_church_count_1000<-NULL
#c$big_church_count_1500<-NULL
#c$big_church_count_500<-NULL
#c$big_church_count_2000<-NULL
#c$big_church_count_5000<-NULL
#c$big_church_km<-NULL
#c$big_market_km<-NULL
#c$big_market_raion<-NULL
#c$big_road1_1line<-NULL
#c$big_road1_km<-NULL
#c$big_road2_km<-NULL
#c$build_count_1921.1945<-NULL
#c$build_count_1946.1970<-NULL
#c$build_count_1946.1970<-NULL
#c$build_count_1971.1995<-NULL
c$build_count_after_1995<-with(data=c, impute(build_count_after_1995, mean))
c$build_count_before_1920<-with(data=c, impute(build_count_before_1920, mean))
c$build_count_block<-NULL
c$build_count_brick<-NULL
c$build_count_foam<-NULL
c$build_count_frame<-NULL
c$build_count_mix<-NULL
c$build_count_monolith<-NULL
c$build_count_panel<-NULL
c$build_count_slag<-NULL
c$build_count_wood<-NULL
#c$bulvar_ring_km<-NULL
#c$bus_termil_avto_km<-NULL
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
#c$cafe_count_1500<-NULL
c$cafe_count_1500__price<-NULL
c$cafe_count_1500__price_1000<-NULL
c$cafe_count_1500__price_1500<-NULL
c$cafe_count_1500__price_2500<-NULL
c$cafe_count_1500__price_4000<-NULL
c$cafe_count_1500__price_500<-NULL
c$cafe_count_1500__price_high<-NULL
#c$cafe_count_2000<-NULL
c$cafe_count_2000__price<-NULL
c$cafe_count_2000__price_1000<-NULL
c$cafe_count_2000__price_1500<-NULL
c$cafe_count_2000__price_2500<-NULL
c$cafe_count_2000__price_4000<-NULL
c$cafe_count_2000__price_500<-NULL
c$cafe_count_2000__price_high<-NULL
#c$cafe_count_3000<-NULL
c$cafe_count_3000__price<-NULL
c$cafe_count_3000__price_1000<-NULL
c$cafe_count_3000__price_1500<-NULL
c$cafe_count_3000__price_2500<-NULL
c$cafe_count_3000__price_4000<-NULL
c$cafe_count_3000__price_500<-NULL
c$cafe_count_3000__price_high<-NULL
#c$cafe_count_500<-NULL
c$cafe_count_500_price<-NULL
c$cafe_count_500_price_1000<-NULL
c$cafe_count_500_price_1500<-NULL
c$cafe_count_500_price_2500<-NULL
c$cafe_count_500_price_4000<-NULL
c$cafe_count_500_price_500<-NULL
c$cafe_count_500_price_high<-NULL
#c$cafe_count_5000<-NULL
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
#c$catering_km<-NULL
#c$cemetery_km<-NULL
#c$children_preschool<-NULL
#c$children_school<-NULL
#c$church_count_1000<-NULL
#c$church_count_1500<-NULL
#c$church_count_3000<-NULL
#c$church_count_2000<-NULL
#c$church_count_500<-NULL
#c$church_count_5000<-NULL
#c$church_sygogue_km<-NULL
#c$culture_objects_top_25<-NULL
#c$detention_facility_raion<-NULL
#c$ekder_all<-NULL
#c$ekder_female<-NULL
#c$ekder_male<-NULL
#c$exhibition_km<-NULL
c$female_f<-NULL
#c$fitness_km<-NULL
c$full_all<-NULL
#c$green_part_1000<-NULL
#c$green_part_1500<-NULL
#c$green_part_2000<-NULL
#c$green_part_3000<-NULL
#c$green_part_5000<-NULL
#c$green_part_500<-NULL
c$green_zone_km<-NULL
c$green_zone_part<-NULL
c$healthcare_centers_raion<-NULL
c$hospice_morgue_km<-NULL
#c$ice_rink_km<-NULL
#c$incineration_km<-NULL
c$incineration_raion<-NULL
#c$industrial_km<-NULL
c$timestamp<-NULL
c$hospital_bed_occupancy_per_year<-NULL
c$hospital_beds_available_per_cap<-NULL
c$provision_retail_space_sqm<-NULL
c$turnover_catering_per_cap<-NULL
c$provision_retail_space_modern_sqm<-NULL
c$housing_fund_sqm<-NULL


#Find factor variables and levels
is.fact <- sapply(c, is.factor)
factors.df <- c[, is.fact]
lapply(factors.df, levels)

c$child_on_acc_pre_school<-as.numeric(c$child_on_acc_pre_school)
c$modern_education_share<-as.numeric(c$modern_education_share)
c$old_education_build_share<-as.numeric(c$old_education_build_share)
# find missing values in each variable

apply(is.na(c),2,sum)


# impute
c$hospital_beds_raion<-with(data=c, impute(hospital_beds_raion, mean))
c$build_year<-with(data=c, impute(build_year, mean))
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
c$preschool_education_centers_raion<-with(c, impute(preschool_education_centers_raion, mean))
c$state<-with(c,impute(c$state, mode))
summary(c)


# transformations
#c$public_healthcare_km<-log(c$public_healthcare_km+1)
#c$full_sq<-log(c$full_sq+1)
#c$leisure_count_1000<-log(c$leisure_count_1000+1)
#c$cafe_count_1000__price<-log(c$cafe_count_1000__price+1)
#c$office_count_500<-log(c$office_count_500+1)
#c$cafe_count_1000_price_1000<-log(c$cafe_count_1000_price_1000+1)
#c$big_church_count_3000<-log(c$big_church_count_3000+1)
#c$power_transmission_line_km<-log(c$power_transmission_line_km+1)
#c$lodging_sqm_per_cap<-log(c$lodging_sqm_per_cap+1)
#c$provision_doctors<-log(c$provision_doctors+1)
#c$construction_value<-log(c$construction_value+1)
#c$average_life_exp<-log(c$average_life_exp+1)
#c$unprofitable_enterpr_share<-log(c$unprofitable_enterpr_share+1)
#c$mosque_count_500<-(c$mosque_count_500+1)*exp(2)
#c$public_transport_station_min_walk<-log(c$public_transport_station_min_walk+1)
#c$lodging_sqm_per_cap<-log(c$lodging_sqm_per_cap+1)

c<-na.roughfix(c)

write.csv(c, "combined.csv")

# seperate into training and test sets
#1-30473
a<-c[(c$id<=30473),]
b<-c[(c$id>30473),]

# remove ID for model building
a<-(a[,(-1)])

a<-na.roughfix(a)

# outlier removal
library(outliers)
a$raion_popul<-rm.outlier(a$raion_popul, fill=TRUE, median=TRUE)
a$school_education_centers_raion<-rm.outlier(a$school_education_centers_raion, fill=TRUE, median=TRUE)
a$school_education_centers_top_20_raion<-rm.outlier(a$school_education_centers_top_20_raion, fill=TRUE, median=TRUE)
a$university_top_20_raion<-rm.outlier(a$university_top_20_raion, fill=TRUE, median=TRUE)
a$sport_objects_raion<-rm.outlier(a$sport_objects_raion, fill=TRUE, median=TRUE)
a$culture_objects_top_25_raion<-rm.outlier(a$culture_objects_top_25_raion, fill=TRUE, median=TRUE)
a$shopping_centers_raion<-rm.outlier(a$shopping_centers_raion, fill=TRUE, median=TRUE)
a$office_raion<-rm.outlier(a$office_raion, fill=TRUE, median=TRUE)
a$male_f<-rm.outlier(a$male_f, fill=TRUE, median=TRUE)
a$young_all<-rm.outlier(a$young_all, fill=TRUE, median=TRUE)
a$young_male<-rm.outlier(a$young_male, fill=TRUE, median=TRUE)
a$young_female<-rm.outlier(a$young_female, fill=TRUE, median=TRUE)
a$work_all<-rm.outlier(a$work_all, fill=TRUE, median=TRUE)
a$work_male<-rm.outlier(a$work_male, fill=TRUE, median=TRUE)
a$work_female<-rm.outlier(a$work_female, fill=TRUE, median=TRUE)
a$X7_14_all<-rm.outlier(a$X7_14_all, fill=TRUE, median=TRUE)
a$X7_14_male<-rm.outlier(a$X7_14_male, fill=TRUE, median=TRUE)
a$X7_14_female<-rm.outlier(a$X7_14_female, fill=TRUE, median=TRUE)
a$X16_29_all<-rm.outlier(a$X16_29_all, fill=TRUE, median=TRUE)
a$X16_29_male<-rm.outlier(a$X16_29_male, fill=TRUE, median=TRUE)
a$X16_29_female<-rm.outlier(a$X16_29_female, fill=TRUE, median=TRUE)
a$office_sqm_500<-rm.outlier(a$office_sqm_500, fill=TRUE, median=TRUE)
a$trc_count_500<-rm.outlier(a$trc_count_500, fill=TRUE, median=TRUE)
a$trc_sqm_500<-rm.outlier(a$trc_sqm_500, fill=TRUE, median=TRUE)
a$mosque_count_500<-rm.outlier(a$mosque_count_500, fill=TRUE, median=TRUE)
a$leisure_count_500<-rm.outlier(a$leisure_count_500, fill=TRUE, median=TRUE)
a$sport_count_500<-rm.outlier(a$trc_count_500, fill=TRUE, median=TRUE)
a$market_count_500<-rm.outlier(a$trc_count_500, fill=TRUE, median=TRUE)
a$office_count_1000<-rm.outlier(a$office_count_1000, fill=TRUE, median=TRUE)
a$office_sqm_1000<-rm.outlier(a$office_sqm_1000, fill=TRUE, median=TRUE)
a$trc_count_1000<-rm.outlier(a$trc_count_1000, fill=TRUE, median=TRUE)
a$trc_sqm_1000<-rm.outlier(a$trc_sqm_1000, fill=TRUE, median=TRUE)
a$mosque_count_1000<-rm.outlier(a$mosque_count_1000, fill=TRUE, median=TRUE)
a$sport_count_1000<-rm.outlier(a$sport_count_1000, fill=TRUE, median=TRUE)
a$market_count_1000<-rm.outlier(a$market_count_1000, fill=TRUE, median=TRUE)
a$office_count_1500<-rm.outlier(a$office_count_1500, fill=TRUE, median=TRUE)
a$office_sqm_1500<-rm.outlier(a$office_sqm_1500, fill=TRUE, median=TRUE)
a$trc_count_1500<-rm.outlier(a$trc_count_1500, fill=TRUE, median=TRUE)
a$mosque_count_1500<-rm.outlier(a$mosque_count_1500, fill=TRUE, median=TRUE)
a$leisure_count_1500<-rm.outlier(a$leisure_count_1500, fill=TRUE, median=TRUE)
a$sport_count_1500<-rm.outlier(a$sport_count_1500, fill=TRUE, median=TRUE)
a$market_count_1500<-rm.outlier(a$market_count_1500, fill=TRUE, median=TRUE)
a$office_count_2000<-rm.outlier(a$office_count_2000, fill=TRUE, median=TRUE)
a$office_sqm_2000<-rm.outlier(a$office_sqm_2000, fill=TRUE, median=TRUE)
a$trc_count_2000<-rm.outlier(a$trc_count_2000, fill=TRUE, median=TRUE)
a$trc_sqm_2000<-rm.outlier(a$trc_sqm_2000, fill=TRUE, median=TRUE)
a$mosque_count_2000<-rm.outlier(a$mosque_count_2000, fill=TRUE, median=TRUE)
a$leisure_count_2000<-rm.outlier(a$leisure_count_2000, fill=TRUE, median=TRUE)
a$sport_count_2000<-rm.outlier(a$sport_count_2000, fill=TRUE, median=TRUE)
a$market_count_2000<-rm.outlier(a$market_count_2000, fill=TRUE, median=TRUE)
a$office_count_3000<-rm.outlier(a$office_count_3000, fill=TRUE, median=TRUE)
a$office_sqm_3000<-rm.outlier(a$office_sqm_3000, fill=TRUE, median=TRUE)
a$trc_count_3000<-rm.outlier(a$trc_count_3000, fill=TRUE, median=TRUE)
a$trc_sqm_3000<-rm.outlier(a$trc_sqm_3000, fill=TRUE, median=TRUE)
a$mosque_count_3000<-rm.outlier(a$mosque_count_3000, fill=TRUE, median=TRUE)
a$leisure_count_3000<-rm.outlier(a$leisure_count_3000, fill=TRUE, median=TRUE)
a$sport_count_3000<-rm.outlier(a$sport_count_3000, fill=TRUE, median=TRUE)
a$market_count_3000<-rm.outlier(a$market_count_3000, fill=TRUE, median=TRUE)
a$office_count_5000<-rm.outlier(a$office_count_5000, fill=TRUE, median=TRUE)
a$office_sqm_5000<-rm.outlier(a$office_sqm_5000, fill=TRUE, median=TRUE)
a$trc_count_5000<-rm.outlier(a$trc_count_5000, fill=TRUE, median=TRUE)
a$trc_sqm_5000<-rm.outlier(a$trc_sqm_5000, fill=TRUE, median=TRUE)
a$mosque_count_5000<-rm.outlier(a$mosque_count_5000, fill=TRUE, median=TRUE)
a$leisure_count_5000<-rm.outlier(a$leisure_count_5000, fill=TRUE, median=TRUE)
a$sport_count_5000<-rm.outlier(a$sport_count_5000, fill=TRUE, median=TRUE)
a$market_count_5000<-rm.outlier(a$market_count_5000, fill=TRUE, median=TRUE)
a$deposits_value<-rm.outlier(a$deposits_value, fill=TRUE, median=TRUE)
a$mortgage_value<-rm.outlier(a$mortgage_value, fill=TRUE, median=TRUE)
a$overdue_wages_per_cap<-rm.outlier(a$overdue_wages_per_cap, fill=TRUE, median=TRUE)
a$housing_fund_sqm<-rm.outlier(a$housing_fund_sqm, fill=TRUE, median=TRUE)
a$hospital_beds_available_per_cap<-rm.outlier(a$hospital_beds_available_per_cap, fill=TRUE, median=TRUE)
a$hospital_bed_occupancy_per_year<-rm.outlier(a$hospital_bed_occupancy_per_year, fill=TRUE, median=TRUE)
a$provision_retail_space_sqm<-rm.outlier(a$provision_retail_space_sqm, fill=TRUE, median=TRUE)
a$provision_retail_space_modern_sqm<-rm.outlier(a$provision_retail_space_modern_sqm, fill=TRUE, median=TRUE)
a$turnover_catering_per_cap<-rm.outlier(a$turnover_catering_per_cap, fill=TRUE, median=TRUE)
#a$theaters_viewers_per_1000_cap<-rm.outlier(a$theaters_viewers_per_1000_cap, fill=TRUE, median=TRUE)
#a$museum_visitis_per_100_cap<-rm.outlier(a$museum_visitis_per_100_cap, fill=TRUE, median=TRUE)
#a$bandwidth_sports<-rm.outlier(a$bandwidth_sports, fill=TRUE, median=TRUE)
#a$apartment_build<-rm.outlier(a$apartment_build, fill=TRUE, median=TRUE)
a$full_sq<-rm.outlier(a$full_sq, fill=TRUE, median=TRUE)
a$life_sq<-rm.outlier(a$life_sq, fill=TRUE, median=TRUE)
a$floor<-rm.outlier(a$floor, fill=TRUE, median=TRUE)
a$max_floor<-rm.outlier(a$max_floor, fill=TRUE, median=TRUE)
a$material<-rm.outlier(a$material, fill=TRUE, median=TRUE)
a$build_year<-rm.outlier(a$build_year, fill=TRUE, median=TRUE)
a$kitch_sq<-rm.outlier(a$kitch_sq, fill=TRUE, median=TRUE)
a$raion_popul<-rm.outlier(a$raion_popul, fill=TRUE, median=TRUE)
a$indust_part<-rm.outlier(a$indust_part, fill=TRUE, median=TRUE)
a$preschool_quota<-rm.outlier(a$preschool_quota, fill=TRUE, median=TRUE)
a$preschool_education_centers_raion<-rm.outlier(a$preschool_education_centers_raion, fill=TRUE, median=TRUE)
a$school_quota<-rm.outlier(a$school_quota, fill=TRUE, median=TRUE)
a$school_education_centers_raion<-rm.outlier(a$school_education_centers_raion, fill=TRUE, median=TRUE)
a$school_education_centers_top_20_raion<-rm.outlier(a$school_education_centers_top_20_raion, fill=TRUE, median=TRUE)
a$university_top_20_raion<-rm.outlier(a$university_top_20_raion, fill=TRUE, median=TRUE)
a$sport_objects_raion<-rm.outlier(a$sport_objects_raion, fill=TRUE, median=TRUE)
a$culture_objects_top_25_raion<-rm.outlier(a$culture_objects_top_25_raion, fill=TRUE, median=TRUE)
a$shopping_centers_raion<-rm.outlier(a$shopping_centers_raion, fill=TRUE, median=TRUE)
a$office_raion<-rm.outlier(a$office_raion, fill=TRUE, median=TRUE)
a$male_f<-rm.outlier(a$male_f, fill=TRUE, median=TRUE)
a$young_male<-rm.outlier(a$young_male, fill=TRUE, median=TRUE)
a$young_all<-rm.outlier(a$young_all, fill=TRUE, median=TRUE)
a$raion_build_count_with_material_info<-rm.outlier(a$raion_build_count_with_material_info, fill=TRUE, median=TRUE)
a$raion_build_count_with_builddate_info<-rm.outlier(a$raion_build_count_with_builddate_info, fill=TRUE, median=TRUE)
a$metro_min_avto<-rm.outlier(a$metro_min_avto, fill=TRUE, median=TRUE)
a$metro_km_avto<-rm.outlier(a$metro_km_avto, fill=TRUE, median=TRUE)
a$metro_min_walk<-rm.outlier(a$metro_min_walk, fill=TRUE, median=TRUE)
a$metro_km_walk<-rm.outlier(a$metro_km_walk, fill=TRUE, median=TRUE)
a$kindergarten_km<-rm.outlier(a$kindergarten_km, fill=TRUE, median=TRUE)
a$school_km<-rm.outlier(a$school_km, fill=TRUE, median=TRUE)
a$park_km<-rm.outlier(a$park_km, fill=TRUE, median=TRUE)
a$water_treatment_km<-rm.outlier(a$water_treatment_km, fill=TRUE, median=TRUE)
a$railroad_station_walk_km<-rm.outlier(a$railroad_station_walk_km, fill=TRUE, median=TRUE)
a$railroad_station_walk_min<-rm.outlier(a$railroad_station_walk_min, fill=TRUE, median=TRUE)
a$railroad_station_avto_km<-rm.outlier(a$railroad_station_avto_km, fill=TRUE, median=TRUE)
a$railroad_station_avto_min<-rm.outlier(a$railroad_station_avto_min, fill=TRUE, median=TRUE)
a$public_transport_station_km<-rm.outlier(a$public_transport_station_km, fill=TRUE, median=TRUE)
a$public_transport_station_min_walk<-rm.outlier(a$public_transport_station_min_walk, fill=TRUE, median=TRUE)
a$water_km<-rm.outlier(a$water_km, fill=TRUE, median=TRUE)
a$mkad_km<-rm.outlier(a$mkad_km, fill=TRUE, median=TRUE)
a$ttk_km<-rm.outlier(a$ttk_km, fill=TRUE, median=TRUE)
a$sadovoe_km<-rm.outlier(a$sadovoe_km, fill=TRUE, median=TRUE)
a$kremlin_km<-rm.outlier(a$kremlin_km, fill=TRUE, median=TRUE)
a$railroad_km<-rm.outlier(a$railroad_km, fill=TRUE, median=TRUE)
a$zd_vokzaly_avto_km<-rm.outlier(a$zd_vokzaly_avto_km, fill=TRUE, median=TRUE)
a$oil_chemistry_km<-rm.outlier(a$oil_chemistry_km, fill=TRUE, median=TRUE)
a$nuclear_reactor_km<-rm.outlier(a$nuclear_reactor_km, fill=TRUE, median=TRUE)

# remove price_doc column for test data
b<-b[,(-225)]

apply(is.na(a),2,sum)

# random forest with 800
set.seed(1235)
model_no_outliers<-randomForest(price_doc~.,data=a, ntree=25, corr.bias=TRUE)
model_no_outliers_grown<-grow(model_no_outliers, 475)

set.seed(1234)
model <- randomForest(price_doc~.,data=a,ntree=25, corr.bias=TRUE)
model2<-grow(model, 1075)
model3<-grow(model, 1000)
#model <- randomForest(price_doc~full_sq+zd_vokzaly_avto_km+public_healthcare_km+num_room+big_church_count_3000+leisure_count_1000+cafe_count_1000__price+prom_part_5000+sport_count_3000+power_transmission_line_km+mosque_count_3000+nuclear_reactor_km+mosque_count_3000+detention_facility_km+trc_count_5000+floor+sport_count_1500+indust_part+ecology,data=a,ntree=25)


# calculate mean squared log error
error<-sqrt(msle(a$price_doc, (predict(model2, a))))
error

# find multicollinearity
vif(lm(price_doc~full_sq+zd_vokzaly_avto_km+public_healthcare_km+num_room+big_church_count_3000+leisure_count_1000+cafe_count_1000__price+prom_part_5000+sport_count_3000+power_transmission_line_km+mosque_count_3000+nuclear_reactor_km+mosque_count_3000+detention_facility_km+trc_count_5000+floor+sport_count_1500+indust_part+ecology,data=a,ntree=800))

#random forest with 1500
model_1500 <- randomForest(price_doc~full_sq+zd_vokzaly_avto_km+public_healthcare_km+num_room+big_church_count_3000+leisure_count_1000+cafe_count_1000__price+prom_part_5000+sport_count_3000+power_transmission_line_km+mosque_count_3000+nuclear_reactor_km+mosque_count_3000+detention_facility_km+trc_count_5000+floor+sport_count_1500+indust_part+ecology,data=a,ntree=1500)

# calcualte mean squared log error with 1500
error_1500<-msle(a$price_doc, (predict(model2, a)))
error

# random forest with 800 and mtry 100 and corr.bias
model2 <- randomForest(price_doc~full_sq+zd_vokzaly_avto_km+public_healthcare_km+num_room+big_church_count_3000+leisure_count_1000+cafe_count_1000__price+prom_part_5000+sport_count_3000+power_transmission_line_km+mosque_count_3000+nuclear_reactor_km+mosque_count_3000+detention_facility_km+trc_count_5000+floor+sport_count_1500+indust_part+ecology,data=a,ntree=1000, mtry=10, corr.bias=TRUE)

model3<-randomForest(price_doc~full_sq+zd_vokzaly_avto_km+public_healthcare_km+num_room+big_church_count_3000+leisure_count_1000+cafe_count_1000__price+prom_part_5000+sport_count_3000+power_transmission_line_km+mosque_count_3000+nuclear_reactor_km+mosque_count_3000+detention_facility_km+trc_count_5000+floor+sport_count_1500+indust_part+ecology,data=a,ntree=50, corr.bias=TRUE)

# calcualte mean squared log error without collinearity
error<-msle(a$price_doc, (predict(model5, a)))
error_untuned<-msle(a$price_doc, (predict(model3, a)))

#compare error
table(sqrt(error_tuned), sqrt(error_untuned))

install.packages("ipred")
library(ipred)

model5<-bagging(price_doc~., data=a, nbagg=100)

#model4<-nnet(price_doc~.,data=a, size=100, MaxNWts=150000, decay = 0.0001,maxit = 500)

pred<-predict(model2, a)
pred3<-predict(model5, a)

a$rforest<-pred
a$bagg<-pred3
a$total<-a$rforest+a$bagg
a$average<-a$total/2
error<-msle(a$price_doc, a$average)
error 

#Predict for test data
pred <- predict(model2, b)
pred3<-predict(model5, b)

b$rforest<-pred
b$bagg<-pred3
b$total<-b$rforest+b$bagg
b$average<-b$total/2

predictions <- data.frame(id=b$id, price_doc=b$average)

write.csv(predictions, "bagging_rforest_3.csv")







# Ideas
#add year of timestamp or month or season
#try lasso with glmnet function 
#try linear svr 
#try ridge regression

# lasso 

lm.fit <- glm(price_doc~., data=c)

# normalize formula
maxs <- apply(c, 2, max) 
mins <- apply(c, 2, min)

c<- as.data.frame(scale(c, center = mins, scale = maxs - mins))

https://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/
