#Set Working Directory
setwd("C:/Users/Amber/Desktop/kaggle")

install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("xgboost", repos="http://dmlc.ml/drat/", type="source")

require(xgboost)

#1-30473
train<-read.csv("train_without_noise_date.csv")
#30474-38135
test<-read.csv("russia_test_date.csv")

target <- train$price_doc
sparsed_train_set <- sparse.model.matrix(~.-1, data=train) 


param<-list(max_depth=2, eta=1, silent=1, ntrhead=2)

bst<-xgb.train(param, train, nrounds=2, watchlist)















c <- rbind(train, test)

macro<-read.csv("macro_date.csv")
macro<-macro[complete.cases(macro),]

c <- merge(c, macro, by=("timestamp"),all.x = TRUE)

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




# seperate into training and test sets
#1-30473
a<-c[(c$id<=30473),]
b<-c[(c$id>30473),]

# remove ID for model building
a<-(a[,(-1)])

a<-na.roughfix(a)
# remove price_doc column for test data
b<-b[,(-225)]
