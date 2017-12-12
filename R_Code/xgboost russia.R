# excel files that were read in were changed in excel.
#-all numbers were changed to general to remove commas
#-timestamp was changed to mm/yy format for easier merging of macro and train/test data

install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("xgboost", repos="http://dmlc.ml/drat/", type="source")

require(xgboost)
require(data.table)
require(randomForest)

setwd("C:/Users/Amber/Desktop/kaggle")

#1-30473
train<-read.csv("train_without_noise_date.csv")
#30474-38135
test<-read.csv("russia_test_date.csv")
test<-test[,-292]
macro<-read.csv("macro_date.csv")
macro<-macro[complete.cases(macro),]
train<-merge(train, macro, by="timestamp", all.x=TRUE)
test<-merge(test, macro, by="timestamp", all.x=TRUE)
table(is.na(train))
# return variables with % missing
sapply(train, function(x) sum(is.na(x))/length(x))*100

# remove above 40% missing
train$hospital_beds_raion<-NULL
train$build_year<-NULL
train$state<-NULL
train$cafe_sum_5000_max_price_avg<-NULL
train$cafe_avg_price_500<-NULL
test$hospital_beds_raion<-NULL
test$build_year<-NULL
test$state<-NULL
test$cafe_sum_5000_max_price_avg<-NULL
test$cafe_avg_price_500<-NULL
train$sub_area<-NULL
test$sub_area<-NULL
train$cafe_sum_5000_min_price_avg<-NULL
test$cafe_sum_5000_min_price_avg<-NULL




levels(train$ecology) <- c("excellent"=4, "good"=3, "satisfactory"=2, "poor"=1, "no data"=0)
train$ecology<-as.numeric(train$ecology)

levels(test$ecology) <- c("excellent"=4, "good"=3, "satisfactory"=2, "poor"=1, "no data"=0)
test$ecology<-as.numeric(test$ecology)

levels(train$railroad_1line)<-c("no"=0, "yes"=1)
train$railroad_1line<-as.numeric(train$railroad_1line)

levels(test$railroad_1line)<-c("no"=0, "yes"=1)
test$railroad_1line<-as.numeric(test$railroad_1line)

levels(train$big_road1_1line)<-c("no"=0, "yes"=1)
train$big_road1_1line<-as.numeric(train$big_road1_1line)

levels(test$big_road1_1line)<-c("no"=0, "yes"=1)
test$big_road1_1line<-as.numeric(test$big_road1_1line)

levels(train$water_1line)<-c("no"=0, "yes"=1)
train$water_1line<-as.numeric(train$water_1line)

levels(test$water_1line)<-c("no"=0, "yes"=1)
test$water_1line<-as.numeric(test$water_1line)

levels(train$detention_facility_raion)<-c("no"=0, "yes"=1)
train$detention_facility_raion<-as.numeric(train$detention_facility_raion)

levels(test$detention_facility_raion)<-c("no"=0, "yes"=1)
test$detention_facility_raion<-as.numeric(test$detention_facility_raion)

levels(train$big_market_raion)<-c("no"=0, "yes"=1)
train$big_market_raion<-as.numeric(train$big_market_raion)

levels(test$big_market_raion)<-c("no"=0, "yes"=1)
test$big_market_raion<-as.numeric(test$big_market_raion)

levels(train$nuclear_reactor_raion)<-c("no"=0, "yes"=1)
train$nuclear_reactor_raion<-as.numeric(train$nuclear_reactor_raion)

levels(test$nuclear_reactor_raion)<-c("no"=0, "yes"=1)
test$nuclear_reactor_raion<-as.numeric(test$nuclear_reactor_raion)

levels(train$railroad_termil_raion)<-c("no"=0, "yes"=1)
train$railroad_termil_raion<-as.numeric(train$railroad_termil_raion)

levels(test$railroad_termil_raion)<-c("no"=0, "yes"=1)
test$railroad_termil_raion<-as.numeric(test$railroad_termil_raion)

levels(train$radiation_raion)<-c("no"=0, "yes"=1)
train$radiation_raion<-as.numeric(train$radiation_raion)

levels(test$radiation_raion)<-c("no"=0, "yes"=1)
test$radiation_raion<-as.numeric(test$radiation_raion)

levels(train$oil_chemistry_raion)<-c("no"=0, "yes"=1)
train$oil_chemistry_raion<-as.numeric(train$oil_chemistry_raion)

levels(test$oil_chemistry_raion)<-c("no"=0, "yes"=1)
test$oil_chemistry_raion<-as.numeric(test$oil_chemistry_raion)

levels(train$incineration_raion)<-c("no"=0, "yes"=1)
train$incineration_raion<-as.numeric(train$incineration_raion)

levels(test$incineration_raion)<-c("no"=0, "yes"=1)
test$incineration_raion<-as.numeric(test$incineration_raion)

levels(train$thermal_power_plant_raion)<-c("no"=0, "yes"=1)
train$thermal_power_plant_raion<-as.numeric(train$thermal_power_plant_raion)

levels(test$thermal_power_plant_raion)<-c("no"=0, "yes"=1)
test$thermal_power_plant_raion<-as.numeric(test$thermal_power_plant_raion)

levels(train$culture_objects_top_25)<-c("no"=0, "yes"=1)
train$culture_objects_top_25<-as.numeric(train$culture_objects_top_25)

levels(test$culture_objects_top_25)<-c("no"=0, "yes"=1)
test$culture_objects_top_25<-as.numeric(test$culture_objects_top_25)

levels(train$product_type)<-c("Investment"=0, "OwnerOccupier"=1)
train$product_type<-as.numeric(train$product_type)

levels(test$product_type)<-c("Investment"=0, "OwnerOccupier"=1)
test$product_type<-as.numeric(test$product_type)

train$old_education_build_share<-as.numeric(train$old_education_build_share)
test$old_education_build_share<-as.numeric(test$old_education_build_share)

train$modern_education_build_share<-as.numeric(train$old_education_build_share)
test$modern_education_build_share<-as.numeric(test$old_education_build_share)

train$child_on_acc_pre_school<-as.numeric(train$child_on_acc_pre_school)
test$child_on_acc_pre_school<-as.numeric(test$child_on_acc_pre_school)

is.fact <- sapply(train, is.factor)
factors.df <- train[, is.fact]
lapply(factors.df, levels)

is.fact <- sapply(test, is.factor)
factors.df <- test[, is.fact]
lapply(factors.df, levels)

table(is.na(test))
sapply(test, function(x) sum(is.na(x))/length(x))*100

train<-na.roughfix(train, fill=TRUE)
test<-na.roughfix(test, fill=TRUE)

id=test[,2]
test=test[,-2]

train=train[,-2]

target=train$price_doc


train=train[,-285]

trainMat=data.matrix(train)
testMat=data.matrix(test)

bst.cv=xgb.cv(data=trainMat,label=target,nfold=5,nrounds=5)

bst=xgboost(param=list(eta=0.05),data=trainMat,label=target,nrounds=2000)

ypred=predict(bst,testMat)

predictions <- data.frame(id=id, price_doc=ypred)

write.csv(predictions, "xgboost2000_time_final.csv")

# xgboost with 2000 rounds: rsme 38254
# xgboost with 2000 rounds and timestamp: 
