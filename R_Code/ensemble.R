install.packages("mlbench")
install.packages("caret")
install.packages("caretEnsemble")
install.packages("randomForest")
install.packages("Hmisc")
install.packages("plyr")
install.packages("mboost")
install.packages("ModelMetrics")
install.packages("gbm")
install.packages("xgboost")
install.packages("rpart")
install.packages("geosphere")
install.packages("rgdal")
install.packages("lubridate")
library(geosphere)
library(rgdal)
library(rpart)
library(xgboost)
library(gbm)
library(mlbench)
library(caret)
library(caretEnsemble)
library(randomForest)
library(Hmisc)
library(plyr)
library(mboost)
library(ModelMetrics)
library(readr)
library(dplyr)
library(Metrics)
library(ggplot2)
library(data.table)
library(dplyr)
library(lubridate)
library(stringr)
library(Matrix)
library(geosphere)
library(rgdal)

setwd("C:/Users/Amber/Desktop/kaggle")
#1-30473
train<-read.csv("train.csv")
#30474-38135
test<-read.csv("test.csv")
# load macro data
macro<-read.csv("macro.csv")

test$price_doc<-1

c <- rbind(train, test)

shp <- readOGR(dsn = "C:/Users/amber/Desktop/kaggle/russia map", layer = "moscow_adm")
centroids <- coordinates(shp)
sub_area <- shp$RAION
okrug <- shp$OKRUGS
location_data <- data.frame(sub_area = sub_area, okrug = okrug, longitude=centroids[,1], latitude=centroids[,2])
c<-left_join(c,location_data,by="sub_area")
kremlin = data.frame(longitude = 37.617664,latitude = 55.752121)
c<- c %>%
  group_by(sub_area) %>%
  top_n(n = 1, wt=id) %>%
  ungroup %>%
  mutate(distance_from_kremlin = distm(.[c("longitude","latitude")], kremlin, fun=distHaversine)) %>%
  select(sub_area, distance_from_kremlin) %>%
  right_join(c, by="sub_area")

# remove rows with more than 40% missing in c
c$cafe_avg_price_500<-NULL
c$cafe_sum_5000_max_price_avg<-NULL
c$cafe_sum_500_min_price_avg<-NULL
c$hospital_beds_raion<NULL

# find insignificant variables with hardly any variance in c
insignificant <- nearZeroVar(c)
print(names(c[ , insignificant]))

# remove insignificant
c<-c[,-insignificant]

# create new features from timestamp
c$date <- as.POSIXct(strptime(c$timestamp, format = "%Y-%m-%d"))
c$day<-as.factor(format(c$date, "%d")) # day
c$month <- as.factor(format(c$date, "%m")) # month
c$year <- as.integer(format(c$date, "%Y")) # year
c$timestamp <- NULL
c$date <- NULL

# create new variable based on ratio of the floor versus total floors in building
c$floor_ratio <- c$floor/c$max_floor

# see all the build years to find errors
table(as.factor(c$build_year))

# remove/replace errors
c$build_year[c$build_year==0]<-NA
c$build_year[c$build_year==1]<-NA
c$build_year[c$build_year==2]<-NA
c$build_year[c$build_year==3]<-NA
c$build_year[c$build_year==20]<-2000
c$build_year[c$build_year==71]<-1971
c$build_year[c$build_year==215]<-2015
c$build_year[c$build_year==20052009]<-2005
c$build_year[c$build_year==4965]<-NA
c$build_year[c$build_year==2018]<-NA
c$build_year[c$build_year==2019]<-NA

# create new variable for how many years old 
c$year_old <- 2017 - c$build_year

# create new variable for inverse floor
c$floor_inverse <- as.integer(c$max_floor) - as.integer(c$floor)

# find non living area
c$non_living_area <- (c$full_sq-c$life_sq)*100/c$full_sq

# area per room
c$room_area <- c$life_sq/c$num_room

c$state[c$state=="33"]<-"3"

id.vars <- grep("^[I][D]",names(c))
print(names(c[ , id.vars]))
c<-c[,-id.vars]

gender.vars <- grep("^[m][a][l][e]",names(c))
print(names(c[ , gender.vars]))
c<-c[,-gender.vars]

gender.vars <- grep("^[y][o][u][n]",names(c))
print(names(c[ , gender.vars]))
c<-c[,-gender.vars]

gender.vars <- grep("^[w][o][r][k]",names(c))
print(names(c[ , gender.vars]))
c<-c[,-gender.vars]

gender.vars <- grep("^[X]",names(c))
print(names(c[ , gender.vars]))
c<-c[,-gender.vars]

gender.vars <- grep("^[e][k][d][e][r]",names(c))
print(names(c[ , gender.vars]))
c<-c[,-gender.vars]

gender.vars <- grep("^[f][e][m][a][l]",names(c))
print(names(c[ , gender.vars]))
c<-c[,-gender.vars]

# fix errors
c$full_sq[c$full_sq>300]<-median(c$full_sq)
c$life_sq[c$life_sq>400]<-median(c$full_sq)
c$kitch_sq[c$kitch_sq>200]<-median(c$full_sq)
c$floor[c$floor>45]<-median(c$full_sq)
c$max_floor[c$max_floor>60]<-median(c$max_floor)

c$floor_from_top<-c$max_floor-c$floor

c$ratio_preschool <- c$children_preschool / c$preschool_quota

c$ratio_school <- c$children_school / c$school_quota

# remove rows with more than 40% missing in macro
macro$load_of_teachers_preschool_per_teacher<-NULL
macro$hospital_bed_occupancy_per_year<-NULL
macro$hospital_beds_available_per_cap<-NULL
macro$students_reg_sports_share<-NULL
macro$population_reg_sports_share<-NULL
macro$museum_visitis_per_100_cap<-NULL
macro$theaters_viewers_per_1000_cap<-NULL

# create new features from timestamp
macro$date <- as.POSIXct(strptime(macro$timestamp, format = "%Y-%m-%d"))
macro$day<-as.factor(format(macro$date, "%d")) # day
macro$month <- as.factor(format(macro$date, "%m")) # month
macro$year <- as.integer(format(macro$date, "%Y")) # year
macro$timestamp <- NULL
macro$date <- NULL

# merge c and macro into one data frame
c<-merge(c, macro, by=c("day", "month", "year"), all.x=TRUE)
c$day<-NULL
c$month<-NULL

c$sub_area<-NULL

c$full_all<-NULL

# run linear regression to find significant variables
linreg<-lm(price_doc~., c)

# turn off scientific notation
options(scipen=999)

# boolean wherer TRuE means p-value is less than 0.10
toselect.x <- summary(linreg)$coeff[-1,4] < 0.50

# subset to only include variables with p value less than 0.10
d<-c[,toselect.x==TRUE]
d$price_doc<-c[,225]
c<-d

c<-na.roughfix(c, fill=TRUE, median=TRUE)

# seperate train and test set
a<-c[(c$id<=30473),]
b<-c[(c$id>30473),]

# remove id from train and test set
names(a)
a<-a[,-a$id]
id<-b[,b$id]
b<-b[,-b$id]

# remove price_doc from test set
b<-b[,-b$price_doc]

set.seed(1234)
rforest <- randomForest(price_doc~.,data=a,ntree=25, corr.bias=TRUE, na.action=na.omit)
forest_predict<-predict(rforest, b)
b$rforest<-forest_predict

set.seed(1235)
linreg<-lm(price_doc~., data=a, singular.ok = TRUE, na.action=na.omit)
linear_predict<-predict(linreg, b)
b$linear<-linear_predict

set.seed(1236)
rpart<-rpart(price_doc~., data=a, na.action=na.omit)
rpart_predict<-predict(rpart, b)
b$rpart<-rpart_predict

b$average<-mean(b$rforest, b$linear, b$rpart)
