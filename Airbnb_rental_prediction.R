rm(list = ls(all.names = TRUE))
install.packages("dplyr")
install.packages('caret')
install.packages('ggplot2')
install.packages('lattice')
install.packages("magrittr")
install.packages('gbm')
library(dplyr)
library(tidyr)
library(magrittr)
data=read.csv('/Users/zhouqiao/Desktop/First_Se_CU/Kaggle/analysisData2.csv')
data1=read.csv('/Users/zhouqiao/Desktop/First_Se_CU/Kaggle/scoringData.csv')
install.packages('stringr')
### combine the scoring data and the analysis data together to make their levels identical ###
price=data[,47]
data_1=data[,-47]
data_1$is_test=TRUE
data1$is_test=FALSE
data_1=data_1[,-71]
new_data=rbind(data_1,data1)
library(stringr)
####clean the data####
new_data$square_feet =  ifelse(is.na(new_data$square_feet), median(new_data$square_feet, na.rm = TRUE), new_data$square_feet)
new_data$security_deposit =  ifelse(is.na(new_data$security_deposit), median(new_data$security_deposit, na.rm = TRUE), new_data$security_deposit)
new_data$cleaning_fee =  ifelse(is.na(new_data$cleaning_fee), median(new_data$cleaning_fee, na.rm = TRUE), new_data$cleaning_fee)
new_data$review_scores_rating =  ifelse(is.na(new_data$review_scores_rating), median(new_data$review_scores_rating, na.rm = TRUE), new_data$review_scores_rating)
new_data$review_scores_location =  ifelse(is.na(new_data$review_scores_location), median(new_data$review_scores_location, na.rm = TRUE), new_data$review_scores_location)
new_data$number_of_reviews =  ifelse(is.na(new_data$number_of_reviews), median(new_data$number_of_reviews, na.rm = TRUE), new_data$number_of_reviews)
new_data$weekly_price =  ifelse(is.na(new_data$weekly_price), median(new_data$weekly_price, na.rm = TRUE), new_data$weekly_price)
new_data$availability_30 =  ifelse(is.na(new_data$availability_30), median(new_data$availability_30, na.rm = TRUE), new_data$availability_30)
new_data$no_rules =  ifelse(is.na(new_data$house_rules),1,0)
new_data$no_rules = as.numeric(new_data$no_rules)
new_data$no_rules = as.factor(new_data$no_rules)
new_data$convenience =  ifelse(is.na(new_data$transit),1,0)
new_data$convenience = as.numeric(new_data$convenience)
new_data$convenience = as.factor(new_data$convenience)
new_data$host_identity_verified=droplevels(new_data$host_identity_verified)
##convert the date variable to POSIXct###
new_data=new_data%>%
  mutate(host_since=
           new_data$host_since%>%
           as.numeric()%>%
           as.Date(origin='1900-01-01')%>%
           as.POSIXct()
  )
new_data$host_since=ifelse(is.na(new_data$host_since), new_data$host_since[1], new_data$host_since)
new_data=new_data%>%
  mutate(
    last_review=
      new_data$last_review%>%
      as.numeric()%>%
      as.Date(origin='1900-01-01')%>%
      as.POSIXct()
  )
new_data$last_review=ifelse(is.na(new_data$last_review), new_data$last_review[1], new_data$last_review)
###create new variables from the descriptive columns###
aac=c()
for (i in 1:nrow(new_data))
{aac=c(aac,str_detect(new_data$amenities[i],regex('cable tv',ignore_case = "TRUE")))}
new_data$cable_tv=aac
new_data$cable_tv = as.numeric(new_data$cable_tv)
new_data$cable_tv = as.factor(new_data$cable_tv)

aac=c()
for (i in 1:nrow(new_data))
{aac=c(aac,str_detect(new_data$amenities[i],regex('tv',ignore_case = "TRUE")))}
new_data$tv=aac
new_data$cable_tv = as.numeric(new_data$cable_tv)
new_data$tv = as.factor(new_data$tv)

aac=c()
for (i in 1:nrow(new_data))
{aac=c(aac,str_detect(new_data$amenities[i],regex('breakfast',ignore_case = "TRUE")))}
new_data$breakfast=aac
new_data$breakfast = as.numeric(new_data$breakfast)
new_data$breakfast = as.factor(new_data$breakfast)

aac=c()
for (i in 1:nrow(new_data))
{aac=c(aac,str_detect(new_data$amenities[i],regex('gym',ignore_case = "TRUE")))}
new_data$gym=aac
new_data$gym = as.numeric(new_data$gym)
new_data$gym = as.factor(new_data$gym)

aac=c()
for (i in 1:nrow(new_data))
{aac=c(aac,str_detect(new_data$amenities[i],regex('kitchen',ignore_case = "TRUE")))}
new_data$kitchen=aac
new_data$kitchen = as.numeric(new_data$kitchen)
new_data$kitchen = as.factor(new_data$kitchen)

aac=c()
for (i in 1:nrow(new_data))
{aac=c(aac,str_detect(new_data$amenities[i],regex('swimming pool',ignore_case = "TRUE")))}
new_data$swimming_pool=aac
new_data$swimming_pool = as.numeric(new_data$swimming_pool)
new_data$swimming_pool = as.factor(new_data$swimming_pool)

aac=c()
for (i in 1:nrow(new_data))
{aac=c(aac,str_detect(new_data$amenities[i],regex('elevator',ignore_case = "TRUE")))}
new_data$elevator=aac
new_data$elevator = as.numeric(new_data$elevator)
new_data$elevator = as.factor(new_data$elevator)

aac=c()
for (i in 1:nrow(new_data))
{aac=c(aac,str_detect(words[i],regex('spacious',ignore_case = "TRUE"))|str_detect(words[i],regex('large',ignore_case = "TRUE")))}
new_data$spacious=aac
new_data$spacious = as.numeric(new_data$spacious)
new_data$spacious = as.factor(new_data$spacious)

aac=c()
for (i in 1:nrow(new_data))
{aac=c(aac,str_detect(words[i],regex('gorgeous',ignore_case = "TRUE"))|str_detect(words[i],regex('beautiful',ignore_case = "TRUE")))}
new_data$pretty=aac
new_data$pretty = as.numeric(new_data$pretty)
new_data$pretty = as.factor(new_data$pretty)

aac=c()
for (i in 1:nrow(new_data))
{aac=c(aac,str_detect(new_data$name[i],regex('luxury',ignore_case = "TRUE")))}
new_data$luxury=aac
new_data$luxury = as.numeric(new_data$luxury)
new_data$luxury = as.factor(new_data$luxury)
### split the new_data to train and test data set ###

train=new_data[new_data$is_test==TRUE,]
test=new_data[new_data$is_test==FALSE,]
train$price=price
### use boosting model to predict price ###
library(caret)
library(ggplot2)
library(lattice)
library(gbm)
modelboost4 <- gbm(price~review_scores_location + security_deposit + guests_included + calculated_host_listings_count_private_rooms + square_feet + cleaning_fee + accommodates + zipcode + minimum_nights + bedrooms + bathrooms+ neighbourhood_cleansed + review_scores_rating + cleaning_fee + bed_type + property_type +state+cable_tv+
                     gym+breakfast+elevator+tv+spacious+host_since+last_review + luxury + host_is_superhost+is_location_exact+
                     accommodates+beds+weekly_price+host_identity_verified+extra_people+calendar_updated+minimum_nights+maximum_nights+availability_30+availability_60+number_of_reviews+review_scores_location+require_guest_profile_picture+instant_bookable, data = train, shrinkage = 0.001, distribution = "gaussian", interaction.depth = 4, n.trees = 40000)
predBoostTrain = predict(modelboost4, n.trees = 40000)
rmseBoostTrain = sqrt(mean((predBoostTrain-train$price)^2)); rmseBoostTrain
predBoostFinal = predict(modelboost4, n.trees = 40000, newdata=test)
submissionFile = data.frame(id = data1$id, price = predBoostFinal)
write.csv(submissionFile, 'submission133.csv',row.names = F)
