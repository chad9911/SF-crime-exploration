# setwd("Z:/r projects/sacrime")

train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Load in libraries

library(dplyr)
library(reshape2)

# Fill in gaps between train and test, then combine them into one data frame

train$Id <- NA
test$Category <- NA
test$Descript <- NA
test$Resolution <- NA

combi <- rbind(train,test)

# Breaking time variables down into different date parts 

combi$Dates <- as.character(combi$Dates)
combi$Year <- as.factor(substr(combi$Dates,1,4))
combi$Day <- sapply(combi$Dates,FUN=function(x){
        strsplit(x,split='[ ]')[[1]][1]})
combi$Time <- sapply(combi$Dates,FUN=function(x){
        strsplit(x,split='[ ]')[[1]][2]})
combi$Hour <- as.factor(substr(combi$Time,1,2))

#========================= Exploration

DayWkDist <- train %>%
        group_by(Category,DayOfWeek) %>%
        summarise(total.count=n()) %>%
        data.frame()

train %>%
        group_by(Category,PdDistrict) %>%
        summarise(total.count=n()) %>%
        data.frame() %>%
        dcast(Category ~ PdDistrict)

#=====================================

# Break combined dataset back into train and test

train.clean <- combi[1:878049,]
test.clean <- combi[878050:1762311,]

# Save cleaned version

save(train.clean,file = "train_modified.RData")
save(test.clean,file = "test_modified.RData")

