
# ****************** This model building script is with issues and will be updated

# load cleaned train dataset with additional zipcode column

load(file = "train_modified2.RData")

cri <- train.zip

levels(cri$Postcode)[1] <- "Unknown"

library(caret)

set.seed(199)

inTraining <- createDataPartition(cri$Category,p = .1, list=F,times = 1)
saTrain <- cri[inTraining,]
saValid <- cri[-inTraining,]

control <- trainControl(method = "cv", number = 3)

cri <- cri[,c("Category","DayOfWeek","PdDistrict","Year")]
formula = Category ~ Year + DayOfWeek + PdDistrict

#LVQ
set.seed(199)
sa.Lvq <- train(formula, data=saTrain, method="lvq", trControl=control)

#RF
set.seed(199)
sa.Rf <- train(formula, data=saTrain, method="rf")

#GBM
set.seed(199)
sa.Gbm <- train(formula, data=saTrain, method="gbm", trControl=control)

#SVM
set.seed(199)
sa.Svm <- train(formula, data=cri1, method="svmRadial", trControl=control)

# Summarise and compare models

summary(resamples(list(LVQ=sa.Lvq, RF=sa.Rf, GBM=sa.Gbm, SVM=sa.Svm)))
bwlot(resamples(list(LVQ=sa.Lvq, RF=sa.Rf, GBM=sa.Gbm, SVM=sa.Svm)))

