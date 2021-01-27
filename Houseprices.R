#Load libraries
library(dplyr)
library(ggplot2)
library(stringr)

#Analyze both datasets
str(trainHouse)
str(testHouse)

#Number of Numerical and Categorical variables
table(sapply(trainHouse[,1:81], typeof))
table(sapply(testHouse[,1:80], typeof))

#Analyze categorical and Numerical data
sapply(trainHouse[,(which(sapply(trainHouse[,1:81], typeof) == "character"))], table)
summary(trainHouse[,(which(sapply(trainHouse[,1:81], typeof) == "double"))])
sapply(testHouse[,(which(sapply(testHouse[,1:80], typeof) == "character"))], table)
summary(testHouse[,(which(sapply(testHouse[,1:80], typeof) == "double"))])


#SalePrice
ggplot(trainHouse, aes(SalePrice)) + geom_density()
summary(trainHouse$SalePrice)

#Combine both data frames
testHouse <- mutate(testHouse, SalePrice = rep(NA, nrow(testHouse)))
data_combined <- rbind(testHouse, trainHouse)

#Analyze new data frame
table(sapply(data_combined[,1:81], typeof))
sapply(data_combined[,(which(sapply(data_combined[,1:81], typeof) == "character"))], table)
summary(data_combined[,(which(sapply(data_combined[,1:81], typeof) == "double"))])

#Show missing data
sum(is.na(data_combined))
library(visdat)
vis_miss(data_combined)


#Subset Categorical data in new data frame
Categoricaldata <- data_combined[,(which(sapply(data_combined[,1:81], typeof) == "character"))]
table(sapply(Categoricaldata[,1:43], typeof))

#Visualize Categorical data
Categoricaldata[1:43] <- lapply(Categoricaldata[1:43], as.factor)

Multiplebarplots <- function(data, na.rm = TRUE, ...) {
  cn <- colnames(data)
  for (i in seq_along(cn)) {
    print(ggplot(data,aes_string(x = cn[i])) + geom_bar()) }
}

Multipleboxplots <- function(data,y, na.rm = TRUE, ...) {
  cn <- colnames(data)
  for (i in seq_along(cn)) {
    print(ggplot(data,aes_string(x = cn[i],y=y)) + geom_boxplot()) }
}


Multiplebarplots(Categoricaldata)
Multipleboxplots(Categoricaldata, y = data_combined$SalePrice)


#subset numerical data in new data frame
Numericaldata <- data_combined[,(which(sapply(data_combined[,1:81], typeof) == "double"))]
table(sapply(Numericaldata[,1:37], typeof))
Numericaldata$SalePrice <- data_combined$SalePrice

#Visualize Numerical data
Multipledensityplots <- function(data, na.rm = TRUE, ...) {
  cn <- colnames(data)
  for (i in seq_along(cn)) {
    print(ggplot(data,aes_string(x = cn[i])) + geom_density()) }
}

#change colnames for function to work
colnames(Numericaldata)[14] <- "Firstfloor"
colnames(Numericaldata)[15] <- "SecondFloor"
colnames(Numericaldata)[32] <- "threeSsnPorch"


Multipledensityplots(Numericaldata)

Multiplescatterplots <- function(data,y, na.rm = TRUE, ...) {
  cn <- colnames(data)
  for (i in seq_along(cn)) {
    print(ggplot(data,aes_string(x = cn[i], y = y)) + geom_point()) }
}

Multiplescatterplots(Numericaldata, y = Numericaldata$SalePrice)

#some are rather factors as seen by the plots
Numericalfactordata <- Numericaldata %>% select(MSSubClass, OverallQual, OverallCond,
                                                BsmtFullBath, BsmtHalfBath, FullBath,
                                                HalfBath,BedroomAbvGr, KitchenAbvGr, 
                                                TotRmsAbvGrd, Fireplaces, GarageCars,
                                                MoSold, YrSold)
#Visualize discrete variables
Numericalfactordata[1:14] <- lapply(Numericalfactordata[1:14], as.factor)
str(Numericalfactordata)
Multiplebarplots(Numericalfactordata)
Multipleboxplots(Numericalfactordata, y = data_combined$SalePrice)

#create dataframe containing continuous variables
Continuousvariables <- Numericaldata %>% select(-MSSubClass, -OverallQual, -OverallCond,
                                                -BsmtFullBath, -BsmtHalfBath, -FullBath,
                                                -HalfBath,-BedroomAbvGr, -KitchenAbvGr, 
                                                -TotRmsAbvGrd, -Fireplaces, -GarageCars,
                                                -MoSold, -YrSold)

#analyze correlation with continuous variables
Regression <- function(x){lm(Continuousvariables$SalePrice ~ Continuousvariables[[x]])}
Allregressions <- lapply(1:24, Regression)
Summaries <- lapply(Allregressions, summary)
#Highest Rsquared 4 = 0.27(YearBuilt), 5 = 0.25(YearRemodAdd), 6 = 0.23(MasVrnArea), 
#10 = 0.38(TotalBsmtSF), 11 = 0.37(Firsfloor), 14 = 0.50(GrLivArea),
#15 = 0.24 (GarageYearBuilt), 16 = 0.39(GarageArea)
#try with the other numerical data
Numericalfactordata$SalePrice <- data_combined$SalePrice
Regression <- function(x){lm(Numericalfactordata$SalePrice ~ Numericalfactordata[[x]])}
Allregressions <- lapply(1:14, Regression)
Summaries <- lapply(Allregressions, summary)
#Highest R squared MSSubClass = 0.25, OverallQual = 0.68, FullBath = 0.33, TotRmsAbvGrd = 0.29
#Fireplaces = 0.23, Garagecars = 0.49


#Transform data 
sapply(trainHouse[1:80], function(x) sum(is.na(x)))

#Let me know if you manage this in a loop remove NA and mean imputation
trainHouse$MasVnrArea[which(is.na(trainHouse$MasVnrArea))] <- mean(trainHouse$MasVnrArea,na.rm=TRUE)
trainHouse$Alley[which(is.na(trainHouse$Alley))] <- "None"
trainHouse$MasVnrType[which(is.na(trainHouse$MasVnrType))] <- "None"
trainHouse$LotFrontage[which(is.na(trainHouse$LotFrontage))] <- median(trainHouse$LotFrontage,na.rm = TRUE)
trainHouse$FireplaceQu[which(is.na(trainHouse$FireplaceQu))] <- "None"
trainHouse$PoolQC[which(is.na(trainHouse$PoolQC))] <- "None"
trainHouse$Fence[which(is.na(trainHouse$Fence))] <- "None"
trainHouse$MiscFeature[which(is.na(trainHouse$MiscFeature))] <- "None"
trainHouse$GarageType[which(is.na(trainHouse$GarageType))] <- "None"
trainHouse$GarageYrBlt[which(is.na(trainHouse$GarageYrBlt))] <- 0
trainHouse$GarageFinish[which(is.na(trainHouse$GarageFinish))] <- "None"
trainHouse$GarageQual[which(is.na(trainHouse$GarageQual))] <- "None"
trainHouse$GarageCond[which(is.na(trainHouse$GarageCond))] <- "None"
trainHouse$BsmtQual[which(is.na(trainHouse$BsmtQual))] <- "None"
trainHouse$BsmtCond[which(is.na(trainHouse$BsmtCond))] <- "None"
trainHouse$BsmtExposure[which(is.na(trainHouse$BsmtExposure))] <- "None"
trainHouse$BsmtFinType1[which(is.na(trainHouse$BsmtFinType1))] <- "None"
trainHouse$BsmtFinType2[which(is.na(trainHouse$BsmtFinType2))] <- "None"
trainHouse$Electrical[which(is.na(trainHouse$Electrical))] <- "None"

str(trainHouse) 
sapply(trainHouse[1:80], function(x) sum(is.na(x)))

testHouse$MasVnrArea[which(is.na(testHouse$MasVnrArea))] <- mean(trainHouse$MasVnrArea,na.rm=TRUE)
testHouse$Alley[which(is.na(testHouse$Alley))] <- "None"
testHouse$MasVnrType[which(is.na(testHouse$MasVnrType))] <- "None"
testHouse$LotFrontage[which(is.na(testHouse$LotFrontage))] <- median(trainHouse$LotFrontage,na.rm = TRUE)
testHouse$FireplaceQu[which(is.na(testHouse$FireplaceQu))] <- "None"
testHouse$PoolQC[which(is.na(testHouse$PoolQC))] <- "None"
testHouse$Fence[which(is.na(testHouse$Fence))] <- "None"
testHouse$MiscFeature[which(is.na(testHouse$MiscFeature))] <- "None"
testHouse$GarageType[which(is.na(testHouse$GarageType))] <- "None"
testHouse$GarageYrBlt[which(is.na(testHouse$GarageYrBlt))] <- 0
testHouse$GarageFinish[which(is.na(testHouse$GarageFinish))] <- "None"
testHouse$GarageQual[which(is.na(testHouse$GarageQual))] <- "None"
testHouse$GarageCond[which(is.na(testHouse$GarageCond))] <- "None"
testHouse$BsmtQual[which(is.na(testHouse$BsmtQual))] <- "None"
testHouse$BsmtCond[which(is.na(testHouse$BsmtCond))] <- "None"
testHouse$BsmtExposure[which(is.na(testHouse$BsmtExposure))] <- "None"
testHouse$BsmtFinType1[which(is.na(testHouse$BsmtFinType1))] <- "None"
testHouse$BsmtFinType2[which(is.na(testHouse$BsmtFinType2))] <- "None"
testHouse$Electrical[which(is.na(testHouse$Electrical))] <- "None"

sapply(trainHouse[1:80], function(x) sum(is.na(x)))

#Backwards elimination using multiple linear regression
colnames(trainHouse)[44] <- "Firstfloor"
colnames(testHouse)[44] <- "Firstfloor"


summary(lm(SalePrice ~ YearBuilt + YearRemodAdd + MasVnrArea + TotalBsmtSF + Firstfloor + GrLivArea + GarageYrBlt +
             GarageArea + MSSubClass + OverallQual + FullBath + TotRmsAbvGrd + Fireplaces + GarageCars, data = trainHouse))
#Eliminate TotRmsAbvGrd
summary(lm(SalePrice ~ YearBuilt + YearRemodAdd + MasVnrArea + TotalBsmtSF + Firstfloor + GrLivArea + GarageYrBlt +
             GarageArea + MSSubClass + OverallQual + FullBath +  Fireplaces + GarageCars, data = trainHouse))
#Eliminate FullBath
summary(lm(SalePrice ~ YearBuilt + YearRemodAdd + MasVnrArea + TotalBsmtSF + Firstfloor + GrLivArea + GarageYrBlt +
             GarageArea + MSSubClass + OverallQual +  Fireplaces + GarageCars, data = trainHouse))
#Eliminate Firsfloor
summary(lm(SalePrice ~ YearBuilt + YearRemodAdd + MasVnrArea + TotalBsmtSF + GrLivArea + GarageYrBlt +
             GarageArea + MSSubClass + OverallQual + Fireplaces + GarageCars, data = trainHouse))
#Eliminate GarageYrBlt
summary(lm(SalePrice ~ YearBuilt + YearRemodAdd + MasVnrArea + TotalBsmtSF + GrLivArea +
             GarageArea + MSSubClass + OverallQual + Fireplaces + GarageCars, data = trainHouse))
#Eliminare GarageArea
summary(lm(SalePrice ~ YearBuilt + YearRemodAdd + MasVnrArea + TotalBsmtSF + GrLivArea +
             MSSubClass + OverallQual + Fireplaces + GarageCars, data = trainHouse))

Regressor <- lm(SalePrice ~ YearBuilt + YearRemodAdd + MasVnrArea + TotalBsmtSF + GrLivArea +
                  MSSubClass + OverallQual + Fireplaces + GarageCars, data = trainHouse)


y_pred <-predict(Regressor, testHouse)




#Randomforest prediction

library(randomForest)
colnames(trainHouse)[44] <- "Firstfloor"
colnames(trainHouse)[45] <- "Secondfloor"
colnames(trainHouse)[70] <- "ThreeSsnPorch"
colnames(testHouse)[44] <- "Firstfloor"
colnames(testHouse)[45] <- "Secondfloor"
colnames(testHouse)[70] <- "ThreeSsnPorch"
Regressor <- randomForest(SalePrice~., data = trainHouse)
varImpPlot(Regressor)
y_pred <- predict(Regressor, testHouse)  

#rpart decision tree
library(rpart)
Regressor <-rpart(SalePrice ~., data=trainHouse,
                                control = rpart.control(minsplit = 100))
plot(Regressor)
y_pred <- predict(Regressor, testHouse) 

#Support vector machine can be applied only to factors with 2 or more levels
library(e1071)
Regressor <- svm(SalePrice ~., data=trainHouse, type = "eps-regression")
y_pred <- predict(Regressor, testHouse)

#Make a submission of any of the models
y_pred[!complete.cases(y_pred)] <- mean(y_pred, na.rm = TRUE)
Submission = data.frame(testHouse$Id, y_pred) 
colnames(Submission) <- c("Id", "SalePrice")
write.csv(Submission, file = "Housepricepredictions.csv", row.names = FALSE)



