#download data from kaggle manually
library(dplyr)
library(ggplot2)
library(stringr)

#add new column and combine test and train
test <- mutate(test, Survived = "None")
data_combined <- rbind(test, train)

#change to factor
str(data_combined)
data_combined$Pclass <- as.factor(data_combined$Pclass)
data_combined$Sex <- as.factor(data_combined$Sex)
data_combined$Survived <- as.factor(data_combined$Survived)

#data exploration 
table(data_combined$Survived[419:1309])
table(data_combined$Pclass)
table(data_combined$Sex)
summary(data_combined$Age)
data_combined %>% group_by(Survived) %>% summarize(meanAge = mean(Age, na.rm = TRUE))
data_combined[419:1309,] %>% group_by(Pclass, Sex) %>% 
  summarize(meanSurvival = mean(as.integer(as.character(Survived), na.rm = TRUE)))

#visualizations 
ggplot(data_combined, aes(x = Sex)) + geom_bar()
ggplot(data_combined, aes(x = Pclass)) + geom_bar()
ggplot(data_combined[419:1309,], aes(x = Pclass, fill = Survived)) + geom_bar()
ggplot(data_combined[419:1309,], aes(x = Sex, fill = Survived)) + geom_bar()
ggplot(data_combined[419:1309,], aes(x = Pclass, fill = Survived)) + geom_bar() + 
  facet_wrap(~ Sex)
ggplot(data_combined, aes(x = Age)) + geom_density()
ggplot(data_combined[419:1309,], aes(x= Survived, y = Age)) + geom_boxplot()
ggplot(data_combined[419:1309,], aes(x = Age, fill = Survived)) + geom_bar() + 
  facet_wrap(~ Sex + Pclass)
#check names
data_combined[!complete.cases(data_combined$Name),]
length(which(duplicated(data_combined$Name)))
data_combined[duplicated(data_combined$Name),]

#analyze and extract titles from names
length(which(str_detect(data_combined$Name, "Master"))) + 
  length(which(str_detect(data_combined$Name, "Mr"))) + 
  length(which(str_detect(data_combined$Name, "Mrs"))) + 
  length(which(str_detect(data_combined$Name, "Miss")))

head(data_combined[str_detect(data_combined$Name, "Mr"),])

#Mr includes Mr and Mrs overlap
head(data_combined[str_detect(data_combined$Name, "Mrs."),])
head(data_combined[str_detect(data_combined$Name, "Miss."),])
head(data_combined %>% filter(Sex == "male"))
head(data_combined[str_detect(data_combined$Name, "Mr."),] %>% filter(Sex == "male"))
head(data_combined[str_detect(data_combined$Name, "Master."),])

#Add column title make sure to put mrs before mr
data_combined$Title <- ifelse(data_combined$Name %>% str_detect("Mrs."), "Mrs", 
       ifelse(data_combined$Name %>% str_detect("Mr."), "Mr", 
              ifelse(data_combined$Name %>% str_detect("Miss."), "Miss", 
                     ifelse(data_combined$Name %>% str_detect("Master."), "Master", "Other"))))

#Exploration
data_combined$Title <- as.factor(data_combined$Title)
data_combined[!complete.cases(data_combined$Title),]
table(data_combined$Title)
data_combined[419:1309,] %>% group_by(Pclass, Title) %>% 
      summarize(meanSurvival = mean(as.integer(as.character(Survived), na.rm = TRUE)))

data_combined %>% group_by(Title, Pclass) %>% summarize(meanAge = mean(Age, na.rm = TRUE))

                                                               
data_combined[which(data_combined$Title == "Mr"),] %>% select(Age) %>% summary()
data_combined[which(data_combined$Title == "Master"),] %>% select(Age) %>% summary()
data_combined[which(data_combined$Title == "Miss"),] %>% select(Age) %>% summary()
data_combined[which(data_combined$Title == "Mrs"),] %>% select(Age) %>% summary()
data_combined[which(data_combined$Title == "Other"),] %>% select(Age) %>% summary()

  
#Visualize Titles
ggplot(data_combined, aes(x = Title, fill = Survived)) + geom_bar()
ggplot(data_combined, aes(x = Title, fill = Survived)) + geom_bar() + 
  facet_wrap(~ Sex + Pclass) + labs(title = "Titanic Survivors")

ggplot(data_combined[419:1309,], aes(x = Age, fill = Survived)) + geom_bar() + 
  facet_wrap(~ Title + Pclass)

#Ticket
data_combined$Ticket <- as.character(data_combined$Ticket)
head(data_combined$Ticket, 15)
data_combined$TicketChar <- ifelse(data_combined$Ticket == "", " " , substr(data_combined$Ticket, 1, 1))
table(data_combined$TicketChar)
data_combined$Ticket <- as.factor(data_combined$Ticket)
ggplot(data_combined, aes(x = TicketChar, fill = Survived)) + geom_bar()
data_combined[419:1309,] %>% group_by(TicketChar) %>% 
       summarize(meanSurvival = mean(as.integer(as.character(Survived), na.rm = TRUE)))
ggplot(data_combined, aes(x = TicketChar, fill = Survived)) + geom_bar() +
  facet_wrap(~Pclass + Title)

#Fare
summary(data_combined$Fare)
ggplot(data_combined, aes(x = Fare)) + geom_density()
data_combined %>% group_by(Survived) %>% summarize(meanFare = mean(Fare, na.rm = TRUE))
ggplot(data_combined, aes(x = Fare)) + geom_histogram()
ggplot(data_combined, aes(x = Survived, y = Fare)) + geom_boxplot()
ggplot(data_combined, aes(x = Fare, fill = Survived)) + geom_histogram() +
  facet_wrap(~Sex + Pclass)

#Explore number of Siblings and parents 
table(data_combined$SibSp)
summary(data_combined$SibSp)
data_combined$SibSp <- factor(data_combined$SibSp)
ggplot(data_combined, aes(x = SibSp, fill = Survived)) + geom_bar()
ggplot(data_combined, aes(x = SibSp, fill = Survived)) + geom_bar() + 
  facet_wrap(~Title + Pclass)
table(data_combined$Parch)
summary(data_combined$Parch)
data_combined$Parch <- factor(data_combined$Parch)
ggplot(data_combined, aes(x = Parch, fill = Survived)) + geom_bar()
ggplot(data_combined, aes(x = Parch, fill = Survived)) + geom_bar() + 
  facet_wrap(~Title + Pclass)
data_combined[419:1309,] %>% group_by(SibSp, Parch) %>% 
       summarize(meanSurvival = mean(as.integer(as.character(Survived), na.rm = TRUE)))

data_combined$Familysize <- as.integer(as.character(data_combined$Parch)) + 
  as.integer(as.character(data_combined$SibSp))

data_combined$Familysize <- as.factor(data_combined$Familysize)
             
data_combined[419:1309,] %>% group_by(Familysize) %>% 
  summarize(meanSurvival = mean(as.integer(as.character(Survived), na.rm = TRUE)))

ggplot(data_combined, aes(x = Familysize, fill = Survived)) + geom_bar()

ggplot(data_combined, aes(x = Familysize, fill = Survived)) + geom_bar() +
  facet_wrap(~Pclass + Title)

#cabin
data_combined$Cabin <- as.character(data_combined$Cabin)
data_combined[is.na(data_combined$Cabin),"Cabin"] <- " "
data_combined$Cabin <- gsub(" ", "Z", data_combined$Cabin)
data_combined$CabinChar <- substr(data_combined$Cabin, 1, 1)

ggplot(data_combined[419:1309,], aes(x = CabinChar, fill = Survived)) + geom_bar()

ggplot(data_combined[419:1309,], aes(x = CabinChar, fill = Survived)) + geom_bar() +
  facet_wrap(~Pclass + Title)
data_combined[419:1309,] %>% group_by(CabinChar) %>% 
      summarize(meanSurvival = mean(as.integer(as.character(Survived), na.rm = TRUE)))

# Multiple cabins
Multiple_cabins <- data_combined[which(str_length(data_combined$Cabin) > 4),]
ggplot(Multiple_cabins, aes(x = Cabin, fill = Survived)) + geom_bar() +
  facet_wrap(~Pclass + Title)

#Embarked
unique(data_combined$Embarked)
data_combined$Embarked <- as.factor(data_combined$Embarked)
table(data_combined$Embarked)

data_combined[419:1309,] %>% group_by(Embarked) %>% 
       summarize(meanSurvival = mean(as.integer(as.character(Survived), na.rm = TRUE)))

ggplot(data_combined, aes(x = Embarked, fill = Survived)) + geom_bar() +
  facet_wrap(~Pclass + Title)

#Most Important variables : Pclass, Title, Familysize
#Prepare training set
str(train)
train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)
train$Sex <- as.factor(train$Sex)


train$Title <- ifelse(train$Name %>% str_detect("Mrs."), "Mrs", 
                              ifelse(train$Name %>% str_detect("Mr."), "Mr", 
                                     ifelse(train$Name %>% str_detect("Miss."), "Miss", 
                                            ifelse(train$Name %>% str_detect("Master."), "Master", "Other"))))

train$Familysize <- as.integer(as.character(train$Parch)) + 
  as.integer(as.character(train$SibSp))

train$Familysize <- as.factor(train$Familysize)

#Same for test
str(test)
test$Survived <- as.factor(test$Survived)
test$Pclass <- as.factor(test$Pclass)
test$Sex <- as.factor(test$Sex)


test$Title <- ifelse(test$Name %>% str_detect("Mrs."), "Mrs", 
                      ifelse(test$Name %>% str_detect("Mr."), "Mr", 
                             ifelse(test$Name %>% str_detect("Miss."), "Miss", 
                                    ifelse(test$Name %>% str_detect("Master."), "Master", "Other"))))

test$Familysize <- as.integer(as.character(test$Parch)) + 
  as.integer(as.character(test$SibSp))

test$Familysize <- as.factor(test$Familysize)


#Classification models
set.seed(1234)
#logistic regression
classifier_lg <- glm(formula = Survived ~ Pclass + Title, family = "binomial",
                       data = train)
#accuracy 0.795
classifier_lg <- glm(formula = Survived ~ Parch + Pclass + Title, family = "binomial",
                       data = train)
#accuracy 0.811
classifier_lg <- glm(formula = Survived ~ SibSp + Pclass + Title, family = "binomial",
                       data = train)
#accuracy 0.812
classifier_lg <- glm(formula = Survived ~ Parch + SibSp + Pclass + Title, family = "binomial",
                  data = train)
#accuracy 0.824
classifier_lg <- glm(formula = Survived ~ Familysize + Pclass + Title, family = "binomial",
                       data = train)
#accuracy 0.827
prob_pred <- predict(classifier_lg, type = "response", newdata = train)
y_pred <- ifelse(prob_pred > 0.5, 1, 0)
cm <- table(train$Survived, y_pred)
accuracy <- (cm[1,1] + cm[2,2])/(cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])

#Naive Bayes
library(e1071)
classifier <- naiveBayes(x= train[,c(3,13,14)], y=train$Survived)
y_pred_nb <- predict(classifier, newdata = train[,c(3,13,14)])
cm <- table(train$Survived, y_pred_nb)
accuracy <- (cm[1,1] + cm[2,2])/(cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
#accuracy 0.804


#Decision Tree
library(rpart)
classifier <- rpart(formula = Survived ~ Familysize + Pclass + Title, data = train)
y_pred_dt <- predict(classifier, newdata = train, type = "class")
cm <- table(train$Survived, y_pred_dt)
accuracy <- (cm[1,1] + cm[2,2])/(cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
#accuracy 0.826
plot(classifier)
text(classifier)

#RandomForest
library(randomForest)
(classifier <- randomForest(x= train[,c(3,13)], y=train$Survived, ntrees = 500))
#accuracy 0.7957
(classifier <- randomForest(x= train[,c(3,7,13)], y=train$Survived, ntrees = 500))
# accuracy 0.8036
(classifier <- randomForest(x= train[,c(3,8,13)], y=train$Survived, ntrees = 500))
#accuracy 0.8025
(classifier <- randomForest(x= train[,c(3,7,8,13)], y=train$Survived, ntrees = 500))
#accuracy 0.8193
(classifier <- randomForest(x= train[,c(3,13,14)], y=train$Survived, ntrees = 500))
varImpPlot(classifier)
#accuracy 0.8036

#K fold cross validation
library(caret)
CVfolds <- createMultiFolds(train$Survived, k = 10, times = 5)
Control <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = CVfolds)
CVClassifier <- train(x = train[,c(3,7,8,13)], y = train$Survived, method = "rf", tuneLegth = 3,
                      ntree = 500, trControl = Control)
#accuracy 0.8159038
CVClassifier <- train(x = train[,c(3,13,14)], y = train$Survived, method = "rf", tuneLegth = 3,
                      ntree = 500, trControl = Control)
#accuracy 0.8138939

varImpPlot(CVClassifier$finalModel)

#cross validation using rpart
CVrpart <- train(x = train[,c(3,7,8,13)], y = train$Survived, method = "rpart", tuneLength = 30,
                 trControl = Control)
#accuracy 0.8183783
library(rpart.plot)
prp(CVrpart$finalModel, extra = 1)

Survival_predictions <- predict(CVrpart$finalModel, test[,c(2,6,7,13)], type = "class")
table(Survival_predictions)
Submission <- data.frame(test$PassengerId, Survival_predictions)
colnames(Submission)[1] <- "PassengerId"
colnames(Submission)[2] <- "Survived"
write.csv(Submission, file = "titanicpredictions_csv", row.names =  FALSE)




