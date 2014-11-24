setwd("~/Centrale/Machine Learning/Titanic")
train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv", stringsAsFactors=FALSE)

summary(train)

# They all die !!!

prop.table(table(train$Survived))
test$Survived <- 0

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, "theyalldie.csv", row.names = FALSE)

# Women survive

summary(train$Sex)

prop.table(table(train$Sex))
prop.table(table(train$Sex, train$Survived),1)
test$Survived[test$Sex == "female"] <- 1

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, "womensurvive.csv", row.names = FALSE)

# Women and children first !

summary(train$Age)
train$Child  <- 0
train$Child[train$Age < 18] <- 1

prop.table(table(train$Child, train$Survived),2)

aggregate(Survived ~ Child + Sex, data=train, function(x) sum(x)/length(x))

# A look at fares and pclass

summary(train$Fare)

train$Fare2 <- ">30"
train$Fare2[train$Fare < 30] <- "20-30"
train$Fare2[train$Fare < 20] <- "10-20"
train$Fare2[train$Fare < 10] <- "<10"

aggregate(Survived ~ train$Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

test$Survived[test$Sex == "female" & test$Pclass == 3 & test$Fare >= 20] <- 0

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, "almostallwomensurvive.csv", row.names = FALSE)

# Decision  tree

library(rpart)

install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
fancyRpartPlot(fit)

prediction <- predict(fit, test, type="class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, "decisiontree.csv", row.names = FALSE)

# Decision tree but OVERFITTING

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class", control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)

prediction <- predict(fit, test, type="class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, "decisiontreeoverfitting.csv", row.names = FALSE)

head(train$Name)
strsplit(train$Name, sep=" ")

# Features engineering

test$Survived <- NA
test$Fare2 <- NA
test$Child <- NA
combi <- rbind(train, test)

strsplit(combi$Name[1], '[,.]')[[1]][2]
sapply(combi$Name, function(x) strsplit(x, '[,.]')[[1]][2])
combi$Title <- sapply(combi$Name, function(x) strsplit(x, '[,.]')[[1]][2])



