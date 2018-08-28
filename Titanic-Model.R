# Kaggle Titanic Competition
# Henry Tessier
# hmt6ra
# SYS6018

# read data
trainread <- read.csv('train.csv')
test <- read.csv('test.csv')

# retain relevant columns and split train into validate.train
train.sample <- sample(1:891, size = 700)
train <- trainread[train.sample,]
validate <- trainread[-train.sample,]
columns <- c('Pclass','Sex','Age','SibSp','Parch','Fare','Survived')

train <- train[columns]
validate <- validate[columns]

titanic.lg1 <- glm(Survived~., data = train, family = 'binomial')
summary(titanic.lg1)

# Remove Parch, Fare 
titanic.lg2 <- glm(Survived~.-Parch-Fare, data = train, family = 'binomial')
summary(titanic.lg2)
# Model looks decent

# Cross Validate
probs<-as.vector(predict(titanic.lg1,newdata=validate[1:6], type="response"))
preds <- rep(0,191) 
preds[probs>0.5] <- 1 
table(preds,validate$Survived)

probs<-as.vector(predict(titanic.lg2,newdata=validate[1:6], type="response"))
preds <- rep(0,191) 
preds[probs>0.5] <- 1 
table(preds,validate$Survived)

# Models perform essentially the same - choose model #2
# Recalculate model with complete data
trainfin <- trainread[columns]
lgfin <- glm(Survived~.-Parch-Fare, data = trainfin, family = 'binomial')
summary(lgfin)

columns <- c('Pclass','Sex','Age','SibSp','Parch','Fare','PassengerId')
test <- test[columns]
meanage <- mean(trainread$Age, na.rm = T)
test$Age[is.na(test$Age)] <- meanage

test$prob <-as.vector(predict(lgfin,newdata=test[1:6], type="response"))
test$Survived[test$prob > 0.5] <- 1
test$Survived[test$prob <= 0.5] <- 0
test <- test[c('PassengerId','Survived')]

# Write to csv
write.csv(test, 'Titanic-Predictions')
