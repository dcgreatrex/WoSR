#============================================================================
#' WoSR - Classification software
#' description: 
#' data from: http://groupware.les.inf.puc-rio.br/har
#' department: Science and Enabling Units IT
#' contacts: fruzsina.soltesz@astrazeneca.com; david.greatrex@astrazeneca.com
#' Last modified: 13-06-2018
#============================================================================

#============================================================================
# Set repository URL (ensure url points to local respository location)
#============================================================================
WoSR <- "C:/Users/kwsp220/Box Sync/projects/cambridge_team/conference/WoSR/WoSR"
setwd(WoSR)

#============================================================================
# Environment and parameter setting
#============================================================================
library(plyr)
library(dplyr)
library(knitr)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
set.seed(35)

#============================================================================
# load data
#============================================================================

# load all csv files into list
f <- list.files("data/")
dat <- lapply(f[endsWith(f, ".csv")], FUN = function(x){
  
  # load and process each csv file
  tmp <- read.csv(paste0("data/", x))

})
names(dat) <- f

# set train and test datafiles
train <- dat$`pml-training.csv`
test <- dat$`pml-testing.csv`


# =====================================================================
# Explore and prepare training data
# =====================================================================
# look at data dimensions and feature names
dim(train)
names(train)

# remove ID and timestamp variables
train <- train[,-(1:5)]

# create a train/ test set within the training data
trainslpit  <- createDataPartition(train$classe, p=0.7, list=FALSE)
train.train <- train[trainslpit, ]
train.test  <- train[-trainslpit, ]

# remove variables where majority of values are missing (95%)
misses   <- sapply(train.train, function(x) mean(is.na(x))) > 0.95
train.train <- train.train[, misses==F]
train.test  <- train.test[, misses==F]
dim(train.train)

# remove variables with Nearly Zero Variance (NZV)
NZV <- nearZeroVar(train.train)
train.train <- train.train[, -NZV]
train.test  <- train.test[, -NZV]
dim(train.train)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# correlation analysis - (not in submission)
library(corrplot)

corMatrix <- cor(select(train.train,-(classe)))
corrplot(corMatrix, order = "FPC", method = "color", type = "lower", 
         tl.cex = 0.8, tl.col = rgb(0, 0, 0))

# here could come PCA


# =====================================================================
# Model building
# =====================================================================

#......................................................................
# 1) Random Forest
#......................................................................
# crossvalidation set
cvset <- trainControl(method="cv", number=5, verboseIter=F)
# model fit
rfmodel <- train(classe ~ ., data=train.train, method="rf",
                 trControl=cvset)
# the model
rfmodel$finalModel

# prediction on test set, and evaluation (accuracy)
rfpredict <- predict(rfmodel, newdata=train.test)
confmatrix <- confusionMatrix(rfpredict, train.test$classe)
# accuracy
print(paste('out of sample error:',as.character(1-confmatrix$overall[1])))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot matrix results - not for report
plot(confmatrix$table, col = confmatrix$byClass, 
     main = paste("Random Forest - Accuracy =",
                  round(confmatrix$overall['Accuracy'], 4)))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#......................................................................
# 2) Generalised Boost Model
#......................................................................
cvset <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
gbmmodel  <- train(classe ~ ., data=train.train, method = "gbm",
                    trControl = cvset, verbose = F)
gbmmodel$finalModel

# prediction on test set, and evaluation (accuracy, and out of sample error)
gbmpredict <- predict(gbmmodel, newdata=train.test)
confmatrix <- confusionMatrix(gbmpredict, train.test$classe)
# accuracy
confmatrix$overall[1]
print(paste('out of sample error:',as.character(1-confmatrix$overall[1])))

#......................................................................
# 3) Decision Tree
#......................................................................
treemodel <- rpart(classe ~ ., data=train.train, method="class")
fancyRpartPlot(treemodel)

# prediction on test set, and evaluation (accuracy, and out of sample error)
treepredict <- predict(treemodel, newdata=train.test, type="class")
confmatrix <- confusionMatrix(treepredict, train.test$classe)
# accuracy
confmatrix$overall[1]
print(paste('out of sample error:',as.character(1-confmatrix$overall[1])))


# =====================================================================
# Model application
# =====================================================================
test.prediction <- predict(rfmodel, newdata=testing)
test.prediction

