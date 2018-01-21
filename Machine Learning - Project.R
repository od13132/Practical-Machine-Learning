
## Project Aims:

#The goal of your project is to predict the manner in which they did the 
#exercise. This is the "classe" variable in the training set. You may use any 
#of the other variables to predict with. You should create a report describing 
#how you built your model, how you used cross validation, what you think the 
#expected out of sample error is, and why you made the choices you did. You 
#will also use your prediction model to predict 20 different test cases.

## Workspace prep

if (!require(caret)){
        install.packages("caret")
        library(caret)
}
if (!require(rpart)){
        install.packages("rpart")
        library(rpart)
}
if (!require(rpart.plot)){
        install.packages("rpart.plot")
        library(rpart.plot)
}
if (!require(randomForest)){
        install.packages("randomForest")
        library(randomForest)
}
if (!require(rattle)){
        install.packages("rattle")
        library(rattle)
}


## Import

setwd("C:/Users/osian/Desktop/Machine Learning/Project")

raw.test <- read.csv(file = "pml-testing.csv", header = T)

raw.training <- read.csv(file = "pml-training.csv", header = T)

## Exploratory Analysis on training set

View(head(raw.training, 1000))
dim(raw.training)
dim(raw.test)


# BASIC SUMMARIES AND BACKGROUND
summary(raw.training$classe) # outcome variable
class(raw.training$classe)

# 6 participants perform one set of 10 repetitions of Dumbbell Biceps Curl
# Classe column defines the quality of the execution of the exercise:
# A - satisfied execution of the exercise
# B - throwing the elbows to the front
# C - lifting dumbbell halfway
# D - lowering dumbbell halfway
# E - throwing hips to the front

## Cleaning Data

# Too many variables! Need to drop as many for optimal computer performance

# We will drop the first 7 columns as it won't be needed to do any 
# predictions

wip.training <- raw.training[-(1:7)]
wip.test <- raw.test[-(1:7)]

# Data Frame contains columns with many NAs
colSums(is.na(wip.training))

# Getting rid of columns containing NAs
wip.training <- wip.training[sapply(wip.training, function(x) !any(is.na(x)))]
wip.test <- wip.test[sapply(wip.test, function(x) !any(is.na(x)))]

# Check for zero variance or near zero variance
# This will get rid of constants and missing values
nzv1 <- nearZeroVar(wip.training, saveMetrics = T)
nzv2 <- nearZeroVar(wip.test, saveMetrics = T)
wip.training <- wip.training[, nzv1$nzv == FALSE]
wip.test <- wip.test[, nzv2$nzv == FALSE]

# finished dataset
train <- wip.training
validate <- wip.test

# check the remaining variables
colnames(train)

# We have our test set of 20 observations, however we have a much bigger 
# training set. This will be ideal to split the training set into a sub 
# training and a new test set so we can have the original test set as the 
# validation set.

set.seed(123)
part <- createDataPartition(y = train$classe, p = 0.6, list = FALSE)
train.subTrain <- train[part,] 
train.subTest <- train[-part,]


## Decision Trees

Decision_Tree.Fit <- rpart(classe ~. , data = train.subTrain, method = "class")
fancyRpartPlot(Decision_Tree.Fit)

prediction1 <- predict(Decision_Tree.Fit, train.subTest, type = "class")
con.Matrix1 <- confusionMatrix(prediction1, train.subTest$classe)

con.Matrix1

# 75% Accuracy - needed some improvement!

## Random Forests

modFit2 <- randomForest(classe ~ ., data = train.subTrain)
prediction2 <- predict(modFit2, train.subTest, type = "class")
con.Matrix2 <- confusionMatrix(prediction2, train.subTest$classe)

con.Matrix2

# 100% (0.9955)

# This gives an out-of-sample error rate of 0.005% - this is very close to 100%,

# Now we use the whole training set to predict and compare to the validate set

modFit <- randomForest(classe ~., data = train)
prediction <- predict(modFit, validate, type = "class")


