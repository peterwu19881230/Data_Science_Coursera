---
title: "Machine learning course project"
author: "Peter Wu"
date: "8/19/2018"
output: 
  html_document:
    keep_md: true
---


```r
pre_train = read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")[, 
    -1]
pre_test = read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")[, 
    -1]


# Decide which variables to use for prediction

# Filter the matrix, data frame or data table so that all rows/columns with
# NA/NAN/NULL/'' are removed
filterTable = function(table) {
    if (!(class(table) %in% c("matrix", "data.frame", "data.table"))) 
        stop("Input is not a matrix, data frame or data table")
    
    
    colToBeRemoved = apply(pre_train, 2, FUN = function(col) {
        ifelse(sum(is.na(col) + is.nan(col) + is.null(col) + sapply(col, FUN = function(point) {
            identical(as.character(point), "")
        })) >= 1, T, F)
    })
    # Note: I don't have to do the same thing for rows because all the undesired
    # values are removed after doing columns
    
    new_table = table[, !colToBeRemoved]
    return(new_table)
}


train = filterTable(pre_train)
train = train[, -(1:4)]  #I think using the first 4 columns don't make lots of sense, so I removed them
test = pre_test[, names(train)[-55]]  #There is no classe  column in the test set (which is in col 59 of train)
levels(test$new_window) = c("no", "yes")  #add the level 'yes' because in new_window variable everying is 'no' (R will complain in treebag if I don't add this)

# response variable: classe

unique(train$classe)  # A B C D E
```

```
## [1] A B C D E
## Levels: A B C D E
```

```r
# Note: there is no classe variable in the test data

# Have to do a cross-validation (usually 10-fold) A clear vedio of how
# cross-validation works: https://www.youtube.com/watch?v=hihuMBCuSlU Ref of
# how to do cross validation:
# http://www.rpubs.com/StephanieStallworth/269560

library(caret)
library(AppliedPredictiveModeling)
library(e1071)


# Run algorithms using 10-fold cross validation
control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"


# rpart method set.seed(101)
# modFit_rpart=train(classe~.,method=c('rpart'),data=train,metric=metric,trControl=control)
# print(modFit_rpart) # Only 53.6% accuracy


# Bagging: bootstrap aggregating: 1. Resample cases and recalculate
# predictions 2. Average or majority vote Built in method: bagEarth,
# treebag, bagFDA set.seed(101) treebag=bag(train[,-55],train$classe,B=10,
# bagControl = bagControl(fit=ctreeBag$fit, predict=ctreeBag$pred,
# aggregate=ctreeBag$aggregate)) #B: no. of replicates (subsamples) from the
# dataset

# summary(treebag) #98.6% accuracy predict(treebag,test)


# Random forests: 1. Bootstrap samples 2. At each split, bootstrap variables
# 3. Grow multiple trees and vote Note: It has overfitting problem, so
# cross-validation is important
# modFit_rf=train(classe~.,method='rf',prox=T,data=train,metric=metric,trControl=control)
# #prox=T will give some more info (!) R complains about not having enough
# memory


# Boosting: 1. Take lots of (possibly weak predictors) 2. Weigh them and add
# them up 3. Get a stronger predictor example: gbm: boosting with trees
# set.seed(101)
# modFit_boost=train(classe~.,method='gbm',data=train,verbose=F,metric=metric,trControl=control)
# #Takes about 20 min print(modFit_boost) #98.8% accuracy
# predict(modFit_boost,test) # I chose this one as the model for the quiz
set.seed(101)
modFit_boost = train(classe ~ new_window + num_window, method = "gbm", data = train, 
    verbose = F, metric = metric, trControl = control)
print(modFit_boost)  #99.9% accuracy if using only the first 2 variables. 
```

```
## Stochastic Gradient Boosting 
## 
## 19622 samples
##     2 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 17660, 17659, 17659, 17660, 17660, 17661, ... 
## Resampling results across tuning parameters:
## 
##   interaction.depth  n.trees  Accuracy   Kappa    
##   1                   50      0.5425057  0.4086666
##   1                  100      0.5858742  0.4659495
##   1                  150      0.6213456  0.5093299
##   2                   50      0.9015407  0.8750333
##   2                  100      0.9704933  0.9627190
##   2                  150      0.9957191  0.9945870
##   3                   50      0.9520449  0.9392006
##   3                  100      0.9961775  0.9951641
##   3                  150      0.9989299  0.9986465
## 
## Tuning parameter 'shrinkage' was held constant at a value of 0.1
## 
## Tuning parameter 'n.minobsinnode' was held constant at a value of 10
## Accuracy was used to select the optimal model using the largest value.
## The final values used for the model were n.trees = 150,
##  interaction.depth = 3, shrinkage = 0.1 and n.minobsinnode = 10.
```

```r
predict(modFit_boost, test)  # I chose this one as the model for the quiz
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

```r
# Model based prediction set.seed(101)
# modFit_lda=train(classe~.,method='lda',data=train,metric=metric,trControl=control)
# modFit_lda #Only 71% accuracy


# modFit_nb=train(classe~.,method='nb',data=train,metric=metric,trControl=control)
# #nb: naive based # Takes about 6 min (!) Error: Numerical 0 probability
# for all classes with observation 1,2,3,4,5.....
```



