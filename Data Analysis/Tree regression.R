#------------------------------------------------------------------------------------------------------------------

# MACHINE LEARNING MODEL SELECTION: REGRESSION TREE - RANDOM FOREST - XGBOOST

# AIRBNB LISTINGS OCCUPANCY RATE FORECAST
# Objective: fitting a model to predict monthly occupancy (days) of Madrid's Airbnb listings.
# Target Variable: availability_30 (number of days in a  month each property is available for rent).
# Occupancy = 30 - availability_30
# Date:10/23/2017
#------------------------------------------------------------------------------------------------------------------

library(lattice)
library(ggplot2)
library(dplyr)
library(rpart.plot)
library(randomForest)
library(rpart)
library(caret)
library(dummies)
#------------------------------------------------------------------------------------------------------

# DECISION TREE. 

#------------------------------------------------------------------------------------------------------
# References:
# http://www-bcf.usc.edu/~gareth/ISL/ISLR%20First%20Printing.pdf

# Decision trees are extremely simple algorithms and lack proper prediction capacity, but they are easily
# interpretable. Considering we have a lot of predictors, a decision tree may help us understand much better than 
# linear regression the way predictors are associated with the target variable.

# On the other hand, decision trees have high variance, they produce different results when applied to
# different datasets, so predictions/classifications are not very reliable.

# Decision tree algorithm was developed by Leo Breiman in 1984, the original method is implemented
# in the library Rpart so we will use this one among all available.
#-----------------------------------------------------------------------------------------------------------------
# 1) Read and explore "RLM.csv".
reg<-read.csv('RLM.csv',stringsAsFactors = TRUE)
dim(reg)
glimpse(reg)

#................................................................................................................
# 2) Cross validation. Split the data set into Train and Test.
mod <- createDataPartition(reg$availability_30, p = 0.6,list = FALSE)
train_mod <- reg[mod,]
test_mpd <- reg[-mod,]

#..............................................................................................................
# 3) Model fitting with training. Anova method implemented for regression.
aib1 <- rpart(availability_30 ~.,train_mod,method = 'anova')
aib1
summary(aib1)
rpart.plot(aib1,fallen.leaves = FALSE)

#...............................................................................................................
# 4) Predictions
aib1_predct <- predict(aib1,test_mpd)

summary(aib1_predct)
summary(test_mpd$availability_30)

#................................................................................................................
# 5) Evaluation. Assesment of the model accuracy. 
# Metric: Root Mean Square Error (RMSE)
mean((aib1_predct-test_mpd$availability_30)^2) 
sqrt(tree_err)
RMSE
tree_err<-RMSE(test_mpd$availability_30,aib1_predct)

# According to test, the model predicts an availability of 7 days around the true availability.
# Considering the month has 30 days, the accuracy is far from good.

#................................................................................................................
# 6)Pruning
printcp(aib1)
# Given that xerror decreases in each split no pruning required to improve MSE

#.................................................................................................................
# 7) CONCLUSION
# Among all variables available in the dataset, the model only finds 4 important predictors.
# 1) The regression tree finds reviews_per_month the most signinficant variable. 
# 2) Again,like Linear regression, room type is the next important variable. Actually an entire home
#    with more than 3 reviews per month shows an estimated availbility of just 5.7 days on 1018 observations.
# 2) Price per person, emerges as other important varibale. If it is greater than 137 Eur availability skyrockets
#    to 18 days. Although it is deducted from just 147 observations, it may be signaling an estimated price 
#    ceiling for not so popular properties.
# 4) As far as ROOMS are concerned, neighbourhood becomes the 4th significant predictor.
#    If reviews per month are less than 0.5 neighbourhood play an important role in terms of occupancy.


#_-----------------------------------------------------------------------------------------------------------------

# BAGGING

#------------------------------------------------------------------------------------------------------------------
# 
# It is a technique that improves the predictive performance of Decision trees. It is a suitable algorithm 
# for the current analysis since it works fine with multiple variables. 
# Bagging is based on bootstrap concept (dividing the dataset in several training data sets). It builds X regression
# trees using X bootstrapped training datasets, and averages the resulting predictions.

# The difference between bagging and Random Forest relies on the way each split is done in the decision tree.
# In Random Forests, each time every tree splits, a random sample of predictors among all available in
# the dataset is selected and only ONE is applied. 
# In Bagging the tree is built as usual.

# In R, library randomForests also performs bagging:the difference is in the number of predictors. In bagging
# all predictors of the dataset must be taken into account. So argument mtry=27.

# 1) MODEL FITTING
# mtry: number of variables taken in each sample.
# ntree:Number of trees to grow (500 by default)
bag <- randomForest(availability_30 ~.,train_mod,mtry=26,ntree=500,na.action = na.omit)
importance(bag)
plot(bag)

# Mean squared residuals measures the error of the model in training and is 38.76
# As seen in the plot, from 300 trees on, it looks like the error keeps steady.

#..............................................................................................................
# 2) PREDICTIONS. TEST:
bag.pred <- predict(bag,test_mpd)

#..............................................................................................................
# 3) EVALUATION OF MODEL PERFORMANCE: Root Mean Squared Error.
mean((bag.pred - test_mpd$availability_30)^2)
bag_err<-RMSE(test_mpd$availability_30,bag.pred)


#-----------------------------------------------------------------------------------------------------------------

# RANDOM FOREST

#------------------------------------------------------------------------------------------------------------------

# 1) MODEL FITTING
rf <- randomForest(availability_30 ~.,train_mod,mtry=9,ntree=300,na.action = na.omit)
rf
table(importance(rf))
plot(rf)
varImpPlot(rf)

#.............................................................................................................
# 2) PREDICTIONS. TEST:

rf_pred <- predict(rf, test_mpd)

#.............................................................................................................
# 3) EVALUATION. RMSE
mean((rf_pred - test_mpd$availability_30)^2)# MSE
rf_err<-RMSE(test_mpd$availability_30,rf_pred)

#--------------------------------------------------------------------------------------------------------------
# APPENDIX
# RANDOM FOREST & Entire Homes.Rooms excluded

# a) SELECTION OF ONLY APPARTMENTS 
ap <-reg[!reg$room_type=='Private room',]
dim(ap)

# b) CROSS VALIDATION
modap <- createDataPartition(ap$availability_30, p = 0.6,list = FALSE)
train_modap <- ap[modap,]
test_modap <- ap[-modap,]

# c) MODEL FITTING
rf1 <- randomForest(availability_30 ~.,train_modap,mtry=9,ntree=400,na.action = na.omit)
rf1
importance(rf1)

# d) PREDICTIONS. TEST
rf1_pred <-predict(rf1,test_modap)

# e) EVALUATION.RMSE
MSE <-mean((rf1_pred - test_modap$availability_30)^2)
rf_app<-RMSE(test_modap$availability_30,rf1_pred)
# There is an improvement in the accuracy of the model when Private rooms are excluded. In any case, 5 days error is
# still high, and therefore the model is not very usuful as a forcasting tool.
#-------------------------------------------------------------------------------------------------------------------

# XGBOOST- Extreme Gradient Boosting

#---------------------------------------------------................................................................
# References:
# http://xgboost.readthedocs.io/en/latest//parameter.html
# https://github.com/dmlc/xgboost

# Boosting Parameters:
# eta:It controls the learning rate, i.e., the rate at which our model learns patterns in data. After every round, 
# it shrinks the feature weights to reach the best optimum.
# nrounds: maximum number of iterations (steps) required for gradient descent to converge.
# lambda: It enables Ridge Regression. Same as above
# alpha(default=1): It enables Lasso Regression. Same as above

#.................................................................................................................
# 1) INSTALLATION from github
install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")
library(xgboost)

#.................................................................................................................
# 2) ONE HOT ENCODING 
# Xgboost only works with numeric values, so we transform categorical features into numeric ones through one hot encoding
reg1 <- dummy.data.frame(reg, names=c("neighbourhood_group_cleansed",'room_type','is_location_exact','host_identity_verified',
                                     'cancellation_policy','host_is_superhost','instant_bookable'), sep="_")

#...................................................................................................................
# 3) TRAIN/TEST
# Split new dataset into train and test. 
xb <- createDataPartition(reg1$availability_30, p = 0.6,list = FALSE)
train_xb <- reg1[xb,]
test_xb <- reg1[-xb,]

#...................................................................................................................
# 4) CONVERSION OF DATAFRAME INTO MATRIX
# Following guidelines of xgboost, algorithm requires matrices :
dtrain <- xgb.DMatrix(as.matrix(train_xb[,names(train_xb)!='availability_30']),label = train_xb$availability_30)
dtest <- xgb.DMatrix(as.matrix(test_xb[,names(test_xb)!='availability_30']),label=test_xb$availability_30)

#..................................................................................................................
# 5) MODEL FITTING: 
watchlist <- list(train=dtrain, test=dtest)
bst <- xgb.train(data=dtrain,booster='gblinear', eta=1, nround = 2, lambda=0, lambda_bias=0, alpha=0, watchlist=watchlist, objective = "reg:linear")

#...................................................................................................................
# 4) PREDICTION:
pred <- predict(bst,dtest)
print(pred[1:10])

#....................................................................................................................
# 5) EVALUATION METRIC: Root mean square error.
xb_err<-RMSE(test_xb$availability_30,pred)
xb_err<-sqrt(mse)

#....................................................................................................................
# 6) VARIABLE IMPORTANCE
imp <- xgb.importance (feature_names = colnames(dtrain),model = bst)
xgb.plot.importance (importance_matrix = imp) 



#-------------------------------------------------------------------------------------------------------------------

# MODELS PERFORMANCE SUMMARY

#-------------------------------------------------------------------------------------------------------------------
Model <- c('Regression_Tree','Bagging','Random Forest','Xgboost')
RMSE_Error <-c(tree_err,bag_err,rf_err,xb_err)
suma<-as.data.frame(cbind(model,RMSE_Error))
suma

# Random Forest turns out to be the most accurate model. 
# Surprisingly, xgboost performance is worst than bagging and almost equal to the regression tree.
# All models performance is poor.   
