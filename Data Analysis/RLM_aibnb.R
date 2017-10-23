
#..............................................................................................................
# TFM 
# MULTIPLE LINEAR REGRESSION ANALYSIS. 

# Objective: Explain/Predict occupancy rate of Madrid's Airbnb listings. 
# Method: Linear Regression
# Target Variable: Availability_30 (number of days in a  month each property is available for rent).
# Date: 10/20/2017
#..............................................................................................................

library(lattice)
library(caret)
library(car)
library(dplyr)
library(ggplot2)
library(corrplot)
#..............................................................................................................

##### 1) READ, EXPLORE, VISUALIZE "RLM.csv". File, cleaned and ready to be analized.  
reg <- read.csv('RLM.csv',stringsAsFactors = TRUE)
glimpse(reg)
summary(reg)

table(reg$availability_30)
boxplot(reg$availability_30)
hist(reg$availability_30)

# Select numeric features and check correlations
regcor <-reg[c('availability_30','bathrooms','beds','minimum_nights','Price_Person_Night','security_deposit','number_of_reviews',
               'reviews_per_month','cleaning_fee','extra_people','accommodates','review_scores_rating')]

corrplot.mixed(cor(regcor), lower="circle", upper="color", 
               tl.pos="lt", diag="n", order="hclust", hclust.method="complete")

#.....................................................................................................................
#### 3) FEATURE SELECTION 
train <-lm(availability_30 ~.,reg)
summary(train)

## SIMPLE LINEAR REGRESSION - PREDICTORS 
a<-lm(availability_30 ~ neighbourhood_group_cleansed,reg)
summary(a)

b<-lm(availability_30 ~ bathrooms,reg)
summary(b)

c<-lm(availability_30 ~ bedrooms,reg)
summary(b)

d<-lm(availability_30 ~ beds,reg)
summary(d)

e<-lm(availability_30 ~ guests_included,reg)
summary(e)

g<-lm(availability_30 ~ security_deposit,reg)
summary(g)

h<-lm(availability_30 ~ cancellation_policy,reg)
summary(h)

i<-lm(availability_30 ~ review_scores_accuracy,reg)
summary(i)

j<-lm(availability_30 ~ cleaning_fee,reg)
summary(j)

k<-lm(availability_30 ~ instant_bookable,reg)
 summary(k)
 
# With 27 predictors there are 2^27 combinations to consider. We cannot do it manually so we apply
# "step" function.
step(train, direction = 'backward')

# AIC=n*log(RSS/n)+2k,
# According to the Akaike Information Criteria (AIC) the best model is:
disp <- lm(availability_30 ~ neighbourhood_group_cleansed + host_identity_verified + 
              bathrooms + bedrooms + beds + minimum_nights + price + Price_Person_Night + 
              room_type + security_deposit + number_of_reviews + cancellation_policy + 
              reviews_per_month + review_scores_rating + review_scores_accuracy + 
              review_scores_value + extra_people + accommodates + host_is_superhost + 
              instant_bookable,reg)
summary(disp)

# Review_scores_rating has a p-value = 0.34, so we can infer there is no linear relationship with 
# the response variable; we remove it:
disp <- lm(availability_30 ~ neighbourhood_group_cleansed + is_location_exact + 
             host_identity_verified + bathrooms + bedrooms + beds + minimum_nights + 
             price + Price_Person_Night + room_type + security_deposit + 
             number_of_reviews + cancellation_policy + reviews_per_month + 
             review_scores_accuracy + review_scores_value + extra_people + 
             accommodates + host_is_superhost + instant_bookable,reg)

summary(disp)
plot(disp)
plot(disp$residuals)
# Lets try other models removing neighbourhood and  non significant variables:
disp2 <- lm(availability_30 ~ is_location_exact + bathrooms + 
              beds + maximum_nights + Price_Person_Night + 
              room_type + security_deposit +  number_of_reviews + 
              cancellation_policy + reviews_per_month + review_scores_rating + 
              review_scores_value + extra_people + guests_included + accommodates + 
              host_is_superhost + instant_bookable,reg)
summary(disp2)
plot(disp2)

disp3 <- lm(availability_30 ~ is_location_exact + host_identity_verified + bedrooms+ bathrooms + price +Price_Person_Night + 
              minimum_nights + room_type + security_deposit +  number_of_reviews + reviews_per_month +
              cancellation_policy + review_scores_rating + review_scores_value +extra_people + 
              guests_included + accommodates + host_is_superhost + instant_bookable,reg)
summary(disp3)
plot(disp3)

disp4 <-  lm(availability_30 ~ neighbourhood_group_cleansed + is_location_exact + host_identity_verified +Price_Person_Night + 
               minimum_nights+room_type + security_deposit +  number_of_reviews + reviews_per_month +
               cancellation_policy + review_scores_rating + review_scores_value +extra_people + guests_included+
               host_is_superhost + instant_bookable,reg)
summary(disp4)
plot(disp4)

# ANOVA-MODEL COMPARISON
anova(disp,disp2)
anova(disp,disp4)

# Although there are not remarkable differences, none of them beats disp. It looks like less predictors affect the performance of the alternative models.
# Model disp has the highest R² adjusted, 0.369, better than the other posibilities. So we select disp. 

#.................................................................................................................
## 4) MULTICOLLINEARITY
# Variance Inflaction Factor (vif). VIF = 1/1-R²
vif(disp)

# "Review_scores_acuracy" and "Review_scores_value" are correlated; Given that R_S_accuracy was not very
# significant we remove it.
# Price is also showing value above 5, obviously is correlated with price_per_person so we remove it as well.
disp <- lm(availability_30 ~ neighbourhood_group_cleansed + is_location_exact + 
             host_identity_verified + bathrooms + bedrooms + beds + minimum_nights + 
             Price_Person_Night + room_type + security_deposit + 
             number_of_reviews + cancellation_policy + reviews_per_month + 
             review_scores_value + extra_people + 
             accommodates + host_is_superhost + instant_bookable,reg)
vif(disp)

# All values under 5. No multicollinearity

#........................................................................................................
## 4) VARIANCE ANALYSIS.
# Confirmation of predictors significance:
Anova(disp)

# Variables "beds","bedrooms" and "accommodates" are not significant. Deleted.
# So finallly the model is as follows:
disp <- lm(availability_30 ~ neighbourhood_group_cleansed + is_location_exact + 
             host_identity_verified + bathrooms + minimum_nights + 
             Price_Person_Night + room_type + security_deposit + 
             number_of_reviews + cancellation_policy + reviews_per_month + 
             review_scores_value + extra_people + host_is_superhost + instant_bookable,reg)

summary(disp)

# After removing these predictors, R² adjusted has slightly decreased to 0.368. Such a low difference is a 
# confirmation that those variables were redundant.

#....................................................................................................................
##  5) RESIDUALS ANALYSIS. 
#    Linear regression hypothesis check:
# 1) Linear: E(r) = 0. Residuals vs fitted plot shows there is a slight 'u' shape pattern and thus non-linearity of the 
#    response-predictor relationships.
plot(disp)

# 2) Equal Variance: residuals vs fitted plot shows lack of homocedasticity.
plot(dispb)

# 3) Normal distribution: ok, residuals follow a Normal distribution.
hist(dispb$residuals)

# 4) Cov(e) = 0. Resduals correlation check  through Durbin-Watson test.
durbinWatsonTest(disp)

# P-value = 0.33 so we fail to accept Ho and therefore infer that residuals are correlated.
# Possible transformations in order to fix heterocedasticity and non linear relationship:
# a) Polynomial regression, lm cannot convert to a quadratic form due to the number of predictors.
# b) Logaritmic, response variable availability_30 has many zeroes, log(0) = inf. 

#.........................................................................................................
#### 6) MODEL ANALYSIS: CONCLUSIONS
# Evaluation:
# The final model has a R² adjusted = 0.368. This is to say that only 36% of the total variance of
# an Airbnb listing availability is explained by our regression model (disp).Then the prediction capacity or the 
# model is poor.
# However, it provides some useful information for landlords like the variables that may affect the availability:
# R summary function displays the results of a hypothesis contrast with F statistic where null hypothesis states that
# there is no linear relationship between availability_30 and the predictors (b1=b2=...bi=0). A p-value < 2.2e-16 allows us to
# reject Ho, or allows us to predict with more than 99% level of confidence that predictors are associated with the 
# availability of listings in Madrid. 
#
# The most interesting findings of the analysis regards to neighbourhoods and room type:
# According to the model Villa de Vallecas,Fuencarral - el pardo,or Barajas are neighbourhoods that increase the 
# availability of o property in Airbnb. These areas are a handicap for maximizing occupancy.
# On the other hand, Centro and Salamanca appear as drivers of higher occupancy rates.
# 
# Listings of Private rooms(room_type) increase the availability of properties in aproximately 5 days (model residuals 
# did not respect hyphotesis so coefficients are biased). 

# Surprisingly "cancelation policy strict and moderate", instead of flexible, reduce the availability.  

#.............................................................................................................
# 7) RECOMMENDATION FOR INVESTORS/LANDLORDS TO MAXIMIZE OCCUPANCY RATE ON AIRBNB PLATFORM.

# 1) Avoid the following districts: Vallecas, Fuerncarral, Barajas, Carabanchel.
# 2) If possible, overweight districts Centro and Salamanca.
# 3) List Entire homes or appartments. Ignore rooms.
# 4) Set the following parameters:
#     - Location exact: yes
#     - Verify host id: yes
#     - instant bookable: yes
#     - Cancellation Policy: moderate
# 5) Pay special attention to:
#     - Price: bear in mind that guests pay more attention to price per person rather than to overall price.
#     - Minimum nights. Greed is not recommended

#.............................................................................................................
