
#....................................................................

# AIRBNB LINEAR REGRESSION ANALYSIS. 
# MODEL FOR AVAILABILITY PREDICTION

#....................................................................

library(caret)
library(car)
#..............................................................................................................

# DATA CLEANING 
reg <- read.csv('model.csv',stringsAsFactors = TRUE)


# Remove dummy varibales created in pandas. 'One hot encoding' is automatic in R for two levels.
reg$X <- NULL
reg$YES_location_exact <-NULL
reg$NO_location_exact <-NULL
reg$YES_instant_book <- NULL
reg$NO_instant_book <-NULL
reg$YES_superhost <-NULL
reg$NO_superhost <-NULL
reg$Cancel_policy_STRICT <-NULL
reg$Cancel_policy_MODERATE <-NULL
reg$Cancel_policy_FLEXIBLE <-NULL
reg$host_identity_verified.1 <- NULL
reg$Host_identity_NOTverified <-NULL
reg$Private.room <-NULL
reg$Entire.home.apt <-NULL
reg$Estimated_income_month <- NULL

reg$room_type <- as.factor(reg$room_type)
table(reg$availability_30)
class(reg$room_type)


# Visualization of relationships among predictors

pairs(reg[,1:10])
cor(reg, use = "everything", method = "pearson")

#....................................................................................................................
# CROSS VALIDATION: Training/Test
# Creation of train data set
dp <- createDataPartition(reg$availability_30, p = 0.7,list = FALSE)
trainReg <- reg[dp,]
testReg <- reg[-dp,]
str(trainReg)

#.....................................................................................................................
# VARIBALE  SELECTION FOR THE MODEL

train <-lm(availability_30 ~.,trainReg)
summary(train)

# Steps method for variable selection
step(train, direction = 'backward')

# Fitting a model
# According to AIC the best model is:

disp<-lm(availability_30 ~ is_location_exact + zipcode + bathrooms + bedrooms + 
           beds + maximum_nights + price + Price_Person_Night + 
           room_type + security_deposit + cleaning_fee + number_of_reviews + 
           cancellation_policy + reviews_per_month + review_scores_rating + 
           review_scores_accuracy + review_scores_value + extra_people + 
           guests_included + accommodates + host_is_superhost + instant_bookable,trainReg)
summary(disp)

# Other models removing non significant variables

disp2 <- lm(availability_30 ~ is_location_exact + bathrooms + 
              beds + maximum_nights + Price_Person_Night + 
              room_type + security_deposit +  number_of_reviews + 
              cancellation_policy + reviews_per_month + review_scores_rating + 
              review_scores_value + extra_people + guests_included + accommodates + 
              host_is_superhost + instant_bookable,trainReg)
summary(disp2)
plot(disp2)

disp3 <- lm(availability_30 ~ is_location_exact + host_identity_verified + bedrooms+ bathrooms + price +Price_Person_Night + 
              minimum_nights + room_type + security_deposit +  number_of_reviews + reviews_per_month +
              cancellation_policy + review_scores_rating + review_scores_value +extra_people + 
              guests_included + accommodates + host_is_superhost + instant_bookable,trainReg)
summary(disp3)
plot(disp3)

disp4 <-  lm(availability_30 ~ is_location_exact + host_identity_verified +Price_Person_Night + 
               minimum_nights+room_type + security_deposit +  number_of_reviews + reviews_per_month +
               cancellation_policy + review_scores_rating + review_scores_value +extra_people + guests_included+
               host_is_superhost + instant_bookable,trainReg)
summary(disp4)
plot(disp4)
# POssible transformations in order to fix heterocedasticity and non linear relationship:
# a) POlynomial regression, lm cannot convert to a quadratic form due to the number of predictors.
# b) Logaritmic, response variable availability_30 has many zeroes, log(0) = inf. 

#.....................................................................................................................

## VARIANCE ANALYSIS
anova(disp)
anova(disp2)
anova(disp4)

# disp4 selected as best model
#.................................................................................................................

## MULTICOLLINEARITY
vif(disp4)
cor(disp4, method = 'pearson')
# No evidence of collinearity, all VIF values between 1 and 2.
#....................................................................................................................

##  MODEL ANALYSIS. 
# Residuals analysis. Linear regression hypothesis check:
# 1) Linear: E(r) = 0. Residuals vs fitted plot shows there is a slight 'u' shape pattern and thus non-linearity of the 
#    response-predictor relationships.
plot(disp4)

# 2) Equal Variance: residuals vs fitted plot shows lack of homocedasticity
plot(disp4)

# 3) Normal distribution: ok, residuals follow a Normal distribution
hist(disp4$residuals)

# 4) Cov(e) = 0. Resduals correlation check  through Durbin-Watson test.
# Given that  p-value = 0.004 we can reject Ho and therefore infer that residuals may be correlated.
durbin.watson(disp4)

#.........................................................................................................

# VALIDATION. IMLEMENTATION OF disp4 MODEL on Test data set

disp4t <-lm(availability_30 ~ is_location_exact + host_identity_verified +Price_Person_Night + 
              minimum_nights+room_type + security_deposit +  number_of_reviews + reviews_per_month +
              cancellation_policy + review_scores_rating + review_scores_value +extra_people + guests_included+
              host_is_superhost + instant_bookable,testReg)
summary(disp4t)

# disp4 model performance decreases with new data (TEst), Some variables turn up as non significant and the R2 adjusted
# is lower than in training set. 
# Hence, the model is poor for future predictions, however it shows that the combination of several variables affect
# the availability(30days) of properties listed in the airbnb website. 
# There is a relationship between response variable and certain predictors. 













