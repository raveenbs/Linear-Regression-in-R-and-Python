##############   Linear Regression   ##############

###################################################
# Step 1: Set the Working Directory
###################################################
setwd("~/raveen/lr")

###################################################
# Step 2: Reading data for linear regression
###################################################
# reading in the data
hotel_stays <- read.csv("Hotel Stays.csv")
summary(hotel_stays)

###################################################
# Step 3: Understand variables available
###################################################

# correlation for continuous variables
cor(hotel_stays[,-1])

# View correlation in table format
View(cor(hotel_stays[,-1]))

# Plot for pairwise variable
pairs(hotel_stays[,-1])

# As seen Weekday_stay_3_mon is most correlation
# Correlation and Causation are two difference concepts
# Two variables can be correlated but it might not be the cause for the output
# In this case we need to make a business decision

# Here we start a model with Stays_6_mon 

###################################################
# Step 4: Visual Analysis of relationship
###################################################
library(ggplot2)
# Examine the fit
par(mfrow=c(1,1))
ggplot(hotel_stays, aes(Weekday_stay_3_mon, Stays_12_mon_Y)) + geom_point() + geom_smooth(method = "lm", se=FALSE)
ggplot(hotel_stays, aes(Stays_6_mon, Stays_12_mon_Y)) + geom_point() + geom_smooth(method = "lm", se=FALSE)

###################################################
# Step 5: Generate the OLS model
###################################################
# Learn about this function 
?lm

lm_stays <- lm(Stays_12_mon_Y ~ ., data = subset(hotel_stays, select=-Customer_ID))
summary(lm_stays)

str(lm_stays)


###################################################
#Step 6: Stepwise Linear Regression
###################################################

# Methodology for analysing all variables
# This process lets us know the best variables to select for model

lm_stays <- lm(Stays_12_mon_Y ~ ., data = subset(hotel_stays, select=-Customer_ID))

# This is forward selection methodology
# you can explore backward selection and 
# run all models and select the best

library(olsrr)
ols_step_forward(lm_stays)
plot(ols_step_forward(lm_stays))

# As seen this shows the best variables are 
# Gold_card_flag
# Stays_6_mon
# Weekday_stay_3_mon
# Customer_Tenure

###################################################
# Step 7: Print and Visualize the Results
###################################################

par(mfrow=c(2,2))
plot(lm_stays)

# Validating the model performance with QQ Plot
library(car)
par(mfrow=c(1,1))
qqPlot(lm_stays)
