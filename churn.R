# Predict Customer Churn with R

install.packages('plyr')
install.packages('corrplot')
install.packages('ggplot2')
install.packages('gridExtra')
install.packages('ggthemes')
install.packages('caret')
install.packages('MASS')
install.packages('randomForest')
install.packages('e1071')
install.packages('party')
install.packages('gpk')

library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(e1071)
library(party)
library(gpk)

churn <- read.csv('Telco-Customer-Churn.csv')
str(churn)

# customerID
# gender (female, male)
# SeniorCitizen (Whether the customer is a senior citizen or not (1, 0))
# Partner (Whether the customer has a partner or not (Yes, No))
# Dependents (Whether the customer has dependents or not (Yes, No))
# tenure (Number of months the customer has stayed with the company)
# PhoneService (Whether the customer has a phone service or not (Yes, No))
# MultipleLines (Whether the customer has multiple lines r not (Yes, No, No phone service))
# InternetService (Customer’s internet service provider (DSL, Fiber optic, No))
# OnlineSecurity (Whether the customer has online security or not (Yes, No, No internet service))
# OnlineBackup (Whether the customer has online backup or not (Yes, No, No internet service))
# DeviceProtection (Whether the customer has device protection or not (Yes, No, No internet service))
# TechSupport (Whether the customer has tech support or not (Yes, No, No internet service))
# streamingTV (Whether the customer has streaming TV or not (Yes, No, No internet service))
# streamingMovies (Whether the customer has streaming movies or not (Yes, No, No internet service))
# Contract (The contract term of the customer (Month-to-month, One year, Two year))
# PaperlessBilling (Whether the customer has paperless billing or not (Yes, No))
# PaymentMethod (The customer’s payment method (Electronic check, Mailed check, Bank transfer (automatic), Credit card (automatic))
# MonthlyCharges (The amount charged to the customer monthly—numeric)
# TotalCharges (The total amount charged to the customer—numeric)
# Churn ( Whether the customer churned or not (Yes or No))

# how many missing value?
sapply(churn, function(x) sum(is.na(x)))



# remove it
churn <- churn[complete.cases(churn), ]

# change “No internet service” to “No” for “OnlineSecurity”, “OnlineBackup”, “DeviceProtection”, “TechSupport”, “streamingTV”, “streamingMovies”.
cols_recode1 <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
  churn[,cols_recode1][,i] <- as.factor(mapvalues
                                        (churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}

# change “No phone service” to “No” for column “MultipleLines”
churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))

# check the range of tenure
min(churn$tenure); max(churn$tenure)

# we can organize the tenure value in 5 segment
group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}
churn$tenure_group <- sapply(churn$tenure,group_tenure)
str(churn)

churn$tenure_group <- as.factor(churn$tenure_group)
str(churn)

# Change the values in column “SeniorCitizen” from 0 or 1 to “No” or “Yes”.
churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))

# Remove the columns we do not need for the analysis.
churn$customerID <- NULL

churn$tenure <- NULL

# Check correlation index of numeric fields
numeric.var <- sapply(churn, is.numeric)
corr.matrix <- cor(churn[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

# Because the correlation of this two fields, we can remove "TotalCharges" 
churn$TotalCharges <- NULL

# Bar plots of categorical variables
p1 <- ggplot(churn, aes(x=gender)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p2 <- ggplot(churn, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p3 <- ggplot(churn, aes(x=Partner)) + ggtitle("Partner") + xlab("Partner") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p4 <- ggplot(churn, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p1, p2, p3, p4, ncol=2)

p5 <- ggplot(churn, aes(x=PhoneService)) + ggtitle("Phone Service") + xlab("Phone Service") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p6 <- ggplot(churn, aes(x=MultipleLines)) + ggtitle("Multiple Lines") + xlab("Multiple Lines") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p7 <- ggplot(churn, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p8 <- ggplot(churn, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p5, p6, p7, p8, ncol=2)

p9 <- ggplot(churn, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p10 <- ggplot(churn, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p11 <- ggplot(churn, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p12 <- ggplot(churn, aes(x=StreamingTV)) + ggtitle("Streaming TV") + xlab("Streaming TV") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p9, p10, p11, p12, ncol=2)

p13 <- ggplot(churn, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p14 <- ggplot(churn, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p15 <- ggplot(churn, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p16 <- ggplot(churn, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p17 <- ggplot(churn, aes(x=tenure_group)) + ggtitle("Tenure Group") + xlab("Tenure Group") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p13, p14, p15, p16, p17, ncol=2)


# All categorical variables has a good distribution, so we keep them.


### Model

# Split the data into training and testing sets
intrain<- createDataPartition(churn$Churn,p=0.7,list=FALSE)
set.seed(2018)
training<- churn[intrain,]
testing<- churn[-intrain,]

# Confirm the splitting is correct
dim(training); dim(testing)

# Fitting the Logistic Regression Model
LogModel <- glm(Churn ~ .,family=binomial(link="logit"),data=training)
print(summary(LogModel))

anova(LogModel, test="Chisq")

# Check Accurancy GLM
testing$Churn <- as.character(testing$Churn)
testing$Churn[testing$Churn=="No"] <- "0"
testing$Churn[testing$Churn=="Yes"] <- "1"
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))

# Confusion Matrix
print("Confusion Matrix for Logistic Regression"); table(testing$Churn, fitted.results > 0.5)

# Odds ratio
exp(cbind(OR=coef(LogModel), confint(LogModel)))



# Decision Tree
tree <- ctree(Churn~Contract+tenure_group+PaperlessBilling, training)
plot(tree)

# Predict
pred_tree <- predict(tree, testing)
print("Confusion Matrix for Decision Tree"); 
table(Predicted = pred_tree, Actual = testing$Churn)

# Check Accurancy Decision Tree
p1 <- predict(tree, training)
tab1 <- table(Predicted = p1, Actual = training$Churn)
tab2 <- table(Predicted = pred_tree, Actual = testing$Churn)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))


# Random Forest
rfModel <- randomForest(Churn ~., data = training)
print(rfModel)

# Confusion Matrix
pred_rf <- predict(rfModel, testing)
table(Predicted = pred_rf, Actual = testing$Churn)

# Accuracy
p1 <- predict(rfModel, training)
tab1 <- table(Predicted = p1, Actual = training$Churn)
tab2 <- table(Predicted = pred_rf, Actual = testing$Churn)
print(paste('Random Forest Accuracy',sum(diag(tab2))/sum(tab2)))

# Error Rate
plot(rfModel)

# Tune
t <- tuneRF(training[, -18], training[, 18], stepFactor = 0.5, plot = TRUE, ntreeTry = 100, trace = TRUE, improve = 0.05)

# Retrain
rfModel_new <- randomForest(Churn ~., data = training, ntree = 100, mtry = 2, importance = TRUE, proximity = TRUE)
print(rfModel_new)

# Confusion Matrix
pred_rf_new <- predict(rfModel_new, testing)
table(Predicted = pred_rf_new, Actual = testing$Churn)

# Accuracy
p1 <- predict(rfModel_new, training)
tab1 <- table(Predicted = p1, Actual = training$Churn)
tab2 <- table(Predicted = pred_rf_new, Actual = testing$Churn)
print(paste('New Random Forest Accuracy',sum(diag(tab2))/sum(tab2)))


# Random Forest Feature Importance
varImpPlot(rfModel_new, sort=T, n.var = 10, main = 'Top 10 Feature Importance')


# Features such as tenure_group, Contract, PaperlessBilling, MonthlyCharges and InternetService appear to play a role in customer churn.

# Exercise;

data(BANK)
