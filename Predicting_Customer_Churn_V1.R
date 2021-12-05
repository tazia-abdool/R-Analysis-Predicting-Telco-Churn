# Predicting Customer Churn V1

#Libraries
library(ggplot2)
library(caret)
library(forecast)
library(rpart)
library(rpart.plot)
library(pROC)
library(reshape2)
library(plyr)
library(randomForest)
library(gridExtra)

################

#Read data
my_raw_data.df <- read.csv("Customer-Churn.csv", stringsAsFactors = TRUE)

#Examine data
head(my_raw_data.df)
str(my_raw_data.df)
View(my_raw_data.df)

#checking for missing data & investigating
sapply(my_raw_data.df, function(x) sum(is.na(x)))  # this shows the number of blanks in each column

#I can see that there are 11 blanks in the column "Total Charges". Lets pull out those rows and look at them
my_raw_data.df[is.na(my_raw_data.df$TotalCharges),]

#I"m considering removing these rows for simplicity but what percentage of my total dataset would that be?
sum(is.na(my_raw_data.df$TotalCharges))/nrow(my_raw_data.df)

#It's 0.16% of my total dataset. Seems fine to remove.

#remove the cases where data is incomplete
mydata.df <- my_raw_data.df[complete.cases(my_raw_data.df), ] 

#rename the headers to make it easier to understand
# Senior Citizen is data type Int , therefore we need to use as.factor
mydata.df$SeniorCitizen <- as.factor(mapvalues(mydata.df$SeniorCitizen,
                                          from=c("0","1"),
                                          to=c("No", "Yes")))

#I Notice that the column "Multiple Lines" has three types of entries "yes", "no"and "no phone service".
# "no phone service" should just be changed to "no"

mydata.df$MultipleLines <- revalue(mydata.df$MultipleLines,c("No phone service"= "No"))

#Columns 10 -15 contain the phrase "No internet service" instead of "No". 
#This needs to be cleaned up

mydata.df$OnlineSecurity <- revalue(mydata.df$OnlineSecurity,c("No internet service"= "No"))
mydata.df$OnlineBackup <- revalue(mydata.df$OnlineBackup,c("No internet service"= "No"))
mydata.df$DeviceProtection <- revalue(mydata.df$DeviceProtection,c("No internet service"= "No"))
mydata.df$TechSupport <- revalue(mydata.df$TechSupport,c("No internet service"= "No"))
mydata.df$StreamingTV <- revalue(mydata.df$StreamingTV,c("No internet service"= "No"))
mydata.df$StreamingMovies <- revalue(mydata.df$StreamingMovies,c("No internet service"= "No"))

str(mydata.df) # checking to see if all the anomalies are fixed i.e expecting to see Factor with 2 levels 

#customer ID column is not needed
mydata.df$customerID <- NULL 

# To categorize the Tenure column based on the range groups available.   

# Minimum tenure is 1 month and maximum tenure is 72 months:       
min(mydata.df$tenure); max(mydata.df$tenure)

# we can group them into five tenure groups: “0–12 Month”, “12–24 Months”, “24–48 Months”, “48–60 Months”, “> 60 Months”
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

mydata.df$tenure<- sapply(mydata.df$tenure,group_tenure)

View(mydata.df)
       
       
#Exploratory Data Analysis

#DEMOGRAPHICS

#Gender plot
plot1 <- ggplot(data=mydata.df)+geom_bar(aes(x=gender),stat="count", width=0.7, fill="steelblue")+ 
  labs(x= "Gender", y="Count", title = "Gender Distribution")+ theme_light() +theme(plot.title = element_text(hjust = 0.5))


#Senior Citizen plot
plot2 <- ggplot(data=mydata.df)+geom_bar(aes(x=SeniorCitizen),stat="count", width=0.7, fill="steelblue")+ 
  labs(x= "Senior Citizen", y="Count", title = "Senior Citizen Distribution")+ theme_light() +theme(plot.title = element_text(hjust = 0.5))

#Partner plot
plot3 <- ggplot(data=mydata.df)+geom_bar(aes(x=Partner),stat="count", width=0.7, fill="steelblue")+ 
  labs(x= "Partner", y="Count", title = "Partner Distribution")+ theme_light()+theme(plot.title = element_text(hjust = 0.5))

#Dependents plot
plot4 <- ggplot(data=mydata.df)+geom_bar(aes(x=Dependents),stat="count", width=0.7, fill="steelblue")+ 
  labs(x= "Dependents", y="Count", title = "Dependent Distribution")+ theme_light()+theme(plot.title = element_text(hjust = 0.5))

#Plot demographic data in 2x2 grid
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

# ANALYSIS OF SERVICES 

#Phone service plot
plot5 <- ggplot(data=mydata.df)+geom_bar(aes(x=PhoneService),stat="count", width=0.7, fill="steelblue")+ 
  labs(x= "Phone Service", y="Count", title = "Phone Service Distribution")+ theme_light()+theme(plot.title = element_text(hjust = 0.5))


#Multiple phone lines plot
plot6 <- ggplot(data=mydata.df)+geom_bar(aes(x=MultipleLines),stat="count", width=0.7, fill="steelblue")+ 
  labs(x= "Multiple Lines", y="Count", title = "Multiple Lines Distribution")+ theme_light()+theme(plot.title = element_text(hjust = 0.5))
  
#Internet service plot
plot7 <- ggplot(data=mydata.df)+geom_bar(aes(x=InternetService),stat="count", width=0.7, fill="steelblue")+ 
  labs(x= "Internet Service", y="Count", title = "Internet Service Distribution")+ theme_light()+theme(plot.title = element_text(hjust = 0.5))

#Online security service plot
plot8 <- ggplot(data=mydata.df)+geom_bar(aes(x=OnlineSecurity),stat="count", width=0.7, fill="steelblue")+ 
  labs(x= "Online Security", y="Count", title = "Online Security Distribution")+ theme_light()+theme(plot.title = element_text(hjust = 0.5))

#Online backup service plot
plot9 <- ggplot(data=mydata.df)+geom_bar(aes(x=OnlineBackup),stat="count", width=0.7, fill="steelblue")+ 
  labs(x= "Online Backup", y="Count", title = "Online Backup Distribution")+ theme_light()+theme(plot.title = element_text(hjust = 0.5))

#Device Protection service plot
plot10 <- ggplot(data=mydata.df)+geom_bar(aes(x=DeviceProtection),stat="count", width=0.7, fill="steelblue")+ 
  labs(x= "Device Protection", y="Count", title = "Device Protection Distribution")+ theme_light()+theme(plot.title = element_text(hjust = 0.5))

#Tech Support service plot
plot11 <- ggplot(data=mydata.df)+geom_bar(aes(x=TechSupport),stat="count", width=0.7, fill="steelblue")+ 
labs(x= "Tech Support", y="Count", title = "Tech Support Distribution")+ theme_light()+theme(plot.title = element_text(hjust = 0.5))

#Streaming TV service plot
plot12 <- ggplot(data=mydata.df)+geom_bar(aes(x=StreamingTV),stat="count", width=0.7, fill="steelblue")+ 
  labs(x= "Streaming TV", y="Count", title = "Streaming TV Distribution")+ theme_light()+theme(plot.title = element_text(hjust = 0.5))


#Streaming Movies service plot
plot13 <- ggplot(data=mydata.df)+geom_bar(aes(x=StreamingMovies),stat="count", width=0.7, fill="steelblue")+ 
  labs(x= "Streaming Movies", y="Count", title = "Streaming Movies Distribution")+ theme_light()+theme(plot.title = element_text(hjust = 0.5))

#Plot service data in a 3x3 grid
grid.arrange(plot5, plot6, plot7,
             plot8, plot9, plot10,
             plot11, plot12, plot13,
             ncol=3)


# ACCOUNT SETTINGS

#Contract status plot
plot14 <- ggplot(data=mydata.df)+geom_bar(aes(x=Contract),stat="count", width=0.7, fill="steelblue")+ 
  labs(x= "Contract Status", y="Count", title = "Contract Status Distribution")+ theme_light()+theme(plot.title = element_text(hjust = 0.5))

#Paperless billing plot

plot15 <- ggplot(data=mydata.df)+geom_bar(aes(x=PaperlessBilling),stat="count", width=0.7, fill="steelblue")+ 
  labs(x= "Paperless Billing", y="Count", title = "Paperless Billing Distribution")+ theme_light()+theme(plot.title = element_text(hjust = 0.5))

#Payment method plot
plot16 <- ggplot(data=mydata.df)+geom_bar(aes(x=PaymentMethod),stat="count", width=0.7, fill="steelblue")+ 
  labs(x= "Payment Method", y="Count", title = "Payment Method Distribution")+ theme_light()+theme(plot.title = element_text(hjust = 0.5))


#Plot account settings
grid.arrange(plot14, plot15, plot16, ncol=1)

#OTHER DATA

#Tenure histogram
plot17 <- ggplot(mydata.df, aes(x = tenure)) + geom_histogram(binwidth = 1, fill="steelblue") + 
  labs(x = "Months", title = "Tenure Distribtion") +theme_light()+theme(plot.title = element_text(hjust = 0.5))

#Monthly charges histogram
plot18 <- ggplot(mydata.df, aes(x = MonthlyCharges )) + geom_histogram(binwidth = 10, fill="steelblue") + 
  labs(x = "Dollars (Bin width = 10) ", title = "Monthly Charges Distribtion") +theme_light()+theme(plot.title = element_text(hjust = 0.5))

#Total charges histogram
plot19 <- ggplot(mydata.df, aes(x = TotalCharges )) + geom_histogram(binwidth = 100, fill="steelblue") + 
  labs(x = "Dollars (Bin width = 100) ", title = "Total Charges Distribtion") +theme_light()+theme(plot.title = element_text(hjust = 0.5))


#Plot other data within a grid
grid.arrange(plot17, plot18, plot19, ncol=1)

plot20 <- ggplot(data=mydata.df)+geom_bar(aes(x=Churn),stat="count", width=0.7, fill="steelblue")+ 
  labs(x= "Churn", y="Count", title = "Churn Distribution")+ theme_light()+theme(plot.title = element_text(hjust = 0.5))

plot20




#MODELING

#Split into training & validation data
set.seed(500)
train.index <- sample(1:nrow(mydata.df), nrow(mydata.df)*0.8) 

# Build training and validation set by indexing
train.df <- mydata.df[train.index, ]
valid.df <- mydata.df[-train.index, ]

#DECISION TREE

tree_fit <- rpart(Churn ~., data = train.df, method="class")
rpart.plot(tree_fit, extra = 1)

#Confusion Matrix for Decision Tree

tree.point.pred <- predict(tree_fit, valid.df, type = "class")
confusionMatrix(tree.point.pred, factor(valid.df$Churn))



# LOGISTIC REGRESSION

logit.reg <- glm(Churn ~., data = train.df, family= "binomial")
summary(logit.reg)

# Confusion Matrix for Logistic Regression

logit.reg.pred <- predict(logit.reg, valid.df,  type = "response")

# Choose cutoff value and evaluate classification performance
pred <- factor(ifelse(logit.reg.pred > 0.5, 'Yes', 'No'))

confusionMatrix(factor(pred), factor(valid.df$Churn), positive = "Yes")

# Find best Cutoff

r_logit_reg <- roc(valid.df$Churn, logit.reg.pred)

plot.roc(r_logit_reg)

# find the best threshold, with a default best method "youden"
coords(r, x = "best")

pred2 <- factor(ifelse(logit.reg.pred > 0.24, 'Yes', 'No'))

confusionMatrix(factor(pred2), factor(valid.df$Churn), positive = "Yes")



#RANDOM FOREST MODEL

#Initial Model
rfModel <- randomForest(Churn ~., data = training)
rfModel
       
#Random Forest Prediction and Confusion Matrix
pred_rf <- predict(rfModel, testing)
#caret::confusionMatrix(pred_rf, testing$Churn)
table(Predicted = pred_rf, Actual = testing$Churn)
#To calculate the error rate
plot(rfModel)
 
#Set control parameters for random forest model selection
ctrl <- trainControl(method = "cv", number=5, 
                     classProbs = TRUE, summaryFunction = twoClassSummary)

#Exploratory random forest model selection
rf_fit1 <- train(Churn ~., data = train.df,
                 method = "rf",
                 ntree = 75,
                 tuneLength = 5,
                 metric = "ROC",
                 trControl = ctrl)
rf_fit1


#Run optimal model
rf_fit2 <- randomForest(as.factor(Churn) ~., data = train.df, 
                        ntree = 75, mtry = 2, 
                        importance = TRUE, proximity = TRUE)

#Display variable importance from random tree
varImpPlot(rf_fit2, sort=T, n.var = 10, 
           main = 'Top 10 important variables')

#confusion matrix for rf2
rf_pred1 <- predict(rf_fit2, dtest)
table(Predicted = rf_pred1, Actual = dtest$Churn)

#calculate Accuracy of rf2
rf_pred2 <- predict(rf_fit2, dtrain)
rf_tab1 <- table(Predicted = rf_pred2, Actual = dtrain$Churn)
rf_tab2 <- table(Predicted = rf_pred1, Actual = dtest$Churn)
rf_acc <- sum(diag(rf_tab2))/sum(rf_tab2)
rf_acc


# Visualizing the Models

p21 <- ggplot(datc, aes(x = Contract, fill = Churn)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) +
  labs(title="Churn rate by contract status")

p21


p22 <- ggplot(datc, aes(x = InternetService, fill = Churn)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) +
  labs(title="Churn rate by internet service status")

p22


p23 <- ggplot(datc, aes(x = tenure, fill = Churn)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Months",
       title = "Churn rate by tenure")
p23


p24 <- ggplot(datc, aes(x = TotalCharges, fill = Churn)) +
  geom_histogram(binwidth = 100) +
  labs(x = "Dollars (binwidth=100)",
       title = "Churn rate by tenure")
p24
