
# Read data

raw.data <- read.csv('./data/train.csv',
                 header = T,
                 sep = ',',
                 na.strings = c("", " ", NA),
                 stringsAsFactors = T)

# Remove Interest_Rate, EMI, Loan_Amount, Loan_Period from the data set as they have 
# high percentage of missing data

data <- raw.data[,-c(17:20)]

# Create a dataset only with clean data, ie. columns having no missing data
# ID - 1
# Gender - 2
# Lead_Creation_Date -4
# Monthly_Income - 10
# Contacted - 13
# Source - 14
# Source_Category -15
# Var1 - 21
# Approved - 22

# DOB - 3
# Existing_EMI - 16
data <-data[,2:18]

# Missing data analysis

library(mice)
library(VIM)

# missind data pattern

md.pattern(data)


# Visual Representation


aggr_plot <- aggr(data, 
                  col=c('navyblue','red'), 
                  numbers=TRUE, 
                  sortVars=TRUE, 
                  labels=names(data), 
                  cex.axis=.7, 
                  gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))


# Extract lead created year and lead created month

library(lubridate)

data$Lead_Creation_Date <- as.Date(data$Lead_Creation_Date, 
                                              format='%d/%m/%y')

data$lead.creation.month <- month(data$Lead_Creation_Date)

data$lead.creation.month <- as.factor(data$lead.creation.month)
data$Var1 <- as.factor(data$Var1)
data$Approved <- as.factor(data$Approved)
data$Employer_Category2 <- as.factor(data$Employer_Category2)

data$DOB <- as.Date(data$DOB,
                    format='%d/%m/%y')

data$Age <- year(strptime(data$DOB,
                          format='%Y-%m-%d')) -
            year(strptime(data$Lead_Creation_Date, 
                          format='%Y-%m-%s'))

data <- data[,-c(2,3,4,6,10)]

# Data Imputation
library(Hmisc)

data$Age <- with(data, impute(Age, mean))
data$Existing_EMI <- with(data, impute(Existing_EMI, mean))

data$City_Category <- with(data, impute(City_Category, 'random'))
data$Employer_Category1 <- with(data, impute(Employer_Category1, 'random'))
data$Employer_Category2 <- with(data, impute(Employer_Category2, 'random'))
data$Primary_Bank_Type <- with(data, impute(Primary_Bank_Type, 'random'))


data <- na.omit(data)

# Split data into train and validation

library(caret)

a <- createDataPartition(data$Approved, 
                         p = 0.8, 
                         list=FALSE)
train <- data[a,]
validation <- data[-a,]

categorical_vars <- c("Gender",
 #                     "City_Code",
                      "City_Category",
#                      "Employer_Code",
                      "Employer_Category1",
                      "Employer_Category2",
#                      "Customer_Existing_Primary_Bank_Code",
                      "Primary_Bank_Type",
                      "Contacted", 
                      "Source",
                      "Source_Category",
                      "Var1",
                      "Approved",
                      "lead.creation.month")

for(col in categorical_vars) {
  levels(train[,col]) <- levels(data[,col])
  levels(validation[,col]) <- levels(data[,col])
}

# Load library ROSE for imbalenced data

library(ROSE)

train.ROSE <- ROSE(Approved ~ .,
                   data = train,
                   seed=100)$data

# load library randomforest

library(randomForest)

model_rf <- randomForest(Approved ~ .,
             data = train.ROSE,
             ntree=20,
             mtry=3)

bestmtry <- tuneRF(train, train$Approved, 
                   ntreeTry =20, stepFactor = 1.1, 
                   improve = 0.01, trace = T, plot = T)
print(bestmtry)

pred <- predict(model_rf, train)

# Model performance evaluation on train dataset
# Calculate root mean square error
RMSE.baseline <- sqrt(mean((as.integer(pred) - as.integer(train$Approved))^2))

print(paste("RMSE",RMSE.baseline))

# Calculate mean average error
MAE.baseline <- mean(abs(as.integer(pred) - as.integer(train$Approved)))
print(paste("MAE",
            MAE.baseline))

accuracy.meas(train$Approved, 
              pred)

roc.curve(train$Approved,
          pred, 
          plotit = F)


val.pred <- predict(model_rf, validation)

# Model performance evaluation on validation dataset
# Calculate root mean square error
RMSE.baseline.val <- sqrt(mean((as.integer(val.pred) - as.integer(validation$Approved))^2))

print(paste("RMSE",RMSE.baseline.val))

# Calculate mean average error
MAE.baseline.val <- mean(abs(as.integer(val.pred) - as.integer(validation$Approved)))
print(paste("MAE",MAE.baseline.val))






# Read test dataset

test <- read.csv('./data/test.csv',
                 header = T,
                 sep = ',',
                 na.strings = c("", " ", NA),
                 stringsAsFactors = T)

md.pattern(test)

test$Lead_Creation_Date <- as.Date(test$Lead_Creation_Date, 
                                   format='%d/%m/%y')

test$lead.creation.month <- month(test$Lead_Creation_Date)

test$lead.creation.month <- as.factor(test$lead.creation.month)
test$Var1 <- as.factor(test$Var1)
test$Employer_Category2 <- as.factor(test$Employer_Category2)

test$DOB <- as.Date(test$DOB,
                    format='%d/%m/%y')

test$Age <- year(strptime(test$DOB,
                          format='%Y-%m-%d')) -
  year(strptime(test$Lead_Creation_Date, 
                format='%Y-%m-%s'))

#test <- test[,-c(2,3,4,6,10)]

# Data Imputation
library(Hmisc)

test$Age <- with(test, impute(Age, mean))
test$Existing_EMI <- with(test, impute(Existing_EMI, mean))

test$City_Category <- with(test, impute(City_Category, 'random'))
test$Employer_Category1 <- with(test, impute(Employer_Category1, 'random'))
test$Employer_Category2 <- with(test, impute(Employer_Category2, 'random'))
test$Primary_Bank_Type <- with(test, impute(Primary_Bank_Type, 'random'))

categorical_vars <- c("Gender",
                      "City_Category",
                      "Employer_Category1",
                      "Employer_Category2",
                      "Primary_Bank_Type",
                      "Contacted", 
                      "Source",
                      "Source_Category",
                      "Var1",
                      "lead.creation.month")

for(col in categorical_vars_test) {
  levels(test[,col]) <- levels(data[,col])
}

test$Approved <- predict(model_rf, test)

write.csv(test[,c("ID", "Approved")],
          "./data/Submission 3.csv",
          row.names = F)
  
table(test$Approved)
