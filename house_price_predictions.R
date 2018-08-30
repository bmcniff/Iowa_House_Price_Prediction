# install packages
install.packages("tidyverse")
install.packages("rpart")
install.packages("modelr")
install.packages("randomForest")

# load in the tidyverse package
library(tidyverse)

# read data
iowa_data <- read_csv("train.csv") 

# ensuring Condition1 is a factor & not a char
iowa_data$Condition1 <- as.factor(iowa_data$Condition1)

# examining data
summary(iowa_data)

# loading in rpart package
library(rpart)

# listing column names
names(iowa_data)

#creating new model
fit <- rpart(SalePrice ~ LotArea + YearBuilt + Condition1 + FullBath + BedroomAbvGr + TotRmsAbvGrd, data = iowa_data)

#evaluating new model
print("Making predictions for:")
print(head(iowa_data))

print("Predictions are")
print(predict(fit, head(iowa_data)))

print("Actual price is")
print(head(iowa_data$SalePrice))

print("The MAE is")
library(modelr)
print(mae(model = fit, data = iowa_data))

# function created to get the maximum average error for a given max depth.
get_mae <- function(maxdepth, target, predictors, training_data, testing_data){
  
  predictors <- paste(predictors, collapse="+")
  formula <- as.formula(paste(target,"~",predictors,sep = ""))
  
  model <- rpart(formula, data = training_data,
                 control = rpart.control(maxdepth = maxdepth))
  
  mae <- mae(model, testing_data)
  return(mae)
}

# finding maxdepth that leads to the lowest mean average error for this dataset
splitData <- resample_partition(iowa_data, c(test = 0.3, train = 0.7))

target <- "SalePrice"
predictors <-  c("LotArea", "YearBuilt", "Condition1", "FullBath", "BedroomAbvGr", "TotRmsAbvGrd")

for(i in 1:10){
  mae <- get_mae(maxdepth = i, target = target, predictors = predictors,
                 training_data = splitData$train, testing_data = splitData$test)
  print(glue::glue("Maxdepth: ",i,"\t MAE: ",mae))
}

# read in the library we'll use for random forests
library(randomForest)

# training random forest model to compare
fitRandomForest <- randomForest(SalePrice ~ LotArea + YearBuilt + Condition1 + FullBath + BedroomAbvGr + TotRmsAbvGrd, data = splitData$train)

# compute MAE
print("MAE is")
mae(model = fitRandomForest, data = splitData$test)