### Load the libraries ### 
library(readxl)
library(GoodmanKruskal)
library(corrplot)
library(performance)
library(parameters)
library(forcats)
library(car)
library(caret)
library(Metrics)
library(randomForest)
library(tidyverse)
library(tidymodels)
library(vip)
library(randomForest)

### Load the data sets ### 
train_data <- read.csv("House_Prices.csv")
dim(train_data)
str(train_data)
summary(train_data)

test_data <- read_excel('BA-Predict.xlsx')
test_data <- as.data.frame(test_data)
test_data[, sapply(test_data, is.numeric)] <- lapply(test_data[, sapply(test_data, is.numeric)], as.integer)
dim(test_data)
str(test_data)
summary(test_data)


###  Check duplicates ###
train_data[duplicated(train_data) | duplicated(train_data, fromLast = TRUE), ]
test_data[duplicated(test_data) | duplicated(test_data, fromLast = TRUE), ]

### Check for missing values ###
count_false_values <- sum(is.na(train_data))
count_false_values

count_false_values <- sum(is.na(test_data))
count_false_values

### Create new column ###
train_data <- train_data %>%
  mutate(Modified = ifelse(YearBuilt == YearRemodAdd, "Not Modified", "Modified"))
test_data <- test_data %>%
  mutate(Modified = ifelse(YearBuilt == YearRemodAdd, "Not Modified", "Modified"))


###  Variable Conversions ###
breaks <- c(1879, 1920, 1960, 2000, 2020)
train_data$YearBuilt <- cut(train_data$YearBuilt, breaks = breaks, labels = c("1880-1920","1920-1960","1960-2000","2000-2020"))
summary(train_data)
test_data$YearBuilt <- cut(test_data$YearBuilt, breaks = breaks, labels = c("1880-1920","1920-1960","1960-2000","2000-2020"))
summary(test_data)


###  Dropping unwanted columns ###
drop_cols <- c("YearRemodAdd")
train_data[drop_cols] <- NULL
test_data[drop_cols] <- NULL


###  Factorized ###
factor_cols <- c("YrSold", "FullBath", "HalfBath", "BedroomAbvGr", "TotRmsAbvGrd", "Fireplaces", "YearBuilt","Modified")
train_data[factor_cols] <- lapply(train_data[factor_cols], factor)
test_data[factor_cols] <- lapply(test_data[factor_cols], factor)
summary(train_data)
summary(test_data)
str(train_data)
str(test_data)


### Create the Response ###
train_data$OverallQual <- cut(train_data$OverallQual,breaks = c(-Inf, 6, Inf),labels = c("0", "1"))
train_data$OverallQual <-factor(train_data$OverallQual,levels = c("0","1"),labels = c(0,1))
test_data$OverallQual <- cut(test_data$OverallQual,breaks = c(-Inf, 6, Inf),labels = c("0", "1"))
test_data$OverallQual <- factor(test_data$OverallQual,levels = c("0","1"),labels = c(0,1))
train_data$OverallQual <- factor(train_data$OverallQual)
test_data$OverallQual <- factor(test_data$OverallQual)
str(train_data)
str(test_data)

### Recreating Levels ###
variables <- c("FullBath", "BedroomAbvGr", "TotRmsAbvGrd", "Fireplaces")
for (variable in variables) {
  levels_train <- levels(train_data[[variable]])
  test_data[[variable]] <- forcats::fct_expand(test_data[[variable]], levels_train)
}

str(train_data)
str(test_data)


### Random Forest ###
# Without hyper parameter tuning

set.seed(345)
model1 = randomForest(formula=OverallQual~.,data = train_data)
predictions_1 = model1 %>% predict(test_data,type = "class")
confusionMatrix(predictions_1,test_data$OverallQual)


# With hyper parameter tuning
### Custom Grid
list_criterion = c("Gini"=1, "Entropy"=2, "Log_Loss"=3)

customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree", "min_samples_split", "max_depth", "criterion"), 
                                  class = rep("numeric", 5), label = c("mtry", "ntree", "min_samples_split", "max_depth", "criterion"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, min_samples_split=param$min_samples_split, 
               max_depth=param$max_depth, criterion=param$criterion, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

## Create a training control object for cross-validation
ctrl <- trainControl(
  method = "cv",
  number = 5,
  search = 'grid',
  savePredictions = "all"
)
hyperparameter_grid <- expand.grid(.mtry=c(2,4,6), .ntree = c(10, 20),
                                   .min_samples_split = c(5, 10, 15, 20, 30) , 
                                   .max_depth = c(2, 10, 20, 40),
                                   .criterion = list_criterion)

model1 <- train(
  OverallQual ~ .,
  data = train_data,
  method = customRF,
  trControl = ctrl,
  tuneGrid = hyperparameter_grid
)

best_params <- model1$bestTune
best_f1_score <- model1$bestTune$F1
cat("Best Parameters: ", paste(names(best_params), best_params, sep = " = "), "\n")

best_ml_1 = randomForest(formula=OverallQual~.,data = train_data, 
                         ntree = 10, 
                         mtry = 6,      
                         min_samples_split = 20, 
                         max_depth = 20,      
                         criterion = "Gini")
predictions_2 <- predict(best_ml_1, newdata = test_data, type = "response")
confusion_matrix <- confusionMatrix(data = predictions_2, 
                                    reference = test_data$OverallQual,
                                    mode = "everything")
confusion_matrix
