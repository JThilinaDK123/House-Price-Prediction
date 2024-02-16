# Load the libraries
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


# Load the data sets
data_train = read.csv("House_Prices.csv")
dim(data_train)

data_test = read_excel('BA-Predict.xlsx')
data_test <- as.data.frame(data_test)
data_test[, sapply(data_test, is.numeric)] <- lapply(data_test[, sapply(data_test, is.numeric)], as.integer)
dim(data_test)


# Data types
str(data_train)
str(data_test)


# Check duplicates
duplicate_rows_train = data_train[duplicated(data_train) | duplicated(data_train, fromLast = TRUE), ]
duplicate_rows_train #no duplicates

duplicate_rows_test = data_test[duplicated(data_test) | duplicated(data_test, fromLast = TRUE), ]
duplicate_rows_test #no duplicates


# Check for missing values
missing_values_train = is.na(data_train)
count_false_values <- sum(missing_values_train)
count_false_values

missing_values_test = is.na(data_test)
print(missing_values_test) # no missing values


# Dropping unwanted columns
drop_cols = c("YearBuilt")
data_train[drop_cols] = NULL
data_test[drop_cols] = NULL


# Re leveling
breaks <- c(1949, 1980, 2000, 2011)
data_train$YearRemodAdd <- cut(data_train$YearRemodAdd, 
                               breaks = breaks, labels = c("1950-1980", "1980-2000", "2000-2010"))
summary(data_train)
data_test$YearRemodAdd <- cut(data_test$YearRemodAdd, 
                              breaks = breaks, labels = c("1950-1980", "1980-2000", "2000-2010"))
summary(data_test)


# Factorized 
cols = c("YrSold","FullBath","OverallQual",
         "HalfBath","BedroomAbvGr","TotRmsAbvGrd",
         "Fireplaces","YearRemodAdd")
data_train[cols] = lapply(data_train[cols], factor)
data_test[cols] = lapply(data_test[cols], factor)
summary(data_train)
summary(data_test)
str(data_train)
str(data_test)

# 2nd phase of Re leveling
training_levels_1 <- levels(data_train$FullBath)
training_levels_2 <- levels(data_train$BedroomAbvGr)
training_levels_3 <- levels(data_train$TotRmsAbvGrd)
training_levels_4 <- levels(data_train$Fireplaces)
training_levels_5 <- levels(data_train$OverallQual)
data_test$FullBath <- forcats::fct_expand(data_test$FullBath, training_levels_1)
data_test$BedroomAbvGr <- forcats::fct_expand(data_test$BedroomAbvGr, training_levels_2)
data_test$TotRmsAbvGrd <- forcats::fct_expand(data_test$TotRmsAbvGrd, training_levels_3)
data_test$Fireplaces <- forcats::fct_expand(data_test$Fireplaces, training_levels_4)
data_test$OverallQual <- forcats::fct_expand(data_test$OverallQual, training_levels_5)
str(data_train)
str(data_test)



# Correlation test
# Spearman Correlation
cor_cot = subset(data_train,select = c("LotArea","GarageArea",
                                       "BsmtFinSF1"))
corrplot(cor(cor_cot,method = "spearman"),method ="color",
         addCoef.col="black",tl.col="black")

# Goodman Kruskal plot
var_set = c("FullBath","HalfBath","OverallQual",
            "BedroomAbvGr","TotRmsAbvGrd","Fireplaces","YrSold","YearRemodAdd")
krus_plot = subset(data_train,select = var_set)
plot(GKtauDataframe(krus_plot))

# Data preprocessing
df = data_train
cols=c("YrSold","FullBath","OverallQual",
       "HalfBath","BedroomAbvGr","TotRmsAbvGrd",
       "Fireplaces","YearRemodAdd")
df[cols] <- lapply(df[cols], factor)
df[cols] <- lapply(df[cols], factor)


# YrSold 

count_table <- table(df$YrSold)
count_df <- as.data.frame(count_table)
colnames(count_df)[colnames(count_df) == "Var1"] <- "Yr_Sold"
count_df$Percentage <- count_df$Freq / sum(count_df$Freq) * 100
ggplot(count_df, aes(x = Yr_Sold, y = Percentage, fill = Yr_Sold)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5) +
  xlab("Year Sold") +
  ylab("Percentage (%)") +
  ggtitle("Year Sold") +
  theme_minimal()

# OverallQual

count_table <- table(df$OverallQual)
count_df <- as.data.frame(count_table)
colnames(count_df)[colnames(count_df) == "Var1"] <- "Overall_Quality"
count_df$Percentage <- count_df$Freq / sum(count_df$Freq) * 100
ggplot(count_df, aes(x = Overall_Quality, y = Percentage, fill = Overall_Quality)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5) +
  xlab("Categories") +
  ylab("Percentage") +
  ggtitle("Overall Quality") +
  theme_minimal()

# LotArea vs OverallQual

ggplot(df, aes(x = OverallQual, y = LotArea)) +
  geom_boxplot() +
  xlab("Category") +
  ylab("Lot size in square feet") +
  ggtitle("Overall Quality vs Lot size in square feet")

# LotArea vs SalePrice

ggplot(df, aes(x = LotArea, y = SalePrice)) +
  geom_point() +
  labs(title = "Scatterplot", x = "LotArea", y = "SalePrice") +
  xlab("Lot size in square feet") +
  ylab("Sales Price $") +
  ggtitle("Sales Price vs Lot size in square feet") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))


# GarageArea vs SalePrice

ggplot(df, aes(x = GarageArea, y = SalePrice)) +
  geom_point() +
  labs(title = "Scatterplot", x = "GarageArea", y = "SalePrice") + 
  xlab("Garage Area size in square feet") +
  ylab("Sales Price $") +
  ggtitle("Sales Price vs Garage Area size in square feet") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))


# GarageArea vs SalePrice vs OverallQuality

ggplot(df, aes(x = GarageArea, y = SalePrice, color = factor(OverallQual))) +
  geom_point() +
  labs(title = "Scatterplot", x = "GarageArea", y = "SalePrice") +
  scale_color_discrete(name = "Overall Quality") +
  xlab("Garage Area size in square feet") +
  ylab("Sales Price $") +
  ggtitle("Sales Price vs Garage Area size in square feet vs Overall Quality") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))


# SalePrice vs OverallQual

ggplot(df, aes(x = OverallQual, y = SalePrice)) +
  geom_boxplot() +
  xlab("Category") +
  ylab("Sales Price $") +
  ggtitle("Overall Quality vs Sales Price") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))


# SalePrice vs YrSold

ggplot(df, aes(x = YrSold, y = LotArea)) +
  geom_boxplot() +
  xlab("Category") +
  ylab("Sales Price $") +
  ggtitle("Sold Year vs Sales Price") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))



# Linear Regression Model
lm_mod <- linear_reg() %>% set_engine("lm")
rec_ex <- recipe(SalePrice ~ ., data = data_train) %>% 
  step_zv(all_predictors()) %>% 
  step_dummy(all_nominal_predictors())
wflow <- workflow(rec_ex, lm_mod)
model_lr <- fit(wflow, data = data_train)
model_lr
prediction_lr = predict(model_lr, new_data = data_test)
r_squared_value <- R2(data_test$SalePrice, prediction_lr)
cat("R-squared:", r_squared_value, "\n")



# Random Forest
rf_model_1 <- train(SalePrice ~ ., data = data_train, method = "rf")
print(rf_model_1)
predictions_1 <- predict(rf_model_1, newdata = data_test)
mse_1 <- mean((data_test$SalePrice - predictions_1)^2)
print(paste("Mean Squared Error: ", mse_1))
r_squared_value_1 <- R2(data_test$SalePrice, predictions_1)
cat("R-squared:", r_squared_value_1, "\n")


# Random Forest
ctrl <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation
grid <- expand.grid(mtry = c(2, 4))
rf_model_2 <- train(SalePrice ~ ., data = data_train, method = "rf",
                  trControl = ctrl)

print(rf_model_2)
predictions <- predict(rf_model_2, newdata = data_test)
mse <- mean((data_test$SalePrice - predictions)^2)
print(paste("Mean Squared Error: ", mse))
r_squared_value <- R2(data_test$SalePrice, predictions)
cat("R-squared:", r_squared_value, "\n")

vip_data <- varImp(rf_model_2, scale = FALSE)
vip(rf_model_2, num_features = 10) 
