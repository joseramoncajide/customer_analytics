##########################################################################
# Jose Cajide - @jrcajide
# Customer Analytics: Predicción de ingresos (Regresión)
##########################################################################

library(tidyverse)

cust_data <- read_tsv("../segmentacion_de_clientes/purchases.txt", col_names = FALSE)
cust_data <- rename(cust_data, customer_id = X1, purchase_amount = X2, date_of_purchase = X3)

cust_data$days_since <- as.numeric(difftime(time1 = "2016-01-01",
                                            time2 = cust_data$date_of_purchase,
                                            units = "days"))  

## Create recency variable
cust_data$year_of_purchase <- as.numeric(format(cust_data$date_of_purchase, "%Y"))

min(cust_data$date_of_purchase)

cust_data$days_since <- as.numeric(difftime(time1 = "2016-01-01",
                                            time2 = cust_data$date_of_purchase,
                                            units = "days"))  


customers_2014 <- cust_data %>%
  filter(year_of_purchase == 2014) %>%
  group_by(customer_id) %>%
  summarise(recency = min(days_since) - 365, 
            frequency = n(), 
            first_purchase = max(days_since) - 365,
            avg_amount = mean(purchase_amount),
            revenue = sum(purchase_amount))

summary(customers_2014)

customers_2015 <- cust_data %>%
  filter(year_of_purchase == 2015) %>%
  group_by(customer_id) %>%
  summarise(recency = min(days_since), 
            frequency = n(), 
            first_purchase = max(days_since),
            avg_amount = mean(purchase_amount),
            revenue = sum(purchase_amount)) 

summary(customers_2015)

# Training set
# 
# 2014 customers who stayed loyal in 2015
active_2014 <- semi_join(customers_2014, customers_2015, by = "customer_id")
summary(active_2014)

row.names(active_2014) <- active_2014$customer_id
active_2014 <- select(active_2014, -customer_id)

# Test set
# 
# Actual spending by active 2014 customers in 2015

# https://psyteachr.github.io/msc-data-skills/joins.html#semi_join

active_2015 <- semi_join(customers_2015, customers_2014, by = "customer_id")
summary(active_2015)

# Revenue per customer

ggplot(active_2014, aes(x = revenue)) +
  geom_histogram(bins = 40, fill = "steelblue") +
  theme_classic() +
  labs(x = "revenue", title = "Annual revenue p/customer") 


# Frequency and Recency

ggplot(active_2014, aes(x = frequency)) +
  geom_histogram(bins = 10, fill = "steelblue") +
  theme_classic() +
  labs(x = "frequency", title = "Purchase frequency") 

ggplot(active_2014, aes(x = recency)) +
  geom_histogram(bins = 40,fill = "steelblue") +
  theme_classic() +
  labs(x = "recency", title = "Recency in Days")

# First purchase and Average amount spent

ggplot(active_2014, aes(x = first_purchase)) +
  geom_histogram(fill = "steelblue") +
  theme_classic() +
  labs(x = "first purchase (no.of days since)", title = "Days since first purchase") 

ggplot(active_2014, aes(x = avg_amount)) +
  geom_histogram(bins = 40, fill = "steelblue") +
  theme_classic() +
  labs(x = "average amount", title = "Average amount spent per transaction")

# MODEL


# MSE: https://en.wikipedia.org/wiki/Mean_squared_error
# RMSE: https://en.wikipedia.org/wiki/Root-mean-square_deviation
# MAE: https://en.wikipedia.org/wiki/Mean_absolute_error


library(caret)

# https://en.wikipedia.org/wiki/Cross-validation_(statistics)
ctrl <- trainControl(method = "repeatedcv", 
                     number = 5, 
                     repeats = 2)

set.seed(32)


# Linear models
lm_fit <- train(revenue ~.,
                data = active_2014,
                method = "lm",
                trControl = ctrl)
lm_fit

# Non-linear models



# Random forests
rF_fit <- train(revenue ~., 
                data = active_2014, 
                method = "rf", 
                metric = "RMSE", 
                trControl = ctrl,
                importance = TRUE)

rF_fit

# Multivariate adaptive regression splines (MARS)
mars_fit <- train(revenue ~., 
                  data = active_2014, 
                  method = "earth", 
                  metric = "RMSE",
                  tuneLength = 10,
                  trControl = ctrl)

mars_fit

# Support vector machines
svm_fit <- train(revenue~.,
                 data = active_2014,
                 method = "svmRadial",
                 preProc = c("center", "scale"),
                 tuneLength = 10,
                 trControl = ctrl)
svm_fit


# Compare all models

results <- resamples(list(RandomForest = rF_fit,
                          MARS = mars_fit,
                          SVM = svm_fit,
                          OLS = lm_fit))
summary(results)


bwplot(results, metric = "RMSE", main = "Comparative Performance\nRoot Mean Squared Error")

bwplot(results, metric = "Rsquared", main = "Comparative Performance\nR Squared")

bwplot(results, metric = "MAE", main = "Comparative Performance\nMean Absolute Error")

# Optional: hypothesis tests to confirm that the performance differences observed above are statistically significant
modelDiff <- diff(results)
summary(modelDiff)


# Best model vs. null model

# Build null model that predicts every outcome as mean revenue
useless <- nullModel(x = active_2014[,-5], y = active_2014$revenue)
useless

uselessPred <- predict(useless, newdata = active_2015, type = "raw")

#MSE
uselessMSE <- mean((uselessPred - active_2015$revenue)^2)
uselessMSE

# RMSE
sqrt(uselessMSE)



# Predictions with best model
rF_pred <- predict(rF_fit, newdata = active_2015)
head(rF_pred)

# MSE - mean squared error
pred_MSE <- mean((rF_pred - active_2015$revenue)^2)
pred_MSE

# RMSE
sqrt(pred_MSE)


