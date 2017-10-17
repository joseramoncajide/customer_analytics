##########################################################################
# Jose Ramón Cajide, 2017-10
# Customer Analytics: Segmentación de clientes
##########################################################################

if(!require(tidyverse)) { install.packages('tidyverse', dependencies = T) }
library(tidyverse)

if(!require(tidyr)) { install.packages('tidyr', dependencies = T) }
library(tidyr)

if(!require(sqldf)) { install.packages('sqldf', dependencies = T) }
library(sqldf)

if(!require(nnet)) { install.packages('nnet', dependencies = T) }
library(nnet)

if(!require(broom)) { install.packages('broom', dependencies = T) }
library(broom)

# Cargamos los datos de compras en el data frame "data"
data <- read_tsv("purchases.txt", col_names = FALSE)
head(data)

# Añadimos al data frame los nombres de las variables
colnames(data) = c('customer_id', 'purchase_amount', 'date_of_purchase')

data <- data %>% 
  mutate(days_since = as.numeric(difftime(time1 = "2016-01-01", time2 = date_of_purchase, units = "days")), 
         year_of_purchase = as.numeric(format(date_of_purchase, "%Y"))) 

head(data)
summary(data)

# Calculamos las varibles recency, frequency y average purchase amount de cada cliente sitúandonos hace un año
customers_2014 = sqldf("SELECT customer_id,
                       MIN(days_since) - 365 AS 'recency',
                       MAX(days_since) - 365 AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'avg_amount',
                       MAX(purchase_amount) AS 'max_amount'
                       FROM data
                       WHERE days_since > 365
                       GROUP BY 1")

customers_2014 <- data %>% 
  filter(days_since > 365) %>% 
  group_by(customer_id) %>% 
  summarise(recency = min(days_since) - 365,
            first_purchase = max(days_since) - 365,
            frequency = n(),
            avg_amount = mean(purchase_amount),
            max_amount = max(purchase_amount))

# ¿Cuánto dinero genera cada segmento a la empresa?
revenue_2015 = sqldf("SELECT customer_id, SUM(purchase_amount) AS 'revenue_2015'
                     FROM data
                     WHERE year_of_purchase = 2015
                     GROUP BY 1")

revenue_2015 <- data %>% 
  filter(year_of_purchase == 2015) %>% 
  group_by(customer_id) %>% 
  summarise(revenue_2015 = sum(purchase_amount))


# Unimos los clientes de 2014 con los ingresos que han generado en 2015 y creamos la variable "active_2015"

in_sample = left_join(customers_2014, revenue_2015)
in_sample <- in_sample %>% 
  mutate(revenue_2015 = if_else(is.na(revenue_2015), 0, revenue_2015), 
         active_2015 = as.numeric(revenue_2015 > 0))


head(in_sample)
summary(in_sample)
table(in_sample$active_2015)



# MODELADO ESTADISTICO ----------------------------------------------------

# Se trata de:
# 1) ver como influyen las variables RFM en los ingresos
ggplot(in_sample, aes(x=frequency, y=log(revenue_2015))) + geom_point() + 
  stat_smooth(method="glm", se=FALSE) + theme_minimal() +  labs(title = "Ingresos en función de la frecuencia de compra.")

ggplot(in_sample, aes(x=recency, y=log(revenue_2015))) + geom_point() + 
  stat_smooth(method="glm", se=FALSE) + theme_minimal() +  labs(title = "Ingresos en función de la recencia de la compra.")


#2) preveer si un cliente estará activo en 2015 dadas las variables RFM
ggplot(in_sample, aes(x=frequency, y=active_2015)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) + theme_minimal() +  labs(title = "Probabilidad de que un cliente siga activo en función de la frecuencia de compra.")

ggplot(in_sample, aes(x=recency, y=active_2015)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) + theme_minimal() +  labs(title = "Probabilidad de que un cliente siga activo en función de la recencia de compra.")



# Entrenamos los modelos con el conjunto de datos "in_sample"

prob.model <- multinom(formula = active_2015 ~ recency + first_purchase + frequency + avg_amount + max_amount,
                      data = in_sample)
coef <- summary(prob.model)$coefficients
std  <- summary(prob.model)$standard.errors
print(coef)
print(std)
print(coef / std) #>2 ó <-2 es significativo

# Si un cliente está activo, ¿cuanto gastará?
# Modelo monetario. Tomamos únicamente aquellos clientes que han hecho una compra
in_sample_purchased_2015 <- in_sample %>% 
  filter(active_2015 == 1) 
head(in_sample_purchased_2015)
summary(in_sample_purchased_2015)

# Ajustamos el modelo
amount.model = lm(formula = revenue_2015 ~ avg_amount + max_amount, data = in_sample_purchased_2015)
summary(amount.model)


tidy(amount.model)
glance(amount.model)
head(augment(amount.model))

# Resultados del modelo
plot(x = in_sample_purchased_2015$revenue_2015, y = amount.model$fitted.values)


# Ajsutamos el modelo empleando una transformación logarítmica
amount.model = lm(formula = log(revenue_2015) ~ log(avg_amount) + log(max_amount), data = in_sample_purchased_2015)
summary(amount.model)

# Resultado del nuevo modelo
plot(x = log(in_sample_purchased_2015$revenue_2015), y = amount.model$fitted.values)



# APLICACION DE LOS MODELOS A LOS DATOS ACTUALES --------------------------


# Variables RFM actuales
customers_2015 = sqldf("SELECT customer_id,
                       MIN(days_since) AS 'recency',
                       MAX(days_since) AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'avg_amount',
                       MAX(purchase_amount) AS 'max_amount'
                       FROM data GROUP BY 1")

customers_2015 <- data %>% 
  group_by(customer_id) %>% 
  summarise(recency = min(days_since),
            first_purchase = max(days_since),
            frequency = n(),
            avg_amount = mean(purchase_amount),
            max_amount = max(purchase_amount))


# Aplicamos los modelos para predecir:
# 1) La probabilidad de que un cliente siga activo
predict(object = prob.model, newdata = customers_2015, type = "probs") %>% head(10)

# 2) Estimar los ingresos que realizarán
predict(object = amount.model, newdata = customers_2015) %>% exp() %>%  head( 10)

customers_2015 <- customers_2015 %>% 
  mutate(prob_predicted = predict(object = prob.model, newdata = customers_2015, type = "probs"),
         revenue_predicted = exp(predict(object = amount.model, newdata = customers_2015)),
         score_predicted = prob_predicted * revenue_predicted)

summary(customers_2015$prob_predicted)
summary(customers_2015$revenue_predicted)
summary(customers_2015$score_predicted)
hist(customers_2015$score_predicted)


# ¿Cuántos clientes esperamos que gasten más de 50€?

customers_2015 %>% 
  filter(score_predicted > 50) %>% 
  nrow()
