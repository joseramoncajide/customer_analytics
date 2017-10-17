##########################################################################
# Jose Ramón Cajide, 2017-10
# Customer Analytics: Segmentación de clientes
##########################################################################

rm(list = ls())

if(!require(tidyverse)) { install.packages('tidyverse', dependencies = T) }
library(tidyverse)

if(!require(clue)) { install.packages('clue', dependencies = T) }
library(tidyr)


# Calculamos RFM de todos los clientes ------------------------------------

past_transactions <- read_tsv("purchases.txt", col_names = FALSE)
colnames(past_transactions) = c('customer_id', 'purchase_amount', 'date_of_purchase')

all_customers <- past_transactions %>% 
  mutate(days_since = as.numeric(difftime(time1 = "2016-01-01", time2 = date_of_purchase, units = "days"))) %>% 
  group_by(customer_id) %>% 
  summarise(recency = min(days_since),
            frequency = n(),
            amount = mean(purchase_amount))


# ####################### NUEVAS TRANSACCIONES


# Objetivo: asignar un segmento -------------------------------------------

new_transactions <- read_csv("new_purchases.txt")

new_transactions <- new_transactions %>% 
  mutate(days_since = as.numeric(difftime(time1 = "2016-01-01", time2 = date_of_purchase, units = "days"))) %>% 
  group_by(customer_id) %>% 
  summarise(recency = min(days_since),
            frequency = n(),
            amount = mean(purchase_amount))

# ¿Qué sucede? ¿Son nuestras variables correctas?
new_transactions


# Incorporación de las nuevas transacciones al histórico ------------------

new_transactions <- read_csv("new_purchases.txt")

# ¿Que ids de cliente aparecen en las nuevas compras?
customers_in_new_transactions <- new_transactions %>% 
  select(customer_id) %>% 
  pull()

customers_in_new_transactions

all_transactions <- bind_rows(new_transactions, past_transactions)

new_customers <- all_transactions %>% 
  filter(customer_id %in% customers_in_new_transactions) %>% 
  mutate(days_since = as.numeric(difftime(time1 = "2016-01-01", time2 = date_of_purchase, units = "days"))) %>% 
  group_by(customer_id) %>% 
  summarise(recency = min(days_since),
            frequency = n(),
            amount = mean(purchase_amount))
 

new_customers_scaled <- new_customers %>% select(-customer_id) %>% scale() 

# Cargamos nuestro modelo kmeans
modelo <- readRDS('kmeans.obj')

cl_predict(modelo, new_customers_scaled)

new_customers %>% mutate(new_segment = cl_predict(modelo, new_customers_scaled))

# Veamos si nuestro clientes han cambiado de segmento
segmented_customers <- read_csv('segmented_customers.csv')

segmented_customers_updated <- segmented_customers %>% 
  filter(customer_id %in% customers_in_new_transactions) %>% 
  mutate(new_segment = cl_predict(segments, new_customers_scaled))

segmented_customers_updated


xtabs(~segment + new_segment ,data = segmented_customers_updated)

