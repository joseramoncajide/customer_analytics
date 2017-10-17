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




# Calculamos las varibles recency, frequency y average purchase amount de cada cliente
customers_2015 <- data %>% 
  group_by(customer_id) %>% 
  summarise(recency = min(days_since),
            first_purchase = max(days_since),
            frequency = n(),
            amount = mean(purchase_amount))

# SEGMENTACION GERENCIAL --------------------------------------------------

# Segementamos los clientes
customers_2015 <-customers_2015 %>% 
  mutate(segment = case_when(recency > 365*3 ~ "inactive",
                             recency <= 365*3 & recency > 365*2  ~ "cold",
                             recency <= 365*2 & recency > 365*1 ~ "warm",
                             recency <= 365 ~ "active"
  ))

customers_2015 <- customers_2015 %>% 
  mutate(segment = case_when(segment == "warm" & first_purchase <= 365*2 ~ "new warm", 
                             segment == "warm" & amount < 100 ~ "warm low value",
                             segment == "warm" & amount >= 100 ~ "warm high value",
                             segment == "active" & first_purchase <= 365 ~ "new active",
                             segment == "active" & amount < 100 ~ "active low value",
                             segment == "active" & amount >= 100 ~ "active high value",
                             TRUE ~ segment))

# Convertimos el segmento, en una variable categórica y la ordenamos 
customers_2015$segment = factor(x = customers_2015$segment, levels = c("inactive", "cold",
                                                                       "warm high value", "warm low value", "new warm",
                                                                       "active high value", "active low value", "new active"))


# Creamos un dataframe con los clientes de 2014

customers_2014 <- data %>% 
  filter(days_since > 365) %>% 
  group_by(customer_id) %>% 
  summarise(recency = min(days_since) - 365,
            first_purchase = max(days_since) - 365,
            frequency = n(),
            amount = mean(purchase_amount))


# Segmentamos los clientes de 2014
customers_2014 <-customers_2014 %>% 
  mutate(segment = case_when(recency > 365*3 ~ "inactive",
                             recency <= 365*3 & recency > 365*2  ~ "cold",
                             recency <= 365*2 & recency > 365*1 ~ "warm",
                             recency <= 365 ~ "active"
  ))

customers_2014 <- customers_2014 %>% 
  mutate(segment = case_when(segment == "warm" & first_purchase <= 365*2 ~ "new warm", 
                             segment == "warm" & amount < 100 ~ "warm low value",
                             segment == "warm" & amount >= 100 ~ "warm high value",
                             segment == "active" & first_purchase <= 365 ~ "new active",
                             segment == "active" & amount < 100 ~ "active low value",
                             segment == "active" & amount >= 100 ~ "active high value",
                             TRUE ~ segment))

# Convertimos el segmento, en una variable categórica y la ordenamos 
customers_2014$segment = factor(x = customers_2014$segment, levels = c("inactive", "cold",
                                                                       "warm high value", "warm low value", "new warm",
                                                                       "active high value", "active low value", "new active"))



# MATRIZ DE TRANSICIÓN ----------------------------------------------------

new_data <- left_join(customers_2014, customers_2015, by="customer_id")
new_data

transition <- table(new_data$segment.x, new_data$segment.y)
transition

# Dividimos cada fila por su total
transition <- transition / rowSums(transition)
transition




# USAR LA MATRIZ DE TRANSCIÓN PARA HACER PREDICCIONES ---------------------


# Creamos un matriz con el número de clientes en cada segmento hoy y en los próximos 10 años
# Initialize a matrix with the number of customers in each segment today and after 10 periods
segments <- matrix(nrow = 8, ncol = 11)
segments
segments[, 1] <- table(customers_2015$segment)
segments
colnames(segments) <- 2015:2025
row.names(segments) <- levels(customers_2015$segment)
segments

# Multiplicamos el vector con el número de clientes por la matriz de transición
segments[, 1] %*% transition

for (i in 2:11) {
  segments[, i] = segments[, i-1] %*% transition
}

round(segments)

# Evolución de los segmentos "inactive", "active high value" en el tiempo
barplot(segments[1, ], main = "inactive")
barplot(segments[2, ], main = "cold")


# CALCULO DEL CLV ---------------------------------------------------------


# Ingresos anuales por segmento
# Viene de modulo 2, línea 2R8
yearly_revenue <- c(0, 0, 0, 0, 0, 323.57, 52.31, 79.17)

# Calculamos los ingresos por segmento
revenue_per_segment <- yearly_revenue * segments
revenue_per_segment

# Ingresos anuales
yearly_revenue <- colSums(revenue_per_segment)
round(yearly_revenue)
barplot(yearly_revenue)

# Ingresos anuales acumulados
cumulated_revenue <- cumsum(yearly_revenue)
round(cumulated_revenue)
barplot(cumulated_revenue)

# Tasa de descuento
discount_rate <- 0.10
discount = 1 / ((1 + discount_rate) ^ ((1:11) - 1))
print(discount)

# Ingresos anuales acumulados descontados
disc_yearly_revenue = yearly_revenue * discount
round(disc_yearly_revenue)
barplot(disc_yearly_revenue)
lines(yearly_revenue)

# Ingresos anuales acumulados actualizados
disc_cumulated_revenue <- cumsum(disc_yearly_revenue)
round(disc_cumulated_revenue)
barplot(disc_cumulated_revenue)

# ¿Cuál es el valor de nuestros clientes?
print(disc_cumulated_revenue[11] - yearly_revenue[1])
