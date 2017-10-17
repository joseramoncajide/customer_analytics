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


# Calculamos las varibles recency, frequency y average purchase amount de cada cliente
customers_2015 = sqldf("SELECT customer_id,
                               MIN(days_since) AS 'recency',
                               MAX(days_since) AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               AVG(purchase_amount) AS 'amount'
                        FROM data GROUP BY 1")
head(customers_2015)

customers_2015 <- data %>% 
  group_by(customer_id) %>% 
  summarise(recency = min(days_since),
            first_purchase = max(days_since),
            frequency = n(),
            amount = mean(purchase_amount))
head(customers_2015)

# Exploramos los datos de clientes ----------------------------------------

summary(customers_2015)
hist(customers_2015$recency)
hist(customers_2015$frequency)
hist(customers_2015$amount)
hist(customers_2015$amount, breaks = 100)



# SEGMENTACION GERENCIAL --------------------------------------------------

# Primeros pasos

customers_2015 <- customers_2015 %>% 
  mutate(segment = if_else(recency > 365*3, "inactive", "NA" ))

table(customers_2015$segment)

customers_2015 %>% 
  select(-customer_id) %>% 
  group_by(segment) %>% 
  summarise_all(mean)




# Solución de 3 segmentos basados únicamente en la recencia de la compra.

customers_2015 <- customers_2015 %>% 
  mutate(segment = if_else(recency > 365*3, "inactive", 
                           if_else(recency > 365*2, "cold", "NA"))
         )

table(customers_2015$segment)

customers_2015 %>% 
  select(-customer_id) %>% 
  group_by(segment) %>% 
  summarise_all(mean)


# Solución de 4 segmentos

customers_2015 <- customers_2015 %>% 
  mutate(segment = if_else(recency > 365*3, "inactive", 
                           if_else(recency <= 365*3 & recency > 365*2, "cold", 
                                   if_else(recency <= 365*2 & recency > 365*1, "warm", 
                                           if_else(recency <= 365, "active", "NA"))))
  )

table(customers_2015$segment)

customers_2015 %>% 
  select(-customer_id) %>% 
  group_by(segment) %>% 
  summarise_all(mean)

# Mismo proceso de segmentación usando "case_when"


customers_2015 <-customers_2015 %>% 
  mutate(segment = case_when(recency > 365*3 ~ "inactive",
                             recency <= 365*3 & recency > 365*2  ~ "cold",
                             recency <= 365*2 & recency > 365*1 ~ "warn",
                             recency <= 365 ~ "active"))

table(customers_2015$segment)



# Toda la segmentación ahora junta
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
customers_2015$segment = factor(x = customers_2015$segment, 
                              levels = c("inactive", "cold", 
                                         "warm high value", "warm low value", "new warm", 
                                         "active high value", "active low value", "new active"))
levels(customers_2015$segment)
table(customers_2015$segment)
barplot(table(customers_2015$segment), main = "Año 2015: Número de clientes por segmento")

# Comprobamos las características de cada segmento
customers_2015 %>% 
  select(-customer_id) %>% 
  group_by(segment) %>% 
  summarise_all(mean)



# PPT ---------------------------------------------------------------------


# SEGMENTACION RETROSPECTIVA ----------------------------------------------


# Calculamos las varibles recency, frequency y average purchase amount de cada cliente sitúandonos hace un año
customers_2014 = sqldf("SELECT customer_id,
                       MIN(days_since) - 365 AS 'recency',
                       MAX(days_since) - 365 AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'amount'
                       FROM data
                       WHERE days_since > 365
                       GROUP BY 1")

customers_2014 <- data %>% 
  filter(days_since > 365) %>% 
  group_by(customer_id) %>% 
  summarise(recency = min(days_since) - 365,
            first_purchase = max(days_since) - 365,
            frequency = n(),
            amount = mean(purchase_amount))

# Repetimos el mismo proceso de segmentación

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
table(customers_2014$segment)
barplot(table(customers_2014$segment), main = "Año 2014: Número de clientes por segmento")

# Características de cada segmento
customers_2014 %>% 
  select(-customer_id) %>% 
  group_by(segment) %>% 
  summarise_all(mean)


# PPT ---------------------------------------------------------------------

# CALCULAMOS LOS INGRESOS POR SEGMENTO ------------------------------------

# ¿Cuánto dinero genera cada segmento a la empresa?

revenue_2015 = sqldf("SELECT customer_id, SUM(purchase_amount) AS 'revenue_2015'
                     FROM data
                     WHERE year_of_purchase = 2015
                     GROUP BY 1")

revenue_2015 <- data %>% 
  filter(year_of_purchase == 2015) %>% 
  group_by(customer_id) %>% 
  summarise(revenue_2015 = sum(purchase_amount))

# Dinero gastado por cada cliente en el 2015
head(revenue_2015)
summary(revenue_2015)


# Unimos el data frame de clientes del 2015 con los ingresos que han generado en 2015
head(customers_2015, 1)
head(revenue_2015, 1)
actual <- inner_join(customers_2015, revenue_2015)
head(actual, 1)

# ¿Qué ha sucedido?
nrow(customers_2015)
nrow(revenue_2015)
nrow(actual)

# Unimos el data frame de clientes con los ingresos (Forma correcta)
actual <- left_join(customers_2015, revenue_2015)
nrow(actual)

# ¿Qué ha pasado con los clientes de 2015 que no han generado ingresos?
summary(actual)
sum(is.na(actual$revenue_2015))

actual <- actual %>% 
  mutate(revenue_2015 = if_else(is.na(revenue_2015), 0, revenue_2015))
summary(actual)


# ¿Cuál ha sido el ingreso medio por segmento en el 2015?
actual %>% 
  group_by(segment) %>% 
  summarise(revenue_2015 = mean(revenue_2015))


# Vamos ahora a viajar al futuro
# Unimos los clientes en 2014 con los ingresos que han generado en 2105
forward <- left_join(customers_2014, revenue_2015)
forward <- forward %>% 
  mutate(revenue_2015 = if_else(is.na(revenue_2015), 0, revenue_2015))
head(forward)


# ¿Cuál ha sido el ingreso medio por segmento en el 2015 de los clientes del 2014?
forward %>% 
  group_by(segment) %>% 
  summarise(revenue_2015 = mean(revenue_2015)) %>% 
  arrange(desc(revenue_2015)) %>% 
  ggplot(aes(x=segment, y=revenue_2015)) + geom_bar(stat = 'identity')
