if(!require(tidyverse)) { install.packages('tidyverse', dependencies = T) }
library(tidyverse)

if(!require(lubridate)) { install.packages('lubridate', dependencies = T) }
library(lubridate)

if(!require(forecast)) { install.packages('forecast', dependencies = T) }
library(forecast)

if(!require(ISOweek)) { install.packages('ISOweek', dependencies = T) }
library(ISOweek)

# Cargamos los datos de compras en el data frame "data"
data <- read_tsv("../segmentacion_de_clientes/purchases.txt", col_names = FALSE)

# Añadimos al data frame los nombres de las variables
colnames(data) = c('customer_id', 'purchase_amount', 'date_of_purchase')
head(data)


weekly_purchase_amount <- data %>% as_data_frame() %>% 
  arrange((date_of_purchase)) %>% 
  mutate(iso_week = ISOweek(date_of_purchase)) %>% 
  group_by(iso_week) %>% 
  summarise(revenue = sum(purchase_amount))

weekly_purchase_amount$revenue

weekly_purchase_amount.ts <- ts(weekly_purchase_amount$revenue,frequency = 365.25/7, start = decimal_date(ymd("2005-01-02")))

autoplot(weekly_purchase_amount.ts) + geom_forecast(h=5)

seasonal_trend_irregular <- decompose(weekly_purchase_amount.ts)

seasonal_trend_irregular

plot(seasonal_trend_irregular)

ggseasonplot(weekly_purchase_amount.ts) + labs(title="Comparativa estacional por semanas") + theme_minimal()

# Descomposicion + predicción por suavizado exponencial
fit <- stlf(weekly_purchase_amount.ts, h = 53)
summary(fit)

autoplot(fit) + labs(title="Predicción") + theme_minimal()

plot(fit$x,col="red")
lines(fitted(fit),col="blue")



# DATOS MENSUALES ---------------------------------------------------------


# EJERCICIO:
# Agrupar los datos por mes y repetir el mismo análisis anterior

monthly_purchase_amount <- data %>% as_data_frame()%>% arrange((date_of_purchase)) %>% 
  mutate(month = format(date_of_purchase, "%m"), 
         year = format(date_of_purchase, "%Y"),
         year_month = paste0(year, month)) %>% 
  group_by(year_month) %>% 
  summarise(revenue = sum(purchase_amount))

# ...

