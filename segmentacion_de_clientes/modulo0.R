
# install.packages('tidyverse')
library(tidyverse)

if(!require(sqldf)) { install.packages('sqldf', dependencies = T) }
library(sqldf)

# Cargamos los datos de compras en el data frame "data"
data <- read_tsv("purchases.txt", col_names = FALSE)
head(data)

# Añadimos al data frame los nombres de las variables
colnames(data) = c('customer_id', 'purchase_amount', 'date_of_purchase')
head(data)

# No necesario:
# Indicamos que la variable date_of_purchase es del tipo fecha
# data$date_of_purchase = as.Date(data$date_of_purchase, "%Y-%m-%d")

# Añadimos una nueva variable con el año de compra
data$year_of_purchase = as.numeric(format(data$date_of_purchase, "%Y"))
head(data)



# Exploramos los datos ----------------------------------------------------

# Calculamos el número de pedidos o compras por año
x <- sqldf("SELECT year_of_purchase, COUNT(year_of_purchase) AS 'purchases' FROM data GROUP BY 1 ORDER BY 1")
head(x)

x <- data %>% 
  group_by(year_of_purchase) %>% 
  summarise(purchases = n()) 
head(x)

barplot(x$purchases, names.arg = x$year_of_purchase)

# Importe medio de compra por año
x = sqldf("SELECT year_of_purchase, AVG(purchase_amount) AS 'avg_amount' FROM data GROUP BY 1 ORDER BY 1")

head(x)

x <- data %>% 
  group_by(year_of_purchase) %>% 
  summarise(avg_amount = mean(purchase_amount))

head(x)

barplot(x$avg_amount, names.arg = x$year_of_purchase)


# Ingresos por año
x <- sqldf("SELECT year_of_purchase, SUM(purchase_amount) AS 'sum_amount' FROM data GROUP BY 1 ORDER BY 1")

x <- data %>% 
  group_by(year_of_purchase) %>% 
  summarise(sum_amount = sum(purchase_amount))

head(x)

barplot(x$sum_amount, names.arg = x$year_of_purchase)

# Podemos hacer todo de una sola vez
x <- sqldf("SELECT year_of_purchase,
          COUNT(year_of_purchase) AS 'counter',
          AVG(purchase_amount) AS 'avg_amount',
          SUM(purchase_amount) AS 'sum_amount'
          FROM data GROUP BY 1 ORDER BY 1")

head(x)

x <- data %>% 
  group_by(year_of_purchase) %>% 
  summarise(counter = n(), 
            avg_amount = mean(purchase_amount),
            sum_amount = sum(purchase_amount) 
            ) -> x

head(x)
