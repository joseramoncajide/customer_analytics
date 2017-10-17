##########################################################################
# Jose Ramón Cajide, 2017-10
# Customer Analytics: Segmentación de clientes
##########################################################################

rm(list = ls())
set.seed(1973)

if(!require(tidyverse)) { install.packages('tidyverse', dependencies = T) }
library(tidyverse)





# Cargamos los datos de compras en el data frame "data"
data <- read_tsv("purchases.txt", col_names = FALSE)
head(data)

# Añadimos al data frame los nombres de las variables
colnames(data) = c('customer_id', 'purchase_amount', 'date_of_purchase')
head(data)


data <- data %>% 
  mutate(days_since = as.numeric(difftime(time1 = "2016-01-01", time2 = date_of_purchase, units = "days"))) 

customers <- data %>% 
  group_by(customer_id) %>% 
  summarise(recency = min(days_since),
            frequency = n(),
            amount = mean(purchase_amount))

new_customers <- customers %>% 
  select(-customer_id)

new_customers <- scale(new_customers)

desired_segments <- 5

segments <- kmeans(new_customers, desired_segments)

customers <- customers %>% 
  mutate(segment=segments$cluster)

table(customers$segment)

write_csv(customers, "segmented_customers.csv")

# Guardamos nuestro modelo
saveRDS(segments, "kmeans.obj")


