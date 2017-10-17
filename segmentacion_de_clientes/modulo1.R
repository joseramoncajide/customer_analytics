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

if(!require(viridis)) { install.packages('viridis', dependencies = T) }
library(viridis)

if(!require(cluster)) { install.packages('cluster', dependencies = T) }
library(cluster)



# Cargamos los datos de compras en el data frame "data"
data <- read_tsv("purchases.txt", col_names = FALSE)
head(data)

# Añadimos al data frame los nombres de las variables
colnames(data) = c('customer_id', 'purchase_amount', 'date_of_purchase')
head(data)

# Nos situamos en 2016-01-01
# Añadimos una nueva variable  con los días que han pasado desde la compra hasta 2016-01-01
data <- data %>% 
  mutate(days_since = as.numeric(difftime(time1 = "2016-01-01", time2 = date_of_purchase, units = "days"))) 

head(data)
summary(data)

# Calculamos las varibles recency, frequency y average purchase amount de cada cliente
customers = sqldf("SELECT customer_id,
                  MIN(days_since) AS 'recency',
                  COUNT(*) AS 'frequency',
                  AVG(purchase_amount) AS 'amount'
                  FROM data GROUP BY 1")

head(customers)

customers <- data %>% 
  group_by(customer_id) %>% 
  summarise(recency = min(days_since),
            frequency = n(),
            amount = mean(purchase_amount))

head(customers)


# Exploramos los datos de clientes ----------------------------------------

summary(customers)
hist(customers$recency)
hist(customers$frequency)
hist(customers$amount)
hist(customers$amount, breaks = 100)



# PPT ---------------------------------------------------------------------



# Segmentación estadística de clientes ------------------------------------


# Hacemos una copia de "customers" en "new_data"
new_data <- customers
head(new_data)

# No podemos incluir "customer_id" como variable de segmentación, pero la guardamos para más adelante
customer_ids <- new_data %>% 
  select(customer_id) %>% 
  pull()
head(customer_ids, 30)

# Eliminamos la variable "customer_id"
new_data <- new_data %>% 
  select(-customer_id)

head(new_data)

# Comprobamos la distribución de clientes según la varible "amount"
hist(new_data$amount)

# Tranformación de los datos

# Tranformamos la variable "amount" pasándola a escala logarítmica
new_data <- new_data %>% 
  mutate(amount = log(amount))

hist(new_data$amount)

# Necesitamos centrar y escalar los datos, estandarizar las unidades para hacerlas comparables
new_data = scale(new_data)
head(new_data)


# PPT ---------------------------------------------------------------------

# Calculamos la distancia euclídea
# OJO: Esto puede generar un error por falta de memoria
length(customer_ids) * length(customer_ids) 
d <- dist(new_data)

# Podemos realizar la segmentación sobre una muestra aletaria de clientes
nrow(new_data)
sample <- seq(1, 18417, by = 10)
head(sample)
customers_sample = customers[sample, ]
new_data_sample  = new_data[sample, ]

# Calculamos la distancia euclídea
d <- dist(new_data_sample)

# Realizamos la segmentación jerárquica sobre las distancias
c <- hclust(d, method="ward.D2")

# Dibujamos el dendograma
plot(c)

# Generamos 9 segementos
members <- cutree(c, k = 9)

# Mostramos los 30 primeros clientes y la tabla de frecuencias
members[1:30]
table(members)
barplot(table(members))

# Mostramos las características de cada segmento
customers_segements <- customers_sample %>%
  mutate(cluster = members) %>% 
  select(-customer_id) %>% 
  group_by(cluster) %>% 
  summarise(customers=n(),
            recency = mean(recency), 
            frequency = mean(frequency),
            amount = mean(amount)
            )

head(customers_segements)

# Cambiamos el formato del data frame para poder dibujar los datos
customers_segements <- customers_segements %>% gather(variable, value, -cluster)
head(customers_segements)
# customers_segements.melt <- reshape2::melt(customers_segements, id.vars = c("cluster"), measure.vars = c("recency", "frequency", "amount"))

ggplot(customers_segements, aes(x = factor(cluster), y = value, fill= factor(cluster))) + geom_bar(stat="identity", colour="white") +  facet_wrap(~ variable, scales = "free", nrow = 1) + labs(x=NULL, y=NULL, title="Características de cada segmento")  + scale_fill_brewer(palette = "RdYlGn") + theme_bw()

# Creamos una nueva variable con el número de cluster asignado anteriormente a cada cliente
customers_asigned_clusters <- customers_sample %>%
  mutate(cluster = members)

head(customers_asigned_clusters)

# Visualizamos como varía la frecuencia y la recencia por cada segemento
p1 <- ggplot(customers_asigned_clusters, aes(x=frequency, y= recency, color=factor(cluster))) + geom_point() + geom_jitter() 
p1 <- p1 + scale_fill_brewer(palette = "RdYlGn")
p1 <- p1 +xlab("Frecuencia")+ylab("Recencia")
p1 <- p1+ ggtitle("Frecuencia vs Recencia")+theme_bw()
p1 <- p1 + theme(axis.text=element_text(size=8), axis.title=element_text(size=8),plot.title = element_text(size=10))
p1 <- p1 +guides(colour=FALSE)
p1


# PPT ---------------------------------------------------------------------

# SEGMENTACIÓN KMEANS -----------------------------------------------------


wssse <- (nrow(new_data_sample)-1)*sum(apply(new_data_sample,2,var))
for(i in 2:15) wssse[i]<- sum(fit=kmeans(new_data_sample,centers=i,15)$withinss)
plot(1:15,wssse,type="b",main="Testing for 15 clusters",xlab="Number of clusters",ylab="Within Set Sum of Squared Error", col=viridis(10))

desired_segments <- 5

fit <- kmeans(new_data_sample, desired_segments)

fit$centers

attributes(new_data)

fit$centers * attr(new_data, 'scaled:scale') + attr(new_data, 'scaled:center') 

plot(new_data_sample,col=fit$cluster,pch=15, main="Clustering", xlab = "Recency", ylab = "Frecuency")
points(fit$centers,pch=4, cex = 1.9, col=viridis(desired_segments))

customers_asigned_clusters <- data.frame(customers[sample, ], segment=fit$cluster)

head(customers_asigned_clusters)

# Comprobamos las características de cada segmento
customers_asigned_clusters %>% 
  select(-customer_id) %>% 
  group_by(segment) %>% 
  summarise(recency = mean(recency), frequency = mean(frequency), amount = mean(amount), num_customers=n())



# CASO PRACTICO APLICACION SEGMENTACION KMEANS ----------------------------

# kmeans_parte1.R y kmeans_parte2.R