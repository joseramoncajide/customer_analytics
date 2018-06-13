##########################################################################
# Jose Cajide - @jrcajide
# Customer Analytics: Aprendinzaje no supervisado: clustering
##########################################################################


# http://archive.ics.uci.edu/ml/datasets/Wholesale+customers

library(tidyverse)

customers <- read_csv('http://archive.ics.uci.edu/ml/machine-learning-databases/00292/Wholesale%20customers%20data.csv')

head(customers)

summary(customers)

customers$Channel <- as.character(customers$Channel)
customers$Region <- as.character(customers$Region)

table(customers$Channel)/nrow(customers)

library(GGally)
# ggpairs(customers)

library(corrplot)
customers %>% 
  select(Fresh:Delicassen) %>% 
  cor() %>% 
  corrplot(method = "number")

# Distancias
source('estadistica_y_modelos/distancias.R')

# Escalar y centrar las variables de forma que todas ellas tengan media 0 y desviación estándar 1
customers_scaled <- customers %>% 
  select(Fresh:Delicassen) %>% 
  scale()



# Elección del número de clusters óptimo
wss <- (nrow(customers_scaled)-1)*sum(apply(customers_scaled,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(customers_scaled, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Número de clusters",
     ylab="Suma de los cuadrados internos")


fit <- kmeans(customers_scaled, 4) 

library(ggfortify)
library(cluster)
autoplot(fit,data = customers, frame = T, frame.type = 'norm')

clusters <- fit$cluster
table(clusters)

# Asiganación de clusters a los clientes
customers <- customers %>% 
  select(Fresh:Delicassen) %>% 
  mutate(cluster = as.character(clusters)) 

customers

# Visualización
ggplot(data = customers, aes(x = Fresh, y = Milk, color = as.factor(cluster))) +
  geom_point(size = 3) +
  labs(title = "Clustering") +
  theme_bw() +
  theme(legend.position = "none")

ggplot(data = customers, aes(x = Delicassen, y = Grocery, color = as.factor(cluster))) +
  geom_point(size = 3) +
  labs(title = "Clustering") +
  theme_bw() +
  theme(legend.position = "none")

summary <- customers %>% 
  group_by(cluster) %>% 
  summarise_all(mean)

summary %>% 
  gather(key='product', value = 'units', -cluster) %>% 
  ggplot(aes(x=product, y=units, fill=product)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~cluster) +
  theme_bw() +
  theme(legend.position = "bottom")


# Clústering jerárquico...

d <- dist(customers_scaled, method = "euclidean")
h_clusters <- hclust(d, method="ward.D") 
plot(h_clusters)
clusters <- cutree(h_clusters, k=4)
table(clusters)
rect.hclust(h_clusters, k=4, border="red")

library(factoextra)
fviz_dend(x = h_clusters, k = 4, cex = 0.6) +
  labs(title = "Clústering jerárquico",
       subtitle = "Distancia euclídea")

summary_hclust <- customers %>% 
  select(Fresh:Delicassen) %>% 
  mutate(cluster = as.character(clusters)) %>% 
  group_by(cluster) %>% 
  summarise_all(mean)

summary_hclust %>% 
  gather(key='product', value = 'units', -cluster) %>% 
  ggplot(aes(x=product, y=units, fill=product)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~cluster) +
  theme_bw() +
  theme(legend.position = "bottom")

