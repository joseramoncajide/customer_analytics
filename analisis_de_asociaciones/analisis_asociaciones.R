##########################################################################
# Jose Cajide - @jrcajide
# Customer Analytics: Análisis de asociaciones - carrito de la compra
##########################################################################

# Objetivo: Entender las preferencias de compra de los clientes. También para encontrar otro tipo de asociaciones


rm(list=ls()) 
cat("\014")

list.of.packages <- c("arules", "arulesViz")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(arules)
library(arulesViz)

df <- read.csv('analisis_de_asociaciones/data/compras.csv')
transactions <- as (df, "transactions")
inspect(transactions)


# Support=Number of transactions with both A and B / Total number of transactions
# Confidence=Number of transactions with both A and B / Total number of transactions with A
# ExpectedConfidence=Number of transactions with B / Total number of transactions
# Lift=Confidence / Expected Confidence

frequentItems <- eclat (transactions, parameter = list(supp = 0.07, maxlen = 15)) # calculates support for frequent items


inspect(frequentItems)
itemFrequencyPlot(transactions, topN=10, type="absolute", main="Item Frequency")

# Recomendaciones de productos
rules <- apriori (transactions, parameter = list(supp = 0.001, conf = 0.5)) # Min Support as 0.001, confidence as 0.8.
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf))

plot(head(rules_conf, 10), method="graph")

rules_lift <- sort (rules, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_lift))


# Eliminar reglas redundantes

subsetRules <- which(colSums(is.subset(rules, rules)) > 1) # get subset rules in vector
length(subsetRules) 
rules <- rules[-subsetRules] 

rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf))

rules_lift <- sort (rules, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_lift))

# Cómo encontrar reglas referentes a un producto: ¿Qué productos ha comprado con antoridad un cliente que ha comprado margarina?

rules <- apriori (data=transactions, parameter=list (supp=0.001,conf = 0.08), appearance = list (default="lhs",rhs="margarine="), control = list (verbose=F))
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf))

# Ej. Prueba con otros productos

# Qué productos han comprado tras comprar ... margarina

rules <- apriori (data=transactions, parameter=list (supp=0.001,conf = 0.15,minlen=2), appearance = list(default="rhs",lhs="margarine="), control = list (verbose=F)) # those who bought 'milk' also bought..
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf))



