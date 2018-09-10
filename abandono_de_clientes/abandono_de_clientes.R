##########################################################################
# Jose Cajide - @jrcajide
# Customer Analytics: Abandono de clientes
##########################################################################

rm(list=ls()) 
cat("\014")

list.of.packages <- c("tidyverse", "survival", "devtools", "caret")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Install packages
install.packages("survival")
install.packages("tidyverse")
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/survminer")

# Load packages
library(survival) 
library(tidyverse)
library(survminer)
library(caret)


# Entendiendo el abandono de clientes. ¿Qué sucede? -----------------------



# Carga de datos
df <- read_csv('abandono_de_clientes/data/churn.csv')
head(df,11)


# Curca de supervivencia
fit <- survfit(Surv(time, churned) ~ 1, data = df)

ggsurvplot(fit)

# Zoom
g1 <- ggsurvplot(fit, 
                 color = "#2E9FDF", 
                 ylim=c(.75,1),
                 xlab = 'Days since subscription', 
                 ylab = '% Survival')
g1


# Segementación por sexo
fit2 <- survfit(Surv(time, churned) ~ gender, data = df)
ggsurvplot(fit2)

# Zoom
g2 <- ggsurvplot(fit2, legend = "bottom", 
                 legend.title = "Gender",
                 conf.int = TRUE,
                 pval = TRUE,
                 ylim=c(.75,1), lty = 1:2, mark.time = FALSE,
                 xlab = 'Days since subscription', ylab = '% Survival',
                 legend.labs = c("Male", "Female"))
g2

# Añadir la tabla de riesgo
g3 <- ggsurvplot(fit2, legend = "bottom", 
                 legend.title = "Gender",
                 conf.int = TRUE,
                 pval = TRUE,
                 ylim=c(.75,1), lty = 1:2, mark.time = FALSE,
                 xlab = 'Days since subscription', ylab = '% Survival',
                 legend.labs = c("Male", "Female"),
                 risk.table = TRUE, risk.table.y.text.col = TRUE)
g3



# ¿Podemos predecir el abandono? ------------------------------------------


churn <- read.csv("abandono_de_clientes/data/historial_clientes.csv")
head(churn)

ggplot(churn, aes(x=Account.Length, fill=Churned))+geom_density()+ facet_grid(Churned ~ .) + labs(title="Account Length")

# Ej. Comprobar otras variables de manera similar a la anterior


# Modelado

set.seed(12)
trainIndex <- caret::createDataPartition(churn$Churned, p = .75, list = FALSE, times = 1)
churnTrain <- churn[ trainIndex,]
churnTest <- churn[-trainIndex, ]

churnTrain$Phone<-NULL
churnTest$Phone<-NULL
churnTrain$Area.Code<-as.factor(churnTrain$Area.Code)
churnTest$Area.Code<-as.factor(churnTest$Area.Code)

table(churnTrain$State)

table(churnTrain$Churned)
table(churnTest$Churned)

# Modelado con random forest
# http://www.r2d3.us/visual-intro-to-machine-learning-part-1/

control <- trainControl(method="cv", number=5, verboseIter = TRUE)

model <- train(Churned~., data=churnTrain, method="rf", trControl=control)

# https://docs.google.com/presentation/d/14ac22V-8Y-69JzBW8FGwxJGqpOhg00pLmdr86cAWFF8/edit#slide=id.p9
confusionMatrix(model)

pred <- predict(model, newdata=churnTest)
confusionMatrix(pred, churnTest$Churned)

# importancia de las variables
importance <- varImp(model, scale=FALSE)
plot(importance)


