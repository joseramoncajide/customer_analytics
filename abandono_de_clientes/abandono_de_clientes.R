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

# Número de clientes
nrow(df)

# Curva de supervivencia
head(Surv(df$time, df$churned), 11)

fit <- survfit(Surv(df$time, df$churned) ~ 1, data = df)
summary(fit)

ggsurvplot(fit)

# Zoom
ggsurvplot(fit, 
           palette = "#2E9FDF", 
           ylim=c(.75,1),
           xlab = 'Días desde la subscripción', 
           ylab = '% Supervivencia')

ggsurvplot(fit, legend = "bottom", 
           palette = "#2E9FDF", 
           legend.title = "Todos los clientes",
           conf.int = TRUE,
           ylim=c(.75,1), lty = 1:2, mark.time = FALSE,
           xlab = 'Días desde la subscripción', 
           ylab = '% Supervivencia',
           risk.table = TRUE, 
           risk.table.y.text.col = T)


# Segementación por sexo
fit2 <- survfit(Surv(time, churned) ~ gender, data = df)
ggsurvplot(fit2)

# Zoom
ggsurvplot(fit2, 
           palette = c("#2E9FDF","pink"), 
           legend = "bottom", 
           legend.title = "Género",
           conf.int = TRUE,
           pval = TRUE,
           ylim=c(.75,1), lty = 1:2, mark.time = FALSE,
           xlab = 'Días desde la subscripción', ylab = '% Supervivencia',
           legend.labs = c("Hombres", "Mujeres"))


# Añadir la tabla de riesgo
ggsurvplot(fit2, 
           palette = c("#2E9FDF","pink"), 
           legend = "bottom", 
           legend.title = "Género",
           conf.int = TRUE,
           pval = TRUE,
           ylim=c(.75,1), lty = 1:2, mark.time = FALSE,
           xlab = 'Días desde la subscripción', ylab = '% Supervivencia',
           legend.labs = c("Hombres", "Mujeres"),
           risk.table = TRUE, risk.table.y.text.col = TRUE) 



# ¿Podemos predecir el abandono? ------------------------------------------


churn <- read.csv("abandono_de_clientes/data/historial_clientes.csv")
head(churn)

ggplot(churn, aes(x=Account.Length, fill=Churned)) +
  geom_density() + 
  facet_grid(Churned ~ .) + 
  labs(title="Duración de la subscripción del cliente")

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
confusionMatrix(model,positive="True.")



pred <- predict(model, newdata=churnTest)

# Cuántos han abandonado? (churnTest)
table(churnTest$Churned)
# Cuantos ha estimado el modelo?
table(pred)

confusionMatrix(pred, churnTest$Churned, positive="True.")

# importancia de las variables
importance <- varImp(model, scale=FALSE)
plot(importance)


