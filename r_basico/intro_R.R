##########################################################################
# Jose Ramón Cajide, 2017-10
# Customer Analytics: R básico
##########################################################################


# R como calculadora ------------------------------------------------------

1 + 100
1 + 4 + 5 * 2
2/10000
2 * 10^(-4)
5e3

# Comparando variables ----------------------------------------------------

1 == 1 
1 != 2
1 < 2
1 <= 1
1 >= -9

# obteniendo ayuda --------------------------------------------------------

?round

# Funciones matemáticas ---------------------------------------------------

pi
round(pi, 0)
round(pi, 4)
floor(pi)
cos(pi) #sin #tan
abs(cos(pi))
log(10)

exp(0.5)


# Directorio de trabajo ---------------------------------------------------

getwd()
dir <- "/" #usar la tecla tabulador tab para completar
dir <- "~/Documents/GitHub/customer_analytics/"
dir
setwd(dir)
getwd()

list.files()

### Entorno
ls()  
x <- 10
y <- 20
x + y
ls()  
rm(y)
y
ls()
rm(list = ls())


# Tipos de variables ------------------------------------------------------

x <- "Hola!"
class(x)

(y <- 1:10)
class(y)
length(y)

## ------------------------------------------------------------------------

# usando secuencias

1:10
seq(10)
seq(1, 10, by = 0.1)





# Matrices ----------------------------------------------------------------

m <- matrix(nrow = 2, ncol = 2)
m
dim(m)

# Se rellenan por columnas
m <- matrix(1:6, nrow = 2, ncol = 3)
m

# Otras formas de construir matrices
x <- 1:3
y <- 10:12
(cbind(x, y))

# ó
(rbind(x, y))


## ------------------------------------------------------------------------
xlist <- list(a = "Conjunto de datos iris", b = 1:10, data = head(iris))
xlist
xlist$b


# variables categóricas ---------------------------------------------------

tallas <- factor(c("s", "xl", "m", "m", "xl", "s", "m", "xl", "m"))
levels(tallas)
length(levels(tallas))

min(tallas)


## ------------------------------------------------------------------------

tallas <- factor(tallas, levels=c("s", "m", "xl"), ordered = TRUE)
levels(tallas)
min(tallas) ## Ahora sí
max(tallas)

## ------------------------------------------------------------------------
table(tallas)
barplot(table(tallas))


# dataframes --------------------------------------------------------------

df <- data.frame(id = letters[1:10], x = 1:10, y = rnorm(10))
df
# agregar nuevas variables
cbind(df, data.frame(z = 4))

# acceder a una columna como vector
df$x

## Prueba las siguientes funciones sobre el anterior dataframe
## head()
## tail()
## dim()
## nrow()
## ncol()
## str()
## names()



# Paquetes R --------------------------------------------------------------
# https://github.com/qinwf/awesome-R
installed.packages()
install.packages("tidyverse")
# update.packages()
library(tidyverse)
