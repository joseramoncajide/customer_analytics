##########################################################################
# Jose Ramón Cajide, 2017-10
# Customer Analytics: Satisfacción de clientes
##########################################################################

rm(list=ls())

# Cargamos las librerías necesarias ---------------------------------------
if(!require(qcc)) { install.packages('qcc', dependencies = T) }
library(qcc)
if(!require(FactoMineR)) { install.packages('FactoMineR', dependencies = T) }
library(FactoMineR)
if(!require(factoextra)) { install.packages('factoextra', dependencies = T) }
library(factoextra)



# Importamos los datos
df <- read.csv('satisfaccion_clientes/encuesta.txt', sep = '\t', stringsAsFactors = F, dec = ',')

# Comprobación de los datos importados
head(df)

# Damos un nombrado signifactivo a las mismas para  facilitar el análisis y su visualización
colnames(df) <- c('Satis_gen', 
                  'Estado',
                  'Embalaje',
                  'Cumpli_plazo_entrega',
                  'Calidad_prod',
                  'Trato_teleoperadora',
                  'Ases_repartidor',
                  'Plazo_entrega_est',
                  'Costes_env',
                  'Acabado',
                  'Correspondencia_web',
                  'Actitud_rep')

# Inspeccionamos las variables
summary(df)

# Reducción de todas las variables en un número de factores más reducido para tener una información más sintética.

# Análisis de componentes principales. 
# Decidimos en número de componentes pricipales o variables sintéticas a generar a partir de las 12 variables. 
# Seleccionamos el número de factores para los que la varinza retenida es superior a 1

varianzas <- prcomp(scale(df))$sdev^2  # varianzas

pareto.chart (varianzas, ylab="Varianza", main = 'Varianzas retenidas por los factores', sub="Dos factores presentan valores mayores que 1.")

# Ejecutamos el análisis de componentes principales indicando que reduzca las variables a dos factores o variables sintéticas
res.pca <- PCA(df[2:ncol(df)],scale.unit=T, ncp=2, graph = F)

# Resultado del análisis:
summary(res.pca)

# Autovalores
autovalores <- res.pca$eig 
head(autovalores)

# Visualizaciones
fviz_screeplot (res.pca, ncp=10) + theme_minimal()  + ggtitle('% de la varianza explicada por cada factor') 

fviz_pca_var(res.pca, jitter = list(what = "label", width = .4, height = .2)) + theme_minimal()  + ggtitle('Mapa de factores')

fviz_contrib(res.pca, choice = "var", axes = 1) +  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))   + labs(title='Qué variables contribuyen más al factor 1') 

fviz_contrib(res.pca, choice = "var", axes = 2) +  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))   + labs(title='Qué variables contribuyen más al factor 2') 

fviz_contrib(res.pca, choice = "var", axes = 1:2) +  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))   + labs(title='Qué variables contribuyen más a amnbos factores') 


knitr::kable(as.data.frame(res.pca$var$contrib))



# Regresión ---------------------------------------------------------------

# Modelamos la satisfacción en función del resto de varibles del modelo
modelo.lineal <- lm(Satis_gen ~ ., data = df) 

# Resultado del modelo
summary(modelo.lineal)

