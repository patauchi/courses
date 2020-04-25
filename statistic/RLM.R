# Librerias
library(readr)

# Cargamos datos
dataManuel <- read_csv('https://raw.githubusercontent.com/patauchi/rstructBio/master/dataTest/dataManuel.csv')
dataManuel <- as.data.frame(dataManuel)

# Nombres de las variables
names(dataManuel)

# Cabecera
head(dataManuel)

#########  Regresion Lineal Multiple #####
#### 0. Estructura de datos ####
dataManuel_fit <- dataManuel[, c('Region', 'WTD', 'Fertility','Mortality','CWD.mean.all','preci_drie_3',
                                 'preci_anual','SAND-0.30-','CLAY-0.30-')]


# nombres
names(dataManuel_fit)

# Renombrar las columnas de las variables
colnames(dataManuel_fit) <- c('Region', 'ProfundidadNapaFreatica','Fertilidad','Mortalidad','EstresHidrico',
                          'PrecipitacionesMesesSecos','PrecipitacionAnual','Arena','Arcilla')

names(dataManuel_fit)

# Transformar Variable de Repuesta
dataManuel_fit$Region

# Extraer codigos unicos
unique(dataManuel_fit$Region)

# Recategorizando
dataManuel_fit$Region[dataManuel_fit$Region == 'GS'] <- 1
dataManuel_fit$Region[dataManuel_fit$Region == 'SWA'] <- 2
dataManuel_fit$Region[dataManuel_fit$Region == 'NWA'] <- 3
dataManuel_fit$Region[dataManuel_fit$Region == 'SA'] <- 4
dataManuel_fit$Region[dataManuel_fit$Region == 'EA'] <- 5
dataManuel_fit$Region[dataManuel_fit$Region == 'CA'] <- 6


# 
class(dataManuel_fit$Region)

# Convertir clase letra a numeros
dataManuel_fit$Region <- as.numeric(dataManuel_fit$Region)

class(dataManuel_fit$Region)

# Vista general
View(dataManuel_fit)


#### 1. Modelo sin especificaciones ####

# RegresionLineal
# lm(VR ~ VP)
#       funcion(VR    ~   VP )
(ModeloLM <- lm(Region ~ ProfundidadNapaFreatica, data=dataManuel_fit))

Modelo0 <- lm(Region ~ ProfundidadNapaFreatica + Fertilidad + Mortalidad + 
                EstresHidrico + PrecipitacionesMesesSecos + PrecipitacionAnual+Arena+Arcilla,
              data=dataManuel_fit)

Modelo0

summary(Modelo0)
#### 2. Multicolinearidad / Collinearidad ####
# 
devtools::install_github('ggobi/ggally')
cor(dataManuel_fit)
library(GGally)

ggpairs(dataManuel_fit)

# Variables correlacionadas (r > 0.75); r > 0.85; r > 0.9
# Eliminar variables:
# Fuente: Interpolados   ---  Interpolados 
# PrecipitacionesAnuales, PrecipitacionesMesesSecos
names(dataManuel_fit)

dataFit <- dataManuel_fit[ , -c(6,7)]

names(dataFit)

#### 3. Calibracion de modelos #####

Modelo1 <- lm(Region ~  ProfundidadNapaFreatica + Fertilidad + Mortalidad + 
                EstresHidrico+Arena+Arcilla,
              data=dataFit)

summary(Modelo1)

Modelo2 <- lm(Region ~ EstresHidrico + PrecipitacionesMesesSecos + Arena, 
              data=dataManuel_fit)

summary(Modelo2)

### Seleccion de variables
vif_func(dataManuel_fit, thresh = 2)

# VIF: Inflación de varianza -> Region, ProfundidadNapaFreatica, 
#                               Fertilidad, EstresHidrico, Arena

Modelo3 <- lm(Region ~ ProfundidadNapaFreatica + Fertilidad + EstresHidrico + Arena,
             data=dataManuel_fit)

summary(Modelo3)

# Particion Jerarquica
# library(betapart)

## Modelo de variables interacuantes

# 10 - 25
# Temperatura:Altitud  Temperatura:Precipitacion
Modelo4 <- lm(Region ~ Arena + Arcilla + Fertilidad + Mortalidad + 
                Fertilidad:Mortalidad + ProfundidadNapaFreatica:EstresHidrico,
              data=dataManuel_fit)

Modelo5 <- lm(Region ~ Arena + Arcilla + Fertilidad + Mortalidad + 
                Fertilidad:Mortalidad,
              data=dataManuel_fit)

Modelo6 <- lm(Region ~ Arena + Fertilidad:Mortalidad,
              data=dataManuel_fit)

#### 4. Seleccion de modelos ####
# Criterio de Informacion de Akaike
# Reducir la complejidad de los modelos
AIC(Modelo0)
AIC(Modelo1)
AIC(Modelo2)
AIC(Modelo3)
AIC(Modelo4)
AIC(Modelo5)
AIC(Modelo6)

#
data.frame(AIC(Modelo0),
           AIC(Modelo1),
           AIC(Modelo2),
           AIC(Modelo3),
           AIC(Modelo4),
           AIC(Modelo5),
           AIC(Modelo6))

# Modelo seleccionado debe ser el Modelo2
# Modelo de regresion : 
Modelo2

plot(Modelo2)

# 
##################








