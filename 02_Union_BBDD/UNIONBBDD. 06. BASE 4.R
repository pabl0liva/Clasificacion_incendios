
## ----------------------------------------------------------- ##
## ----------------------------------------------------------- ##
## -------------- LIMPIEZA BBDD INCENDIOS (BASE) ------------- ##
## ----------------------------------------------------------- ##
## ----------------------------------------------------------- ##




## ----------------------------------------------------------- ##
## ---------------- LECTURA DE DATOS ------------------------- ## 
## ----------------------------------------------------------- ##

#(encoding = "UTF-8")


# setwd("C:/Users/nerea/OneDrive/Desktop/Master/TFM")
setwd("/Users/samantha/Documents/Master/TFM/BASE/1" )

datos_ori = read.csv("incendios_fin.csv")
datos = datos_ori

#Dimensión de datos 
#82620    41
dim(datos)
summary(datos)




## ----------------------------------------------------------- ##
## ---------------- CARGA DE LIBRERÍAS ----------------------- ## 
## ----------------------------------------------------------- ##

source("Funciones_R.R")

library(questionr)
library(psych)
library(car)
library(corrplot)
library(dplyr)
library(inspectdf)
library(ggplot2)
library(DataExplorer)
library(ranger)
library(caret)
library(forcats)
library(tidyr)
library(visdat)
library(naniar)
library(stringr)




## ----------------------------------------------------------- ##
## ---------------- TRANSFORMACIONES ----------------------- ## 
## ----------------------------------------------------------- ##

# Pasar los datos correspondientes a numeric, character y date: 

print(miss_var_summary(datos), n = 41)

# 1 dir           13200     16.0 
# 2 idcomunidad    6171     7.47
# 3 idprovincia    6171     7.47
# 4 indsinop       2317     2.80
# 5 altitud        1759     2.13
# 6 latitud        1759     2.13
# 7 longitud       1759     2.13

datos$tmed = as.numeric(gsub(",", ".", datos$tmed))
datos$tmin = as.numeric(gsub(",", ".", datos$tmin))
datos$tmax = as.numeric(gsub(",", ".", datos$tmax))
datos$velmedia = as.numeric(gsub(",", ".", datos$velmedia))
datos$racha = as.numeric(gsub(",", ".", datos$racha))
# datos$sol = as.numeric(gsub(",", ".", datos$sol))
datos$presMax = as.numeric(gsub(",", ".", datos$presMax))
datos$presMin = as.numeric(gsub(",", ".", datos$presMin))

# Para AEMET precipitación Ip significa precipitación inapreciable, es decir, cantidad inferior a 0.1 mm.
datos$prec[datos$prec == 'Ip'] <- 0.0
datos$prec = as.numeric(gsub(",", ".", datos$prec))

datos[,c('id','idmunicipio','time_ctrl','time_ext','personal', 'medios', 'idprovincia', 'dir', 'indsinop', 'idcomunidad', 'altitud')] <- lapply(
  datos[,c('id','idmunicipio','time_ctrl','time_ext','personal', 'medios', 'idprovincia', 'dir', 'indsinop',  'idcomunidad', 'altitud')], as.numeric)

datos[,as.vector(which(sapply(datos, class) == "character"))] <- lapply(
  datos[,as.vector(which(sapply(datos, class) == "character"))] , factor)

datos$fecha = as.Date(datos$fecha, format = "%Y-%m-%d")


#plot dimensión de columnas, observaciones y row
introduce(datos)
plot_intro(datos)

# Eliminamos sol debido a la cantidad de nulos, tipoincendio por colinealidad con superficie y indsinop por no ser relevante: 

datos$sol <- NULL
#datos$tipoincendio <- NULL # Antes de borrar habrá que verlo, ¿ya está vista la colinealidad   antes, no aporta nada en ningún modelo?
datos$indsinop <- NULL 

#Eliminamos las variables de hora
datos$horaPresMax <- NULL
datos$horaPresMin <- NULL
datos$horaracha <- NULL
datos$horatmax <- NULL
datos$horatmin <- NULL

## ----------------------------------------------------------- ##
## ----------- NULOS y VALORES "" ---------------------------- ## 
## ----------------------------------------------------------- ##

print(miss_var_summary(datos), n = 41)

# 1 presMax       29179    35.3 
# 2 presMin       29178    35.3 
# 3 dir           13200    16.0 
# 4 racha         13200    16.0 
# 5 velmedia      11335    13.7 
# 6 idcomunidad    6171     7.47
# 7 idprovincia    6171     7.47
# 8 prec           5196     6.29
# 9 tmed           4692     5.68
# 10 tmin           4683     5.67
# 11 tmax           4660     5.64
# 12 altitud        1759     2.13
# 13 latitud        1759     2.13
# 14 longitud       1759     2.13


## ----------- VALORES "" ---------------------------- ## 

colSums(datos == "", na.rm = TRUE)

#  nombre    provincia_y          
#   1759         1759          

# Procedemos a eliminar las filas donde ahora se encuentran valores RR en la variable nombre, 
# estas filas coinciden con NAs en las variables:
# provincia_y, altitud, latitud y longitud, componen un 2,19% del total del DF (1759 registros)

# El resto de valores "" aparecen en las variables horarias (horatmin, horatmax, horaracha, horapresmax y horapresmin)

datos <- datos[!(datos$nombre == ""), ]


#    horatmin          
#      5059        
# horatmax    horaracha     horaPresMax     horaPresMin 
# 5013          13260         29191            29194 

#Nuevo estado de las variables
dim(datos)
str(datos)
summary(datos)

#Plot missing
vis_miss(datos, warn_large_data = FALSE)
plot_missing(datos)

## ----------------------------------------------------------- ##
## ----------- IMPUTACIÓN DE NULOS ---------------------------- ## 
## ----------------------------------------------------------- ##
#Vemos los datos nulos:

colSums(is.na(datos), na.rm = TRUE)

# idcomunidad  idprovincia   
#  6111         6111            
# tmed         prec         tmin     tmax      dir     velmedia 
# 2933         3437         2924     2901     11441      9576 
# racha    presMax  presMin  
# 11441    27420    27419     



#----------- TEMPERATURAS:
# Imputamos las temperaturas mínimas y máxias según 
# la media por provincia y mes: (MEDIA)

# Incluimos la variable mes usando la variable fecha: 
datos$fe_month <- as.numeric(format(datos$fecha,'%m'))


# CÁLCULO TEMPERATURA MÍNIMA: 
tmin_sample = datos[ ,c("id", "fe_month", "provincia_x", "tmin")]
tmin_sample <- tmin_sample %>%
  unite("mesprov", fe_month:provincia_x, sep = "/")

# Registros con y sin NA de las variables month, prov y tmin:
tmin_sample_sinNA = tmin_sample %>% drop_na(tmin)
tmin_sample_NA = tmin_sample %>% filter(is.na(tmin))
tmin_sample_NA$tmin <- NULL

# Media temperatura mínima:
media_tmin = tmin_sample_sinNA %>%
  group_by(mesprov) %>%
  summarise(tmin = mean(tmin))
media_tmin = as.data.frame(media_tmin)

final_tmin <- merge(tmin_sample_NA, media_tmin, by = "mesprov")
final_tmin$mesprov <- NULL

datos$tmin[match(final_tmin$id, datos$id)] <- final_tmin$tmin
datos$tmin <- round(datos$tmin, digits = 2)

# Antes:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -16.000   5.600  10.400   9.873  14.800  30.000    2924 

# Despés: 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -16.000   5.600  10.400   9.877  14.800  30.000 



# CÁLCULO TEMPERATURA MÁXIMA:
tmax_sample = datos[ ,c("id", "fe_month", "provincia_x", "tmax")]
tmax_sample <- tmax_sample %>%
  unite("mesprov", fe_month:provincia_x, sep = "/")

# Registros con y sin NA de las variables month, prov y tmax:
tmax_sample_sinNA = tmax_sample %>% drop_na(tmax)
tmax_sample_NA = tmax_sample %>% filter(is.na(tmax))
tmax_sample_NA$tmax <- NULL

# Media temperatura mínima:
media_tmax = tmax_sample_sinNA %>% 
  group_by(mesprov) %>%
  summarise(tmax = mean(tmax))
media_tmax = as.data.frame(media_tmax)

final_tmax <- merge(tmax_sample_NA, media_tmax, by = "mesprov")
final_tmax$mesprov <- NULL 

datos$tmax[match(final_tmax$id, datos$id)] <- final_tmax$tmax
datos$tmax <- round(datos$tmax, digits = 2)

# Antes:
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -10.40   16.50   22.00   22.56   28.60   46.40    2901 

# Despés: 
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -10.40   16.60   22.00   22.57   28.60   46.40 



# CÁLCULO TEMPERATURA MEDIA:
tmed_sample = datos[ ,c("id", "fe_month", "provincia_x", "tmed")]
tmed_sample <- tmed_sample %>%
  unite("mesprov", fe_month:provincia_x, sep = "/")

# Registros con y sin NA de las variables month, prov y tmed:
tmed_sample_sinNA = tmed_sample %>% drop_na(tmed)
tmed_sample_NA = tmed_sample %>% filter(is.na(tmed))
tmed_sample_NA$tmed <- NULL

# Media temperatura mínima:
media_tmed = tmed_sample_sinNA %>% 
  group_by(mesprov) %>%
  summarise(tmed = mean(tmed))
media_tmed = as.data.frame(media_tmed)

final_tmed <- merge(tmed_sample_NA, media_tmed, by = "mesprov")
final_tmed$mesprov <- NULL 

datos$tmed[match(final_tmed$id, datos$id)] <- final_tmed$tmed
datos$tmed <- round(datos$tmed, digits = 2)

# Antes:
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  -12.20   11.40   16.30   16.22   21.20   36.20    2933 

# Despés: 
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  -12.20   11.40   16.30   16.22   21.20   36.20 






#----------- PRECIPITACIÓN:
#Imputacion de la precipitación en función media según de provincia y fecha
# CÁLCULO PRECIPITACIÓN MEDIA:
prec_sample = datos[ ,c("id", "fe_month", "provincia_x", "prec")]
prec_sample <- prec_sample %>%
  unite("mesprov", fe_month:provincia_x, sep = "/")

# Registros con y sin NA de las variables month, prov y tmed:
prec_sample_sinNA = prec_sample %>% drop_na(prec)
prec_sample_NA = prec_sample %>% filter(is.na(prec))
prec_sample_NA$prec <- NULL

# Media precipitación:
media_prec = prec_sample_sinNA %>%
  group_by(mesprov) %>%
  summarise(prec = mean(prec))
media_prec = as.data.frame(media_prec)

final_prec <- merge(prec_sample_NA, media_prec, by = "mesprov")
final_prec$mesprov <- NULL

datos$prec[match(final_prec$id, datos$id)] <- final_prec$prec
datos$prec <- round(datos$prec,digits = 1)


# Antes:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.000   0.000   0.000   0.653   0.000 155.900    5135 

# 0    62817 77.7 83.0
# 0.1   1225  1.5  1.6
# 0.2   1195  1.5  1.6
# 0.3    643  0.8  0.8
# 0.4    719  0.9  0.9
# 0.5    419  0.5  0.6

# Después: 
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.0000   0.0000   0.0000   0.6576   0.0000 155.9000    1 

# 0    63061 78.0 78.0
# 0.1   1578  2.0  2.0
# 0.2   1472  1.8  1.8
# 0.3   1372  1.7  1.7
# 0.4   1086  1.3  1.3
# 0.5   1004  1.2  1.2


# Imputamos un nulo que no se ha limpiado
#1 NA: esto se da porque justo de ese valor no había media, pues lo imputamos a 0 
datos$prec[is.na(datos$prec)] <- 0







#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.0000   0.0000   0.0000   0.6117   0.0000 155.9000 

# 0    67952 84.0 84.0
# 0.1   1225  1.5  1.5
# 0.2   1195  1.5  1.5
# 0.3    643  0.8  0.8
# 0.4    719  0.9  0.9
# 0.5    419  0.5  0.5



# Las medias son más parecidas en la transformación anterior, 
# pero las frecuencias son más parecidas en esta transformación.  





#----------- PRESIÓN:
# Probamos a imputar por los valores medios en cada mes/provincia, a ver cómo se comportan los datos:


# Presión Máxima: 

# CÁLCULO PRESIÓN MÁXIMA::
presmax_sample = datos[ ,c("id", "fe_month", "provincia_x", "presMax")]
presmax_sample <- presmax_sample %>%
  unite("mesprov", fe_month:provincia_x, sep = "/")

# Registros con y sin NA de las variables month, prov y presMax:
presmax_sample_sinNA = presmax_sample %>% drop_na(presMax)
presmax_sample_NA = presmax_sample %>% filter(is.na(presMax))
presmax_sample_NA$presMax <- NULL

# Media presión máxima:
media_presmax = presmax_sample_sinNA %>%
  group_by(mesprov) %>%
  summarise(presMax = mean(presMax))
media_presmax = as.data.frame(media_presmax)

final_presmax <- merge(presmax_sample_NA, media_presmax, by = "mesprov")
final_presmax$mesprov <- NULL

datos$presMax[match(final_presmax$id, datos$id)] <- final_presmax$presMax
datos$presMax <- round(datos$presMax,digits = 1)


# Antes:

# 1011.9 173 0.2  0.3
# 1008.7 164 0.2  0.3
# 1009.9 154 0.2  0.3
# 1007.7 146 0.2  0.3
# 1014.6 146 0.2  0.3

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 769.1   942.7   983.3   973.1  1009.4  1043.0   27420 

# Después:

# 1019   1096 1.4  1.4
# 970.2  1062 1.3  1.3
# 995.5   919 1.1  1.1
# 968.2   872 1.1  1.1
# 969.3   716 0.9  0.9

#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 769.1   948.2   981.2   974.1  1007.5  1043.0      26 

#Imputamos con la media esos 26 valores que no se han imputado antes
datos$presMax[is.na(datos$presMax)] <- mean(datos$presMax, na.rm = TRUE)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 769.1   948.2   981.2   974.1  1007.5  1043.0




# Presión Mínima: 

# CÁLCULO PRESIÓN MÍNIMA:
presmin_sample = datos[ ,c("id", "fe_month", "provincia_x", "presMin")]
presmin_sample <- presmin_sample %>%
  unite("mesprov", fe_month:provincia_x, sep = "/")

# Registros con y sin NA de las variables month, prov y presMin:
presmin_sample_sinNA = presmin_sample %>% drop_na(presMin)
presmin_sample_NA = presmin_sample %>% filter(is.na(presMin))
presmin_sample_NA$presMin <- NULL

# Media presión mínima:
media_presmin = presmin_sample_sinNA %>%
  group_by(mesprov) %>%
  summarise(presMin = mean(presMin))
media_presmin = as.data.frame(media_presmin)

final_presmin <- merge(presmin_sample_NA, media_presmin, by = "mesprov")
final_presmin$mesprov <- NULL

datos$presMin[match(final_presmin$id, datos$id)] <- final_presmin$presMin
datos$presMin <- round(datos$presMin,digits = 1)


# Antes:

# 1006.3 155 0.2  0.3
# 1004.5 152 0.2  0.3
# 1007   150 0.2  0.3
# 1004.6 149 0.2  0.3
# 1008.1 146 0.2  0.3

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 765.9   939.0   979.0   968.8  1004.9  1039.8   27419 


# Después: 

# 1013.8 1101 1.4  1.4
# 966.7  1047 1.3  1.3
# 990.5   915 1.1  1.1
# 963.5   870 1.1  1.1
# 958     825 1.0  1.0

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 765.9   944.2   977.2   969.8  1003.0  1039.8      26 

#Imputamos con la media esos valors que no se han imputado antes
datos$presMin[is.na(datos$presMin)] <- mean(datos$presMin, na.rm = TRUE)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 765.9   944.2   977.2   969.8  1003.0  1039.8 


#Comprobamos el número de nulos que nos quedan por imputar
colSums(is.na(datos), na.rm = TRUE)

# 1 dir           11441    14.1 
# 2 racha         11441    14.1 
# 3 velmedia       9576    11.8 
# 4 idcomunidad    6111     7.56
# 5 idprovincia    6111     7.56


##Otros cambios

# Velmedia
datos$velmedia[is.na(datos$velmedia)] <- 0
datos$velmedia[datos$velmedia == 0.0] <- 0


# Racha
# Si el viento es cero, asumimos que la racha es 0

for (i in 1:nrow(datos)) {
  
  if ((datos[i, 29] == 0.0) || (datos[i, 29] == 0))  {
    
    datos[i,30] = 0
  
    }
}

#datos[!complete.cases(datos$racha), ]

#creamos rachax2
datos$rachax2 <- datos$racha

  #1.imputamos el resto de valores de forma aleatoria entre su velocidad media y el maximo de las rachas registradas
for (i in 1:nrow(datos)) {
  
  if (is.na(datos[i,30])) {
    datos[i,30] = sample(datos[i,29]:max(datos$racha, na.rm = TRUE),
                        sum(is.na(datos[i,30])), replace = T)
  }
}

  #2.imputamos el resto de valores de forma aleatoria entre su velocidad media*2 y el maximo de las rachas registradas
for (i in 1:nrow(datos)) {
  
  if (is.na(datos[i,36])) {
    datos[i,36] = sample((datos[i,29]*2):max(datos$rachax2, na.rm = TRUE),
                        sum(is.na(datos[i,36])), replace = T)
  }
}

#Comprobamos nuevamente los nulos que nos quedan por imputar
colSums(is.na(datos), na.rm = TRUE)

# 1 dir           11441    14.1 
# 4 idcomunidad    6111     7.56
# 5 idprovincia    6111     7.56


# Direccion
# Asignamos NA a valores que no tienen sentido (tenemos grados de una circunferencia,
# de 0 a 360)
datos$dir[datos$dir == 99] <- NA
datos$dir[datos$dir == 88] <- NA

#datos[!complete.cases(datos$dir), ]

#Otras imputaciones
  #imputamos de forma aletoria el resto de nulos de esta variable:
rand.imput <- function(x){
  missing <- (is.na(x)) #vector booleano
  n.missing <- sum(missing)#Numero de NA’s
  x.obs <- x[!missing]#Datos no NA
  imputed <- x
  imputed[missing] <- sample(x.obs,n.missing,replace = T)
  #Se extrae una muestra aleatoria conocida y se remplazan estos en los NA
  return(imputed)}

datos$dir <- rand.imput(datos$dir)

## ----------------------------------------------------------- ##
## --------------------- DIR CATEGÓRICA ---------------------- ## 
## ----------------------------------------------------------- ##
#Pasamos dir a categorica en función de la orientación de los puntos cardinales
# Cuando el viento es 0, le ponemos dir = 'C' de Calma.

for (i in 1:nrow(datos)) {
  
  if (datos[i,29] == 0) {
    
    datos[i,28] = 'C'
  
  } else if ((datos[i, 28] >= 3) && (datos[i, 28] < 7)) {
    
    datos[i,28] = 'NE'
    
  } else if ((datos[i, 28] >= 7) && (datos[i, 28] < 12)) {
    
    datos[i,28] = 'E'
    
  } else if ((datos[i, 28] >= 12) && (datos[i, 28] < 16)) {
    
    datos[i,28] = 'SE'
    
  } else if ((datos[i, 28] >= 16) && (datos[i, 28] < 21)) {
    
    datos[i,28] = 'S'
    
  } else if ((datos[i, 28] >= 21) && (datos[i, 28] < 25)) {
    
    datos[i,28] = 'SO'
    
  } else if ((datos[i, 28] >= 25) && (datos[i, 28] < 30)) {
    
    datos[i,28] = 'O'
    
  } else if ((datos[i, 28] >= 30) && (datos[i, 28] < 34)) {
    
    datos[i,28] = 'NO'
    
  } else {
    
    datos[i,28] = 'N'
    
  }
}


## ----------------------------------------------------------- ##
## --------------------- NULOS FALTANTES---------------------- ## 
## ----------------------------------------------------------- ##

# Rellenamos las idcomunidad e idprovincia que son nulos y corregimos la id asignada, ya que es errónea
datos$comunidad <- as.character(datos$comunidad)

datos$idcomunidad <- replace(datos$idcomunidad, which(datos$comunidad == 'Andalucía'), 1)
datos$idcomunidad <- replace(datos$idcomunidad, which(datos$comunidad == 'Aragón'), 2)
datos$idcomunidad <- replace(datos$idcomunidad, which(datos$comunidad == 'Asturias'), 3)
datos$idcomunidad <- replace(datos$idcomunidad, which(datos$comunidad == 'Islas Baleares'), 4)
datos$idcomunidad <- replace(datos$idcomunidad, which(datos$comunidad == 'Canarias'), 5)
datos$idcomunidad <- replace(datos$idcomunidad, which(datos$comunidad == 'Cantabria'), 6)
datos$idcomunidad <- replace(datos$idcomunidad, which(datos$comunidad == 'Castilla y León'), 7)
datos$idcomunidad <- replace(datos$idcomunidad, which(datos$comunidad == 'Castilla-La Mancha'), 8)
datos$idcomunidad <- replace(datos$idcomunidad, which(datos$comunidad == 'Cataluña'), 9)
datos$idcomunidad <- replace(datos$idcomunidad, which(datos$comunidad == 'Comunidad Valenciana'), 10)
datos$idcomunidad <- replace(datos$idcomunidad, which(datos$comunidad == 'Extremadura'), 11)
datos$idcomunidad <- replace(datos$idcomunidad, which(datos$comunidad == 'Galicia'), 12)
datos$idcomunidad <- replace(datos$idcomunidad, which(datos$comunidad == 'Madrid'), 13)
datos$idcomunidad <- replace(datos$idcomunidad, which(datos$comunidad == 'Murcia'), 14)
datos$idcomunidad <- replace(datos$idcomunidad, which(datos$comunidad == 'Navarra'), 15)
datos$idcomunidad <- replace(datos$idcomunidad, which(datos$comunidad == 'País Vasco'), 16)
datos$idcomunidad <- replace(datos$idcomunidad, which(datos$comunidad == 'La Rioja'), 17)
datos$idcomunidad <- replace(datos$idcomunidad, which(datos$comunidad == 'Ceuta y Melilla'), 18)
datos$comunidad <- replace(datos$comunidad, which(datos$comunidad == 'Ceuta y Melilla'), 'Ceuta')

# Esto se guarda como csv y se utiliza en descomposicion.ipynb
com_prov = datos %>% 
              distinct(comunidad, provincia_x, idcomunidad, idprovincia, zona) %>% 
              drop_na() %>%
              arrange(comunidad, provincia_x)

melilla <- data.frame(comunidad = "Melilla",
                      provincia_x = "Melilla",
                      zona = "Mediterráneo",
                      idcomunidad = 19,
                      idprovincia = 52)

com_prov <- rbind(com_prov, melilla)

for (i in 1:nrow(datos)) {
  
  if (is.na(datos[i,18])) {
    
    for (j in 1:nrow(com_prov)) {
    
      if ((datos[i,7] == com_prov[j,1]) & (datos[i,8] == com_prov[j,2])) {
      
          datos[i,18] = com_prov[j,5]
      }  
    }
  } 
}


#---Otras transformaciones----#

## ----------------------------------------------------------- ##
## -------------------- precipitación ------------------------ ## 
## ----------------------------------------------------------- ##

# Transformamos esta variable numérica a sea binaria: 0 si no hubo precipitaciones y 1 si la hubo.

# Vemos que hay lluvia en un 20% de las observaciones.
(length(datos$prec) - sum(datos$prec == 0))/length(datos$prec)*100

# Reemplazamos los valores con lluvia por 1
datos$prec <- replace(datos$prec, which(datos$prec > 0),1)

## ----------------------------------------------------------- ##
## --------------------- COD_AEMET ------------------------- ## 
## ----------------------------------------------------------- ##

# Unimos idprovincia e idmunicipio para crear COD_AEMET para identificar los municipios

datos$idmunicipio2 = str_pad(datos$idmunicipio, 3, pad = "0")
datos$idprovincia2 = str_pad(datos$idprovincia, 2, pad = "0")
datos$idcomunidad2 = str_pad(datos$idcomunidad, 2, pad = "0")
datos$COD_AEMET = str_c(datos$idprovincia2, datos$idmunicipio2)

# En el INE hay municipios desaparecidos o integrados en otros que dan problema. Ponemos el código del municipio con el que se integra

datos$COD_AEMET <- replace(datos$COD_AEMET, which(datos$COD_AEMET == '15063'), '15902') # Oza dos Ríos en Oza-Cesuras
datos$COD_AEMET <- replace(datos$COD_AEMET, which(datos$COD_AEMET == '15026'), '15902') # Cesuras en Oza-Cesuras
datos$COD_AEMET <- replace(datos$COD_AEMET, which(datos$COD_AEMET == '31817'), '31032') # Las Bárdenas Reales las adscribimos a Arguedas
datos$COD_AEMET <- replace(datos$COD_AEMET, which(datos$COD_AEMET == '31800'), '31032') # Las Bárdenas Reales las adscribimos a Arguedas
datos$COD_AEMET <- replace(datos$COD_AEMET, which(datos$COD_AEMET == '36011'), '36902') # Cerdedo a Cerdedo-Cotobade
datos$COD_AEMET <- replace(datos$COD_AEMET, which(datos$COD_AEMET == '36012'), '36902') # Cotobade a Cerdedo-Cotobade 
datos$COD_AEMET <- replace(datos$COD_AEMET, which(datos$COD_AEMET == '04039'), '04007') # Darrical a Alcolea
datos$COD_AEMET <- replace(datos$COD_AEMET, which(datos$COD_AEMET == '39801'), '39032') # Mancomunidad de Campoo-Cabuérniga a Hermandad de Campoo de Suso 
datos$COD_AEMET <- replace(datos$COD_AEMET, which(datos$COD_AEMET == '31785'), '31139') # Facería 85 a Lana 

# Los municipios que no se pueden integrar son OTRAS_PROVINCIAS y PORTUGAL

# Creamos la columna NATCODE para integrar con los shapefile que utilizaremos en los mapas
datos$NATCODE <- str_c('34', datos$idcomunidad2, datos$idprovincia2, datos$COD_AEMET)

# Eliminamos las columnas creadas
datos$idmunicipio2 <- NULL
datos$idprovincia2 <- NULL
datos$idcomunidad2 <- NULL


## ----------------------------------------------------------- ##
## ------------------- OTRAS VARIABLES ----------------------- ## 
## ----------------------------------------------------------- ##

#Se necesita leer el csv : municipiosINE.csv
# Unificamos los nombres de los municipios y provincias, añadimos las variables poblacion_muni y superficie_muni, a las cuales reemplazamos los nulos por la media de cada columna.

municipios_ine = read.csv("municipiosINE.csv", header = TRUE, 
                          fileEncoding = "UTF-8", colClasses = c("COD_AEMET" = "character"))

municipios_ine$municipio <- replace(municipios_ine$municipio, 
                                    which(municipios_ine$municipio == 'Erriberabeitia'),
                                    'Ribera Baja/Erribera Beitia')

municipios_ine$municipio <- replace(municipios_ine$municipio, 
                                    which(municipios_ine$municipio == 'Palma '),
                                    'Palma')

municipios_ine$municipio <- replace(municipios_ine$municipio, 
                                    which(municipios_ine$municipio == 'Salines, Ses'),
                                    'Salines, ses')

municipios_ine$municipio <- replace(municipios_ine$municipio, 
                                    which(municipios_ine$municipio == 'Castell, Es'),
                                    'Castell, es')


# 
# # Corregimos la idcomunidad
# municipios_ine <- merge(municipios_ine, com_prov, by = "idprovincia", all.x = TRUE)
# 
# # Eliminamos las columnas innecesarias
# municipios_ine$idcomunidad.x <- NULL
# municipios_ine$comunidad.y <- NULL
# municipios_ine$provincia_x <- NULL
# municipios_ine$zona.y <- NULL
# # Renombramos
# names(municipios_ine)[4] = "comunidad"
# names(municipios_ine)[8] = "zona"
# names(municipios_ine)[14] = "idcomunidad"
# Creamos la columna NATCODE
municipios_ine$idprovincia2 = str_pad(municipios_ine$idprovincia, 2, pad = "0")
municipios_ine$idcomunidad2 = str_pad(municipios_ine$idcomunidad, 2, pad = "0")
municipios_ine$NATCODE <- str_c('34', municipios_ine$idcomunidad2, municipios_ine$idprovincia2,
                                municipios_ine$COD_AEMET)
# Eliminamos las nuevas variables sobrantes
municipios_ine$idcomunidad2 <- NULL
municipios_ine$idprovincia2 <- NULL

#Se cre un df com_prov_num
com_prov_mun = municipios_ine %>% 
  distinct(idcomunidad, comunidad, idprovincia, provincia, COD_AEMET, municipio, zona,
           poblacion_muni, superficie_muni, id_zona_meteo, zona_meteo, NATCODE) %>% 
  drop_na() %>%
  arrange(COD_AEMET)

municipios_ine_sub = subset(municipios_ine, select = c(5:8, 11:14))
datos_ine <- merge(datos, municipios_ine_sub, by = "COD_AEMET", all.x = TRUE)

datos_ine$superficie_muni = as.numeric(gsub(",", ".", datos_ine$superficie_muni))

# Los nulos son debido a incendios en OTRA_PROVINCIA o en PORTUGAL: 
sum(is.na(datos_ine))

print(miss_var_summary(datos_ine), n = 45)
# 644 nulos en 92 filas. Eliminamos estas filas.
datos_ine = datos_ine %>% drop_na()

# #datos_ine <- mutate_at(datos_ine, c("poblacion_muni"), ~replace(., is.na(.),
#                         mean(datos_ine$poblacion_muni, na.rm = TRUE)))
# 
# datos_ine <- mutate_at(datos_ine, c("superficie_muni"), ~replace(., is.na(.), 
#                         mean(datos_ine$superficie_muni, na.rm = TRUE)))
# 
# datos_ine$provincia_x = as.character(datos_ine$provincia_x)
# datos_ine$municipio.x = as.character(datos_ine$municipio.x)
# 
# for (i in 1:nrow(datos_ine)) {
#   
#   if (is.na(datos_ine[i,38])) {
#     
#     datos_ine[i, 38] = datos_ine[i, 9] # provincia
#     datos_ine[i, 39] = datos_ine[i, 11] # municipio
#     
#     } 
#   i = i + 1
# }

## ----------------------------------------------------------- ##
## --------------------- VARIABLE OBJETIVO---------------------- ## 
## ----------------------------------------------------------- ##


# Creamos la variable objetivo superficie_quemada (categórica)

datos_ine$superficie_quemada <- cut(datos_ine$superficie, 
                                    breaks = c(0, 1, 5, 50, 200, 500, 28880),
                                    right = TRUE, 
                                    include.lowest = TRUE, 
                                    labels = c('Conato', 'Incendio Muy Pequeño', 'Incendio Pequeño',
                                              'Incendio Mediano', 'Incendio Grande',
                                              'Gran Incendio'))

#Observación de las precuencias de var obj.
freq(datos_ine$superficie_quemada)

# Eliminamos columnas sin interés y las reordenamos para que queden igual que los datos obtenidos en predicciones
# Dejamos la id porque la necesitaremos para posteriores transformaciones
colnames(datos_ine)
datos_ine = subset(datos_ine, select = c(2, 6:7, 18, 8, 19, 40, 1, 41, 12, 24, 42:45, 38, 5, 28, 27,
                                         31, 26, 30, 29, 36, 3, 46))

names(datos_ine)[6] = "idprovincia"
names(datos_ine)[9] = "municipio"

summary(datos_ine)



## ----------------------------------------------------------- ##
## ------------------------ ATIPICOS ------------------------- ## 
## ----------------------------------------------------------- ##

at <- data.frame(sort(
          round(sapply(Filter(
          is.numeric, datos_ine),function(nOut) atipicosAmissing(
                nOut)[[2]])/nrow(datos_ine)*100,3), decreasing = T))

names(at) <- "% Outliers por variable"

at

# % Outliers por variable
# prec                             19.954
# superficie                        9.332
# poblacion_muni                    8.465
# superficie_muni                   2.399
# latitude                          0.303
# longitude                         0.163
# velmedia                          0.103
# racha                             0.010


#Guardamos los datos limpios en incendios_fin_base4
write.csv(datos_ine,"incendios_fin_base4.csv", row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8")
write.csv(com_prov,"com_prov.csv", row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8")
write.csv(municipios_ine,"municipiosINE_fin.csv", row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8")
