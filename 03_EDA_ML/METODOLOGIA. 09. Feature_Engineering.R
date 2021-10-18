


setwd("C:/Users/cregueir/Downloads")
datos = read.csv("incendios_fin_base4.csv")
datoseda = read.csv("datos_eda.csv")



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

colSums(is.na(datos), na.rm = TRUE)
str(datos)

datos$dir[datos$dir == 0] <- 'N'


datos <- tibble::rowid_to_column(datos, "ID")

write.csv(datos,"incendios_fin_base4_ID.csv", row.names = FALSE)


fe <- as.data.frame(datos$ID)
colnames(fe) <- 'ID'





# 1. Distancia:

fe$fe_dist <- sqrt ( (datos$longitude)^2 + (datos$latitude)^2 )



# 2. Trimestre/Estación:

for (i in 1:length(datos$fe_month)){
  if(datos$fe_month[i] <= 3){
    fe$fe_trim[i] = 1}
  if((datos$fe_month[i]>3)&(datos$fe_month[i]<= 6)){
    fe$fe_trim[i] = 2}
  if((datos$fe_month[i]>6)&(datos$fe_month[i]<= 9)){
    fe$fe_trim[i] = 3}
  if(datos$fe_month[i]> 9){
    fe$fe_trim[i] = 4}
}



# 3. Verano: binaria (SI o NO)
fe$fe_summer <- if_else((datos$fe_month>= 7)&(datos$fe_month<= 9), 1,0) 





# 5. Densidad de población:

fe$fe_densPob <- datos$poblacion_muni/datos$superficie_muni



# 6. Clasificación viento: 

# Velocidad del viento: Velocidad media del viento en los 10 minutos anteriores a la hora indicada, en km/h. 
# Se considera que el viento está en calma cuando  la velocidad media, en diez minutos, es inferior a 1,8 km/h.
# summary(datos$velmedia)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.100   2.200   2.519   3.300  20.000 

# Velocidad racha: Velocidad máxima del viento, en los 60 minutos anteriores a la hora indicada, en km/h.
# summary(datos$racha)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   6.100   8.900   9.117  11.700  42.200 

# Número de Beaufort	Velocidad del viento (km/h)  Denominación	                    Aspecto del mar y Efectos en tierra
# 0                 	0 a 1		                      Calma	                          Despejado	Calma, el humo asciende verticalmente
# 1	                  2 a 5		                      Ventolina	                      Pequeñas olas, pero sin espuma	El humo indica la dirección del viento
# 2	                  6 a 11	                      Flojito (Brisa muy débil)	      Crestas de apariencia vítrea, sin romper	Se caen las hojas de los árboles, empiezan a moverse los molinos de los campos
# 3	                  12 a 19	                      Flojo (Brisa Ligera)	          Pequeñas olas, crestas rompientes.	Se agitan las hojas, ondulan las banderas
# 4	                  20 a 28                     	Bonancible (Brisa moderada)	    Borreguillos numerosos, olas cada vez más largas	Se levanta polvo y papeles, se agitan las copas de los árboles
# 5	                  29 a 38                     	Fresquito (Brisa fresca)      	Olas medianas y alargadas, borreguillos muy abundantes	Pequeños movimientos de los árboles, superficie de los lagos ondulada
# 6	                  39 a 49                     	Fresco (Brisa fuerte)	          Comienzan a formarse olas grandes, crestas rompientes, espuma	Se mueven las ramas de los árboles, dificultad para mantener abierto el paraguas
# 7	                  50 a 61	                      Frescachón (Viento fuerte)	    Mar gruesa, con espuma arrastrada en dirección del viento	Se mueven los árboles grandes, dificultad para caminar contra el viento
# 8	                  62 a 74                       Temporal (Viento duro)	        Grandes olas rompientes, franjas de espuma	Se quiebran las copas de los árboles, circulación de personas muy difícil, los vehículos se mueven por sí mismos.
# 9	                  75 a 88	                      Temporal fuerte (Muy duro)	    Olas muy grandes, rompientes. Visibilidad mermada	Daños en árboles, imposible caminar con normalidad. Se empiezan a dañar las construcciones. Arrastre de vehículos.
# 10	                89 a 102                      Temporal duro (Temporal)	      Olas muy gruesas con crestas empenachadas. Superficie del mar blanca.	Árboles arrancados, daños en la estructura de las construcciones. Daños mayores en objetos a la intemperie.
# 11	                103 a 117                     Temporal muy duro (Borrasca)   	Olas excepcionalmente grandes, mar completamente blanca, visibilidad muy reducida	Destrucción en todas partes, lluvias muy intensas, inundaciones muy altas. Voladura de personas y de otros muchos objetos.
# 12	                + 118	                        Temporal huracanado (Huracán)	  Olas excepcionalmente grandes, mar blanca, visibilidad nula	Voladura de vehículos, árboles, casas, techos y personas. Puede generar un huracán o tifón



# Número de Beaufort con velmedia:
for (i in 1:length(datos$velmedia)){
  if(datos$velmedia[i] < 2){
    fe$fe_beauMed[i] = 0}
  if((datos$velmedia[i]>= 2)&(datos$velmedia[i]< 6)){
    fe$fe_beauMed[i] = 1}
  if((datos$velmedia[i]>= 6)&(datos$velmedia[i]< 12)){
    fe$fe_beauMed[i] = 2}
  if((datos$velmedia[i]>= 12)&(datos$velmedia[i]< 20)){
    fe$fe_beauMed[i] = 3}
  if((datos$velmedia[i]>= 20)&(datos$velmedia[i]< 29)){
    fe$fe_beauMed[i] = 4}
  if((datos$velmedia[i]>= 29)&(datos$velmedia[i]< 39)){
    fe$fe_beauMed[i] = 5}
  if((datos$velmedia[i]>= 39)&(datos$velmedia[i]< 50)){
    fe$fe_beauMed[i] = 6}
}

# Número de Beaufort con racha:
for (i in 1:length(datos$racha)){
  if(datos$racha[i] < 2){
    fe$fe_beauRacha[i] = 0}
  if((datos$racha[i]>= 2)&(datos$racha[i]< 6)){
    fe$fe_beauRacha[i] = 1}
  if((datos$racha[i]>= 6)&(datos$racha[i]< 12)){
    fe$fe_beauRacha[i] = 2}
  if((datos$racha[i]>= 12)&(datos$racha[i]< 20)){
    fe$fe_beauRacha[i] = 3}
  if((datos$racha[i]>= 20)&(datos$racha[i]< 29)){
    fe$fe_beauRacha[i] = 4}
  if((datos$racha[i]>= 29)&(datos$racha[i]< 39)){
    fe$fe_beauRacha[i] = 5}
  if((datos$racha[i]>= 39)&(datos$racha[i]< 50)){
    fe$fe_beauRacha[i] = 6}
}

# Estado del viento con velmedia:
for (i in 1:length(fe$fe_beauMed)){
  if(fe$fe_beauMed[i]==0){
    fe$fe_estadoVientoMed[i] = 'Calma'}
  if(fe$fe_beauMed[i]==1){
    fe$fe_estadoVientoMed[i] = 'Ventolina'}
  if(fe$fe_beauMed[i]==2){
    fe$fe_estadoVientoMed[i] = 'Brisa Débil'}
  if(fe$fe_beauMed[i]==3){
    fe$fe_estadoVientoMed[i] = 'Brisa Ligera'}
  if(fe$fe_beauMed[i]==4){
    fe$fe_estadoVientoMed[i] = 'Brisa Moderada'}
  if(fe$fe_beauMed[i]==5){
    fe$fe_estadoVientoMed[i] = 'Brisa Fresca'}
  if(fe$fe_beauMed[i]==6){
    fe$fe_estadoVientoMed[i] = 'Brisa Fuerte'}
} 
fe$fe_estadoVientoMed <- as.factor(fe$fe_estadoVientoMed)

# Estado del viento con racha:
for (i in 1:length(fe$fe_beauRacha)){
  if(fe$fe_beauRacha[i]==0){
    fe$fe_estadoVientoRacha[i] = 'Calma'}
  if(fe$fe_beauRacha[i]==1){
    fe$fe_estadoVientoRacha[i] = 'Ventolina'}
  if(fe$fe_beauRacha[i]==2){
    fe$fe_estadoVientoRacha[i] = 'Brisa Débil'}
  if(fe$fe_beauRacha[i]==3){
    fe$fe_estadoVientoRacha[i] = 'Brisa Ligera'}
  if(fe$fe_beauRacha[i]==4){
    fe$fe_estadoVientoRacha[i] = 'Brisa Moderada'}
  if(fe$fe_beauRacha[i]==5){
    fe$fe_estadoVientoRacha[i] = 'Brisa Fresca'}
  if(fe$fe_beauRacha[i]==6){
    fe$fe_estadoVientoRacha[i] = 'Brisa Fuerte'}
} 
fe$fe_estadoVientoRacha <- as.factor(fe$fe_estadoVientoRacha)



# 7. Temperatura media: 
fe$fe_tmed = (datos$tmax+datos$tmin)/2


# 4. Sensacion termica por frio(temperatura max 'C' y viento max 'km/h')--> variables max
fe$fe_STfrio= 13.1267 + (0.6215*(datos$tmax))- (11.37*(datos$racha))^0.16 +  (0.3965*(datos$tmax)*(datos$racha))^0.16
# sensacion termica media



# 8. idmunicipio:
municipio <- as.factor(datos$municipio)
levels <- as.vector(levels(municipio))
fe$fe_idmunicipio <- match(datos$municipio, levels)



# 9. tipo temperatura
for (i in 1:length(datos$tmax)){
  if(datos$tmax[i] <= -10){
    fe$fe_tipotempMax[i] = 'Frío Extremo'}
  if((datos$tmax[i]>-10)&(datos$tmax[i]<= 0)){
    fe$fe_tipotempMax[i] = 'Mucho Frío'}
  if((datos$tmax[i]>0)&(datos$tmax[i]<= 10)){
    fe$fe_tipotempMax[i] = 'Frío'}
  if((datos$tmax[i]>10)&(datos$tmax[i]<= 20)){
    fe$fe_tipotempMax[i] = 'Templado'}
  if((datos$tmax[i]>20)&(datos$tmax[i]<= 30)){
    fe$fe_tipotempMax[i] = 'Calor'}
  if((datos$tmax[i]>30)&(datos$tmax[i]<= 40)){
    fe$fe_tipotempMax[i] = 'Mucho Calor'}
  if(datos$tmax[i]> 40){
    fe$fe_tipotempMax[i] = 'Calor Extremo'}
}

for (i in 1:length(datos$tmin)){
  if(datos$tmin[i] <= -10){
    fe$fe_tipotempMin[i] = 'Frío Extremo'}
  if((datos$tmin[i]>-10)&(datos$tmin[i]<= 0)){
    fe$fe_tipotempMin[i] = 'Mucho Frío'}
  if((datos$tmin[i]>0)&(datos$tmin[i]<= 10)){
    fe$fe_tipotempMin[i] = 'Frío'}
  if((datos$tmin[i]>10)&(datos$tmin[i]<= 20)){
    fe$fe_tipotempMin[i] = 'Templado'}
  if((datos$tmin[i]>20)&(datos$tmin[i]<= 30)){
    fe$fe_tipotempMin[i] = 'Calor'}
  if((datos$tmin[i]>30)&(datos$tmin[i]<= 40)){
    fe$fe_tipotempMin[i] = 'Mucho Calor'}
  if(datos$tmin[i]> 40){
    fe$fe_tipotempMin[i] = 'Calor Extremo'}
}

for (i in 1:length(datos$tmax)){
  if(datos$tmax[i] <= -10){
    fe$fe_tipotempMaxNUM[i] = 0}
  if((datos$tmax[i]>-10)&(datos$tmax[i]<= 0)){
    fe$fe_tipotempMaxNUM[i] = 1}
  if((datos$tmax[i]>0)&(datos$tmax[i]<= 10)){
    fe$fe_tipotempMaxNUM[i] = 2}
  if((datos$tmax[i]>10)&(datos$tmax[i]<= 20)){
    fe$fe_tipotempMaxNUM[i] = 3}
  if((datos$tmax[i]>20)&(datos$tmax[i]<= 30)){
    fe$fe_tipotempMaxNUM[i] = 4}
  if((datos$tmax[i]>30)&(datos$tmax[i]<= 40)){
    fe$fe_tipotempMaxNUM[i] = 5}
  if(datos$tmax[i]> 40){
    fe$fe_tipotempMaxNUM[i] = 6}
}

for (i in 1:length(datos$tmin)){
  if(datos$tmin[i] <= -10){
    fe$fe_tipotempMinNUM[i] = 0}
  if((datos$tmin[i]>-10)&(datos$tmin[i]<= 0)){
    fe$fe_tipotempMinNUM[i] = 1}
  if((datos$tmin[i]>0)&(datos$tmin[i]<= 10)){
    fe$fe_tipotempMinNUM[i] = 2}
  if((datos$tmin[i]>10)&(datos$tmin[i]<= 20)){
    fe$fe_tipotempMinNUM[i] = 3}
  if((datos$tmin[i]>20)&(datos$tmin[i]<= 30)){
    fe$fe_tipotempMinNUM[i] = 4}
  if((datos$tmin[i]>30)&(datos$tmin[i]<= 40)){
    fe$fe_tipotempMinNUM[i] = 5}
  if(datos$tmin[i]> 40){
    fe$fe_tipotempMinNUM[i] = 6}
}

for (i in 1:length(fe$fe_tmed)){
  if(fe$fe_tmed[i] <= -10){
    fe$fe_tipotempMed[i] = 'Frío Extremo'}
  if((fe$fe_tmed[i]>-10)&(fe$fe_tmed[i]<= 0)){
    fe$fe_tipotempMed[i] = 'Mucho Frío'}
  if((fe$fe_tmed[i]>0)&(fe$fe_tmed[i]<= 10)){
    fe$fe_tipotempMed[i] = 'Frío'}
  if((fe$fe_tmed[i]>10)&(fe$fe_tmed[i]<= 20)){
    fe$fe_tipotempMed[i] = 'Templado'}
  if((fe$fe_tmed[i]>20)&(fe$fe_tmed[i]<= 30)){
    fe$fe_tipotempMed[i] = 'Calor'}
  if((fe$fe_tmed[i]>30)&(fe$fe_tmed[i]<= 40)){
    fe$fe_tipotempMed[i] = 'Mucho Calor'}
  if(fe$fe_tmed[i]> 40){
    fe$fe_tipotempMed[i] = 'Calor Extremo'}
}

for (i in 1:length(fe$fe_tmed)){
  if(fe$fe_tmed[i] <= -10){
    fe$fe_tipotempMedNUM[i] = 0}
  if((fe$fe_tmed[i]>-10)&(fe$fe_tmed[i]<= 0)){
    fe$fe_tipotempMedNUM[i] = 1}
  if((fe$fe_tmed[i]>0)&(fe$fe_tmed[i]<= 10)){
    fe$fe_tipotempMedNUM[i] = 2}
  if((fe$fe_tmed[i]>10)&(fe$fe_tmed[i]<= 20)){
    fe$fe_tipotempMedNUM[i] = 3}
  if((fe$fe_tmed[i]>20)&(fe$fe_tmed[i]<= 30)){
    fe$fe_tipotempMedNUM[i] = 4}
  if((fe$fe_tmed[i]>30)&(fe$fe_tmed[i]<= 40)){
    fe$fe_tipotempMedNUM[i] = 5}
  if(fe$fe_tmed[i]> 40){
    fe$fe_tipotempMedNUM[i] = 6}
}



# 9. Condiciones optimas para incendio, otras ideas:

fe$fe_condopt <- ifelse (fe$fe_beauRacha >= 4 & fe$fe_tipotempMedNUM >= 4 & datos$prec == 0, 1, 0)
fe$fe_condopt2 <- ifelse (fe$fe_beauRacha >= 2 & fe$fe_tipotempMedNUM >= 4 & datos$prec == 0, 1, 0)
fe$fe_condopt3 <- ifelse (fe$fe_tipotempMedNUM >= 4 & datos$prec == 0, 1, 0)
fe$fe_condopt4 <- ifelse (fe$fe_summer == 1 & datos$prec == 0, 1, 0)




# 10: idzona:
zona <- as.factor(datos$zona)
levels <- as.vector(levels(zona))
fe$fe_zona <- match(datos$zona, levels)




# Guardamos los datos

incendios_fin_base4_FeTrans <- merge(x=datos, y=fe, by.y=c('ID'))

write.csv(incendios_fin_base4_FeTrans,"incendios_fin_base4_FeTrans.csv", row.names = FALSE)
write.csv(fe,"FE_Transformaciones.csv", row.names = FALSE)




