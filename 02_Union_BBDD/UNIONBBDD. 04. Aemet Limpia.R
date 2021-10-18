library(dplyr)
library(openxlsx)

aemet_ori <- read.csv("C:/Users/saulo/Documents/R/data/datosAemetNoIndex.csv",
                             sep = ",", header = TRUE, encoding = "UTF-8")

lista_bases <- read.csv("C:/Users/saulo/Documents/R/data/estacionesNoIndex.csv",
                      sep = ",", header = TRUE, encoding = "UTF-8")

estaciones_ori <- aemet_ori %>%
                    select(everything()) %>%
                    group_by(indicativo) %>%
                    count(indicativo)


aemet_fusion <- merge(lista_bases, aemet_ori,
                      by.x = c('indicativo'),
                      by.y = c('indicativo'))

aemet_fusion <- subset(aemet_fusion, select = c(1:8, 12:26))
names(aemet_fusion)[3] = 'provincia'
names(aemet_fusion)[4] = 'altitud'
names(aemet_fusion)[5] = 'nombre'
aemet_fusion <- mutate_all(aemet_fusion, ~replace(., is.na(.), ""))

cuenta_medidas <- aemet_fusion %>%
                    group_by(indicativo) %>%
                    count(indicativo)

datosjunio21 <- read.csv("C:/Users/saulo/Documents/R/data/datosjunio21.csv",
                      sep = ",", header = TRUE, encoding = "UTF-8")

est_jun21 = unique(datosjunio21$indicativo)
est_jun21 = as.data.frame(est_jun21)
names(est_jun21)[1] = 'indicativo'
est_jun21 = est_jun21[order(est_jun21$indicativo),]
est_jun21 = as.data.frame(est_jun21)
names(est_jun21)[1] = 'indicativo'

cuenta_junio <- datosjunio21 %>%
  group_by(indicativo) %>%
  count(indicativo)

setdiff(cuenta_junio$indicativo, cuenta_medidas$indicativo)
setdiff(cuenta_medidas$indicativo, cuenta_junio$indicativo)

estaciones_fin <- read.xlsx("C:/Users/saulo/Documents/R/data/estaciones21.xlsx",
                    sheet = 1)

estacionesLatLong21 <- subset(estaciones_fin, select = c(1:7))

# Guardamos el .csv
write.csv(estacionesLatLong21,"estacionesLatLong21.csv", row.names = FALSE, col.names = TRUE)
