##load libraries
library(rgdal)
library(raster)

#read in first polygon shapefile
peninsula <- readOGR(dsn = "C:/Users/saulo/Documents/R/data/mapas_shp/SIGLIM_Publico_INSPIRE/SHP_ETRS89/recintos_municipales_inspire_peninbal_etrs89",layer = "recintos_municipales_inspire_peninbal_etrs89",
                     use_iconv = TRUE, encoding = "UTF-8", stringsAsFactors = FALSE)

class(peninsula)

#read in second polygon shapefile
canarias <- readOGR(dsn = "C:/Users/saulo/Documents/R/data/mapas_shp/SIGLIM_Publico_INSPIRE/SHP_WGS84/recintos_municipales_inspire_canarias_wgs84",layer = "recintos_municipales_inspire_canarias_wgs84",
                    use_iconv = TRUE, encoding = "UTF-8", stringsAsFactors = FALSE)

summary(peninsula)
summary(canarias)

# Unimos los shapefile de la península y de Canarias
espania <- union(peninsula, canarias)

class(espania)
length(espania)
extent(espania)
crs(espania)

head(espania@data)
length(espania@data)
names(espania@data)

espania$NATCODE

# Importamos el fichero municipios INE
municipios_ine = read.csv("municipiosINE_fin.csv", header = TRUE, stringsAsFactors = FALSE,
                          fileEncoding = "UTF-8", colClasses = c("COD_AEMET" = "character"))

municipios_ine$superficie_muni = as.numeric(gsub(",", ".", municipios_ine$superficie_muni))

# Unimos el fichero de los polígonos de los municipios de España con municipiosINE
espania <- merge(espania, municipios_ine, 
                     by.x = c("NATCODE"), by.y = c("NATCODE"), all.x = FALSE)

# Vemos que se han eliminado todos los polígonos pertenecientes a entidades a las que AEMET no da predicción
View(espania@data)
colnames(espania@data)

# Ordenamos las columnas para que queden como antes
espania = subset(espania, select = c(2:5, 1, 6:23))

# Guardamos en nuevo fichero shapefile

writeOGR(espania, ".", "espania", 
         driver = "ESRI Shapefile")
