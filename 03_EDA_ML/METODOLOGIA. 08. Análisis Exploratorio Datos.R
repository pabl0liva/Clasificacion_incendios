



setwd("C:/Users/cregueir/Downloads/TFM")
datos = read.csv("datoseda.csv")
source("Funciones_R.R")
zonameteo = read.csv("incendios_fin_base4.csv")
setwd("C:/Users/cregueir/Downloads/TFM/Plots")




library(visdat)  # EDA (Preliminary Visualisation of Data)
library(naniar)  # (Data Structures, Summaries, and Visualisations for Missing Data)
library(inspectdf)    # EDA (Exploratory data analysis)
library(DataExplorer) # EDA (Exploratory data analysis)
library(plotly)
library(htmlwidgets)
library(webshot)
library(forcats)
library(dplyr)


########################## DATOS ANTES ##########################################

introduce(datoseda)

#    rows     columns discrete_columns continuous_columns all_missing_columns
#    80861      21                5                 16                   0
# total_missing_values complete_rows total_observations memory_usage
#                 72459         61525            1698081     12318328


# Inspección gráfica inicial
listaGraf <- dfplot_box(datoseda) #Boxplots
listaHist<-dfplot_his(datoseda) #Histogramas

pdf(file = "dfboxplot.pdf",   width = 8,height = 8)
gridExtra::marrangeGrob(listaGraf, nrow = 2, ncol = 2)
dev.off()


pdf(file = "dfhist.pdf",   width = 8,height = 8)
gridExtra::marrangeGrob(listaHist, nrow = 2, ncol = 2)
dev.off()

# correlaciones entre los missings
jpeg(file="corrmissings.jpeg")
corrplot(cor(is.na(datoseda[colnames(
  datoseda)[colSums(is.na(datoseda))>0]])),method = "ellipse",type = "upper")
dev.off()


# Informe en HTML sobre los aspectos más importantes de los datos:
create_report(datoseda)

# Aspectos más relevantes de los datos:
jpeg(file="intro.jpeg")
plot_intro(datoseda)
dev.off()

# Valores nulos por columnas:
jpeg(file="nas.jpeg")
plot_missing(datoseda)
dev.off()


# Summary and comparison of the levels in categorical columns
jpeg(file="categoricas.jpeg")
show_plot(inspect_cat(datoseda))
dev.off()
# Tidy correlation coefficients for numeric dataframe columns
jpeg(file="correlacion.jpeg")
show_plot(inspect_cor(datoseda))
dev.off()
# Summary and comparison of the most common levels in categorical columns
jpeg(file="levelscat.jpeg")
show_plot(inspect_imb(datoseda))
dev.off()
# Summary and comparison of the rate of missingness across dataframe columns
jpeg(file="na.jpeg")
show_plot(inspect_na(datoseda))
dev.off()
# Summary and comparison of numeric columns
jpeg(file="numericas.jpeg")
show_plot(inspect_num(datoseda))
dev.off()
# Summary and comparison of column types
jpeg(file="tipos.jpeg")
show_plot(inspect_types(datoseda))
dev.off()


# Obtenemos tablas con los valores de arriba:
cat<-inspect_cat(datoseda)
cor<-inspect_cor(datoseda)
levls<-inspect_imb(datoseda)
na<-inspect_na(datoseda)
num<-inspect_num(datoseda)
type<-inspect_types(datoseda)


# dividimos en numéricas y categóricas:
datosedanum <- datoseda %>% select(where(is.numeric))
datosedacat <- datoseda %>% select(where(is.character))

# correlaciones en CSV
corr<-gather_cor(datosedanum)
write.csv(corr,"correlaciones.csv", row.names = FALSE)

# correlaciones:
jpeg(file="correlaciones.jpeg")
vis_cor(datosedanum)
dev.off()

# dataframe en columnas con sus missings representados_
jpeg(file="columnasymissings.jpeg")
vis_dat(datoseda, warn_large_data = FALSE)
dev.off()

# Missings:
jpeg(file="missings.jpeg")
vis_miss(datoseda, warn_large_data = FALSE)
dev.off()


# missings en CSV:
missing<-as.data.frame(miss_var_summary(datoseda))
write.csv(missing,"missings.csv", row.names = FALSE)






# Barplot variable zona:
jpeg(file="zonabarplot.jpeg")
datoseda %>%
  group_by(zona) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(zona,(-count)), y = count, fill=zona))+
  geom_bar(stat = 'identity') +
  labs(title = 'Zonas en las que se han producido incendios', 
       subtitle = 'Distribución en conjunto') +
  xlab('Zona') +
  ylab('Cantidad de incendios') +
  theme(legend.position = "none")
dev.off()



# Barplot variable comunidad:
jpeg(file="comunidadbarplot.jpeg")
datoseda %>%
  group_by(comunidad) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(comunidad,(-count)), y = count, fill=comunidad))+
  geom_bar(stat = 'identity') +
  labs(title = 'Comunidades en las que se han producido incendios', 
       subtitle = 'Distribución en conjunto') +
  xlab('Comunidad') +
  ylab('Cantidad de incendios') +
  theme(legend.position = "none")+
  guides(x =  guide_axis(angle = 45))
dev.off()


# Barplot variable provincia:
jpeg(file="provinciabarplot.jpeg")
datoseda %>%
  group_by(provincia) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(provincia,(-count)), y = count, fill=provincia))+
  geom_bar(stat = 'identity') +
  labs(title = 'Provincias en las que se han producido incendios', 
       subtitle = 'Distribución en conjunto') +
  xlab('Provincia') +
  ylab('Cantidad de incendios') +
  theme(legend.position = "none") +
  guides(x =  guide_axis(angle = 45))
dev.off()








# Boxplot variable zona:
jpeg(file="zonaboxplot.jpeg")
datoseda %>%
  ggplot(aes(x = reorder(zona,(-superficie)), y = superficie, fill=zona))+
  geom_boxplot() + 
  stat_summary(fun = mean, shape=20, 
               color='red') +
  stat_summary(aes(label=round(..y..)), 
               fun=mean, 
               geom="text", 
               size=3, 
               vjust = +1.5)+
  labs(title = 'Zonas en las que se han producido incendios', 
       subtitle = 'Distribución en conjunto') +
  xlab('Zona') +
  ylab('Cantidad de incendios') +
  theme(legend.position = "none") 
dev.off()


# Boxplot variable comunidad:
jpeg(file="comunidadboxplot.jpeg")
datoseda %>%
  ggplot(aes(x = reorder(comunidad,(-superficie)), y = superficie, fill=comunidad))+
  geom_boxplot() + 
  stat_summary(fun = mean, shape=20, 
               color='red') +
  stat_summary(aes(label=round(..y..)), 
               fun=mean, 
               geom="text", 
               size=3, 
               vjust = +1.5)+
  labs(title = 'Comunidades en las que se han producido incendios', 
       subtitle = 'Distribución en conjunto') +
  xlab('Zona') +
  ylab('Cantidad de incendios') +
  theme(legend.position = "none") +
  guides(x =  guide_axis(angle = 45))
dev.off()

# Boxplot variable provincia:
jpeg(file="provinciaboxplot.jpeg")
datoseda %>%
  ggplot(aes(x = reorder(provincia,(-superficie)), y = superficie, fill=provincia))+
  geom_boxplot() + 
  stat_summary(fun = mean, shape=20, 
               color='red') +
  stat_summary(aes(label=round(..y..)), 
               fun=mean, 
               geom="text", 
               size=3, 
               vjust = +1.5)+
  labs(title = 'Provincias en las que se han producido incendios', 
       subtitle = 'Distribución en conjunto') +
  xlab('Provincia') +
  ylab('Cantidad de incendios') +
  theme(legend.position = "none") +
  guides(x =  guide_axis(angle = 45))
dev.off()







########################## DATOS DESPÚES ##########################################

setwd("C:/Users/cregueir/Downloads/TFM")
datos = read.csv("datosedafin.csv")
source("Funciones_R.R")
zonameteo = read.csv("incendios_fin_base4.csv")

zonameteo <- zonameteo[ ,c("id","id_zona_meteo","zona_meteo")]
datoseda <- merge(datos, zonameteo, by.y = "id", all = TRUE)
vis_miss(datoseda, warn_large_data = FALSE)


setwd("C:/Users/cregueir/Downloads/TFM/Plots/Plotsfin")

datoseda$fe_month<-as.character(datoseda$fe_month)

introduce(datoseda)

# rows columns discrete_columns continuous_columns all_missing_columns
# 1 80861      23                7                 16                   0
# total_missing_values complete_rows total_observations memory_usage
# 1                    0         80861            1859803     13313688


str(datoseda)
summary(datoseda)

# Inspección gráfica inicial
listaGraf <- dfplot_box(datoseda) #Boxplots
listaHist<-dfplot_his(datoseda) #Histogramas


pdf(file = "dfboxplot.pdf",   width = 8,height = 8)
gridExtra::marrangeGrob(listaGraf, nrow = 2, ncol = 2)
dev.off()


pdf(file = "dfhist.pdf",   width = 8,height = 8)
gridExtra::marrangeGrob(listaHist, nrow = 2, ncol = 2)
dev.off()



create_report(datoseda)
jpeg(file="intro.jpeg")
plot_intro(datoseda)
dev.off()
jpeg(file="nas.jpeg")
plot_missing(datoseda)
dev.off()



jpeg(file="categoricas.jpeg")
show_plot(inspect_cat(datoseda))
dev.off()
jpeg(file="correlacion.jpeg")
show_plot(inspect_cor(datoseda))
dev.off()
jpeg(file="levelscat.jpeg")
show_plot(inspect_imb(datoseda))
dev.off()
jpeg(file="na.jpeg")
show_plot(inspect_na(datoseda))
dev.off()
jpeg(file="numericas.jpeg")
show_plot(inspect_num(datoseda))
dev.off()
jpeg(file="tipos.jpeg")
show_plot(inspect_types(datoseda))
dev.off()

# Obtenemos tablas con los valores de arriba, pero no soy capaz de guardarlas:
cat<-inspect_cat(datoseda)
cor<-inspect_cor(datoseda)
levls<-inspect_imb(datoseda)
na<-inspect_na(datoseda)
num<-inspect_num(datoseda)
type<-inspect_types(datoseda)



datosedanum <- datoseda %>% select(where(is.numeric))
datosedacat <- datoseda %>% select(where(is.character))


corr<-gather_cor(datosedanum)
write.csv(corr,"correlaciones.csv", row.names = FALSE)

jpeg(file="correlaciones.jpeg")
vis_cor(datosedanum)
dev.off()


jpeg(file="columnasymissings.jpeg")
vis_dat(datoseda, warn_large_data = FALSE)
dev.off()

jpeg(file="missings.jpeg")
vis_miss(datoseda, warn_large_data = FALSE)
dev.off()



missing<-as.data.frame(miss_var_summary(datoseda))
write.csv(missing,"missings.csv", row.names = FALSE)







jpeg(file="zonabarplot.jpeg")
datoseda %>%
  group_by(zona) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(zona,(-count)), y = count, fill=zona))+
  geom_bar(stat = 'identity') +
  labs(title = 'Zonas en las que se han producido incendios', 
       subtitle = 'Distribución en conjunto') +
  xlab('Zona') +
  ylab('Cantidad de incendios') +
  theme(legend.position = "none")
dev.off()



jpeg(file="comunidadbarplot.jpeg")
datoseda %>%
  group_by(comunidad) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(comunidad,(-count)), y = count, fill=comunidad))+
  geom_bar(stat = 'identity') +
  labs(title = 'Comunidades en las que se han producido incendios', 
       subtitle = 'Distribución en conjunto') +
  xlab('Comunidad') +
  ylab('Cantidad de incendios') +
  theme(legend.position = "none")+
  guides(x =  guide_axis(angle = 45))
dev.off()


jpeg(file="provinciabarplot.jpeg")
datoseda %>%
  group_by(provincia) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(provincia,(-count)), y = count, fill=provincia))+
  geom_bar(stat = 'identity') +
  labs(title = 'Provincias en las que se han producido incendios', 
       subtitle = 'Distribución en conjunto') +
  xlab('Provincia') +
  ylab('Cantidad de incendios') +
  theme(legend.position = "none") +
  guides(x =  guide_axis(angle = 45))
dev.off()


jpeg(file="dirbarplot.jpeg")
datoseda %>%
  group_by(dir) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(dir,(-count)), y = count, fill=dir))+
  geom_bar(stat = 'identity') +
  labs(title = 'Dirección del viento en los incendios producidos', 
       subtitle = 'Distribución en conjunto') +
  xlab('Dirección del viento') +
  ylab('Cantidad de incendios') +
  theme(legend.position = "none") 
dev.off()


jpeg(file="superficiequemadabarplot.jpeg")
datoseda %>%
  group_by(superficie_quemada) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(superficie_quemada,(-count)), y = count, fill=superficie_quemada))+
  geom_bar(stat = 'identity') +
  labs(title = 'Superficie quemada en los incendios producidos', 
       subtitle = 'Distribución en conjunto') +
  xlab('Superficie Quemada') +
  ylab('Cantidad de incendios') +
  theme(legend.position = "none") +
  guides(x =  guide_axis(angle = 45))
dev.off()



jpeg(file="monthbarplot.jpeg")
datoseda %>%
  group_by(fe_month) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(fe_month,(-count)), y = count, fill=fe_month))+
  geom_bar(stat = 'identity') +
  labs(title = 'Meses en los que más incendios se han producido', 
       subtitle = 'Distribución en conjunto') +
  xlab('Mes') +
  ylab('Cantidad de incendios') +
  theme(legend.position = "none") 
dev.off()






jpeg(file="zonaboxplot.jpeg")
datoseda %>%
  ggplot(aes(x = reorder(zona,(-superficie)), y = superficie, fill=zona))+
  geom_boxplot() + 
  stat_summary(fun = mean, shape=20, 
               color='red') +
  stat_summary(aes(label=round(..y..)), 
               fun=mean, 
               geom="text", 
               size=3, 
               vjust = +1.5)+
  labs(title = 'Superficie quemada según la zona', 
       subtitle = 'Distribución en conjunto') +
  xlab('Zona') +
  ylab('Superficie Quemada') +
  theme(legend.position = "none") 
dev.off()



jpeg(file="comunidadboxplot.jpeg")
datoseda %>%
  ggplot(aes(x = reorder(comunidad,(-superficie)), y = superficie, fill=comunidad))+
  geom_boxplot() + 
  stat_summary(fun = mean, shape=20, 
               color='red') +
  stat_summary(aes(label=round(..y..)), 
               fun=mean, 
               geom="text", 
               size=3, 
               vjust = +1.5)+
  labs(title = 'Superficie quemada según las comunidades', 
       subtitle = 'Distribución en conjunto') +
  xlab('Zona') +
  ylab('Superficie Quemada') +
  theme(legend.position = "none") +
  guides(x =  guide_axis(angle = 45))
dev.off()


jpeg(file="provinciaboxplot.jpeg")
datoseda %>%
  ggplot(aes(x = reorder(provincia,(-superficie)), y = superficie, fill=provincia))+
  geom_boxplot() + 
  stat_summary(fun = mean, shape=20, 
               color='red') +
  stat_summary(aes(label=round(..y..)), 
               fun=mean, 
               geom="text", 
               size=3, 
               vjust = +1.5)+
  labs(title = 'Superficie quemada según las provincias', 
       subtitle = 'Distribución en conjunto') +
  xlab('Provincia') +
  ylab('Superficie Quemada') +
  theme(legend.position = "none") +
  guides(x =  guide_axis(angle = 45))
dev.off()



jpeg(file="dirboxplot.jpeg")
datoseda %>%
  ggplot(aes(x = reorder(dir,(-superficie)), y = superficie, fill=dir))+
  geom_boxplot() + 
  stat_summary(fun = mean, shape=20, 
               color='red') +
  stat_summary(aes(label=round(..y..)), 
               fun=mean, 
               geom="text", 
               size=3, 
               vjust = +1.5)+
  labs(title = 'Superficie quemada según la dirección del viento', 
       subtitle = 'Distribución en conjunto') +
  xlab('Dirección del viento') +
  ylab('Superficie Quemada') +
  theme(legend.position = "none") 
dev.off()



jpeg(file="superficiequemadaboxplot.jpeg")
datoseda %>%
  ggplot(aes(x = reorder(superficie_quemada,(-superficie)), y = superficie, fill=superficie_quemada))+
  geom_boxplot() + 
  stat_summary(fun = mean, shape=20, 
               color='red') +
  stat_summary(aes(label=round(..y..)), 
               fun=mean, 
               geom="text", 
               size=3, 
               vjust = +1.5)+
  labs(title = 'Superficie quemada en los incendios', 
       subtitle = 'Distribución en conjunto') +
  xlab('Superficie Quemada') +
  ylab('Superficie Quemada') +
  theme(legend.position = "none") +
  guides(x =  guide_axis(angle = 45))
dev.off()


jpeg(file="mesboxplot.jpeg")
datoseda %>%
  ggplot(aes(x = reorder(fe_month,(-superficie)), y = superficie, fill=fe_month))+
  geom_boxplot() + 
  stat_summary(fun = mean, shape=20, 
               color='red') +
  stat_summary(aes(label=round(..y..)), 
               fun=mean, 
               geom="text", 
               size=3, 
               vjust = +1.5)+
  labs(title = 'Meses en los que más superficie se ha quemado', 
       subtitle = 'Distribución en conjunto') +
  xlab('Mes') +
  ylab('Superficie Quemada') +
  theme(legend.position = "none") 
dev.off()








# mosaico superficie quemada en función de la zona
tabla = table(datoseda$zona, datoseda$superficie_quemada)
jpeg(file="mosaicozona.jpeg")
mosaicplot(tabla,
           main = 'Superficie Quemada según la zona',
           xlab = 'Zona',
           ylab = 'Superficie Quemada', dir = c('v','h'),
           las = 1,
           color = palette("Tableau")
)
dev.off()

