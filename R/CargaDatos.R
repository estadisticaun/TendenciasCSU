library(readr)
library(tidyr)
library(dplyr)
library(readxl)
library(tidyr)
library(dplyr)
library(plotly)
library(highcharter)
library(stringr)
library(radarchart)

#Carga de datos UN
P2019_SaberPro_GEN <- read_excel("P2019 SaberPro GEN.xlsx")

#Defino dataframe UN
saber19un <- as.data.frame(P2019_SaberPro_GEN)
#Convierto columnas en factores
cols.to.factor <- sapply( saber19un, function(col) length(unique(col)) < 97 )
saber19un[cols.to.factor] <- lapply(saber19un[cols.to.factor] , factor)
str(saber19un)



#Carga de datos ICFES
P2019_Icfes <- read_delim("2019 descargaICFES - copiacsv.csv", ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "ISO-8859-1"))

str(P2019_SaberPro_GEN)
str(P2019_Icfes)

#Defino dataframe ICFES nacional
saber19icfes <- as.data.frame(P2019_Icfes)
#Convierto columnas en factores
cols.to.factor <- sapply(saber19icfes, function(col) length(unique(col)) < 100)
saber19icfes[cols.to.factor] <- lapply(saber19icfes[cols.to.factor] , factor)
str(saber19icfes)

#Defino dataframe ICFES UN
saber19icfesun <- saber19icfes %>% filter( INST_COD_INSTITUCION == 1101 |
                                           INST_COD_INSTITUCION == 1102 |
                                           INST_COD_INSTITUCION == 1103 |
                                           INST_COD_INSTITUCION == 1104)
cols.to.factor <- sapply(saber19icfesun, function(col) length(unique(col)) < 100)
saber19icfesun[cols.to.factor] <- lapply(saber19icfesun[cols.to.factor] , factor)
str(saber19icfesun)

