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
P2019_Icfes <- read_delim("Datos/2019 descargaICFES.csv", ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "ISO-8859-1"))

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


# CARGA BASE DE DATOS SABER PRO GENERAL/PAÍS

SBPRO_2019_GEN <- read_delim("Datos/2019 descargaICFES.csv", ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "ISO-8859-1"))

#Defino dataframe ICFES nacional
SBPRO_2019_GEN <- as.data.frame(SBPRO_2019_GEN)

# SELECCIONAR VARIABLES DE INTERÉS

SBPRO_2019_GEN <- SBPRO_2019_GEN %>% select(c(INST_COD_INSTITUCION:INST_NOMBRE_INSTITUCION, 
                                              MOD_RAZONA_CUANTITAT_PUNT:PUNT_GLOBAL))


# CREAR VARIABLES DE INTERÉS

# Crear Variable Unal

SBPRO_2019_GEN <- SBPRO_2019_GEN %>% 
  mutate(Unal =  case_when(.$INST_COD_INSTITUCION %in% c(1101, 1102, 1103, 1104, 1124, 1125, 1126, 9920) ~ "UN",
                           TRUE ~ "Resto IES"))


# Crear Sedes Unal

SBPRO_2019_GEN <- SBPRO_2019_GEN %>% 
  mutate(Sedes =  case_when(.$INST_COD_INSTITUCION == 1101 ~ "UN-Bogota",
                            .$INST_COD_INSTITUCION == 1102 ~ "UN-Medellin",
                            .$INST_COD_INSTITUCION == 1103 ~ "UN-Manizales",
                            .$INST_COD_INSTITUCION == 1104 ~ "UN-Palmira",
                            .$INST_COD_INSTITUCION == 1124 ~ "UN-Orinoquia",
                            .$INST_COD_INSTITUCION == 1125 ~ "UN-Amazonia",
                            .$INST_COD_INSTITUCION == 1126 ~ "UN-Caribe",
                            .$INST_COD_INSTITUCION == 9920 ~ "UN-Tumaco",
                            TRUE ~ "Resto IES"))


# Crear G12

SBPRO_2019_GEN <- SBPRO_2019_GEN %>% 
  mutate(G12 =  case_when(.$INST_COD_INSTITUCION == 1813  ~ "U. de los Andes",
                          .$INST_COD_INSTITUCION == 1714  ~ "U. del Rosario",
                          .$INST_COD_INSTITUCION == 1706  ~ "U. Externado",
                          .$INST_COD_INSTITUCION == 1828  ~ "ICESI",
                          .$INST_COD_INSTITUCION %in% c(1201, 1219, 1220, 1221, 1223) ~ "U. de Antioquia",
                          .$INST_COD_INSTITUCION == 1712  ~ "EAFIT",
                          .$INST_COD_INSTITUCION == 1203  ~ "U. del Valle",
                          .$INST_COD_INSTITUCION %in% c(1701, 1702)  ~ "U. Javeriana",
                          .$INST_COD_INSTITUCION == 1204  ~ "UIS",
                          .$INST_COD_INSTITUCION == 1713  ~ "U. del Norte",
                          .$INST_COD_INSTITUCION %in% c(1710, 1723, 1727, 1730)  ~ "U. Bolivariana",
                          .$INST_COD_INSTITUCION %in% c(1101, 1102, 1103, 1104)  ~ "U. Nacional",
                          TRUE ~ "Resto IES"))


# Crear G16 (sedes Unal)
SBPRO_2019_GEN <- SBPRO_2019_GEN %>% 
  mutate(G15 =  case_when(.$INST_COD_INSTITUCION == 1813  ~ "U. de los Andes",
                          .$INST_COD_INSTITUCION == 1714  ~ "U. del Rosario",
                          .$INST_COD_INSTITUCION == 1706  ~ "U. Externado",
                          .$INST_COD_INSTITUCION == 1828  ~ "ICESI",
                          .$INST_COD_INSTITUCION %in% c(1201, 1219, 1220, 1221, 1223) ~ "U. de Antioquia",
                          .$INST_COD_INSTITUCION == 1712  ~ "EAFIT",
                          .$INST_COD_INSTITUCION == 1203  ~ "U. del Valle",
                          .$INST_COD_INSTITUCION %in% c(1701, 1702)  ~ "U. Javeriana",
                          .$INST_COD_INSTITUCION == 1204  ~ "UIS",
                          .$INST_COD_INSTITUCION == 1713  ~ "U. del Norte",
                          .$INST_COD_INSTITUCION %in% c(1710, 1723, 1727, 1730)  ~ "U. Bolivariana",
                          .$INST_COD_INSTITUCION == 1101  ~ "UN-Bogota",
                          .$INST_COD_INSTITUCION == 1102  ~ "UN-Medellin",
                          .$INST_COD_INSTITUCION == 1103  ~ "UN-Manizales",
                          .$INST_COD_INSTITUCION == 1104  ~ "UN-Palmira",
                          TRUE ~ "Resto IES"))


# Distribución puntajes globales, unal y sedes

### crear base de datos 

Unal <- SBPRO_2019_GEN %>% select(PUNT_GLOBAL, Unal) %>% rename(Categoria = Unal)
sedes <- SBPRO_2019_GEN %>% select(PUNT_GLOBAL, Sedes) %>% 
  filter(Sedes != "Resto IES") %>% rename(Categoria = Sedes)
Global <- bind_rows(Unal, sedes)


