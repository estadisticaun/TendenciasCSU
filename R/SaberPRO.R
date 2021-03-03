# Librerías requeridas

library(readr)
library(tidyr)
library(dplyr)
library(readxl)
library(plotly)
library(highcharter)
library(stringr)
library(radarchart)
library(htmlwidgets)

# Importar datos

# DATOS UNAL

P2019_SaberPro_GEN <- read_excel("Datos/P2019 SaberPro GEN.xlsx")

# Transformar datos
saber19un <- as.data.frame(P2019_SaberPro_GEN)
#Convierto columnas en factores
cols.to.factor <- sapply( saber19un, function(col) length(unique(col)) < 97 )
saber19un[cols.to.factor] <- lapply(saber19un[cols.to.factor] , factor)

# Función salvar widges

Salvar <- function(objeto, ruta, nombre){
  saveWidget(objeto,
             file = file.path(str_sub(getwd(), 1, str_length(getwd())),
                              ruta,
                              nombre),
             selfcontained = F, libdir = "libraryjs")
  
}


# Función para radares

grafica_radar <- function(datos, clase, etiqueta, mini = 100, maxi=300, orden = "normal", opacity = 1, llenar = 1, leyendav = F, colores = c())
{
  pruebaspunUN <- c("PUNTAJE_GLOBAL",
                    "RAZONAMIENTO CUANTITATIVO",
                    "INGLÉS",
                    "LECTURA CRÍTICA",   
                    "COMPETENCIAS CIUDADANAS", 
                    "COMUNICACIÓN ESCRITA")
  
  if(llenar == 1){
    fillG = "tonext"
  }
  else if(llenar == 2){
    fillG = "toself"
  }
  else if (llenar == 0) {
    fillG = "none"
  }
  
  
  if(leyendav){
    ori = "v"
  }
  else{
    ori = "h"
  }
  
  
  colnames(datos)[colnames(datos) == clase] <- "claseG"
  
  datosgrafica <- data.frame(group_by(datos[c("claseG",pruebaspunUN)],
                                      claseG) %>% mutate (n = n()) %>%  #Cambiar aquí
                               summarise_all(c(mean), na.rm = T))
  grafica <- plot_ly(
    type = 'scatterpolar',
    fill = fillG,
    #opacity = opacity,
    mode = "lines+markers"
  )
  
  if(orden == "normal"){
    ordenG = c(1:nrow(datosgrafica))
    ordenleyendas = "normal"
  }
  else{
    ordenG = c(nrow(datosgrafica):1)
    ordenleyendas = "reversed"
  }
  
  if(is.null(colores))
  {
    for(i in ordenG){
      
      grafica <- grafica %>%
        add_trace(
          r = matrix(datosgrafica[i,-c(1, ncol(datosgrafica))]),
          theta = pruebaspunUN,
          name = datosgrafica[i,1],
          opacity = opacity,
          hoverinfo = "text",
          text = str_c(datosgrafica[i,1], '<br> Promedio: ', matrix(round(datosgrafica[i,-c(1, ncol(datosgrafica))], 2)), '<br> N: ', matrix(datosgrafica[i, ncol(datosgrafica)]))
        )
    }
  }
  else{
    for(i in ordenG){
      grafica <- grafica %>%
        add_trace(
          r = matrix(datosgrafica[i,-c(1, ncol(datosgrafica))]),
          theta = pruebaspunUN,
          name = datosgrafica[i,1],
          opacity = opacity,
          line = list(
            color = colores[i]
          ),
          marker = list(
            color = colores[i]
          ),
          fillcolor = list(
            color = colores[i]
          ),
          hoverinfo = "text",
          text = str_c(datosgrafica[i,1], '<br> Promedio: ', matrix(round(datosgrafica[i,-c(1, ncol(datosgrafica))], 2)), '<br> N: ', matrix(datosgrafica[i, ncol(datosgrafica)]))
        )
    }
  }
  
  grafica <- grafica %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(mini,maxi)
        )
      ),
      title = list(
        text = "",
        x = 0.98,
        y = 0.8
      ),
      legend = list(title = list(text = etiqueta, font = list(size = 15)), traceorder = ordenleyendas, orientation = ori)
    )
  
  grafica
}

#Datos: dataframe.  Sin Default.
#Clase: Variable a segregar.  Sin Default.
#mini = valor a iniciar la escala (centro del circulo). Default = 100.
#maxi = valor máximo de la escala (borde del circulo).  Default = 300.
#order = "normal" si organiza las capas por orden alfabetico, Default = "normal"
#        "inverso" si organiza las capas por orden alfabetico inverso.
#opacity = transparencia de las figuras. Default = 1.
#llenar = True si colorea la figura,
#         False si no colorea la figura. 

# Gráficos de interes

Saber_Global <- grafica_radar(saber19un, clase = "GLOBAL", etiqueta = " ")
Saber_GlobalZ <- grafica_radar(saber19un, clase = "GLOBAL", etiqueta = " ", maxi = 200)
Saber_Sede <- grafica_radar(saber19un, clase = "SEDE_NOMBRE_ADM", etiqueta = "Sede", maxi = 200)
Saber_Sexo <- grafica_radar(saber19un, clase = "SEXO", etiqueta = "Sexo", maxi = 220)
Saber_Tipo <- grafica_radar(saber19un, clase = "TIPO_ADM", etiqueta = "Tipo de admisión", maxi = 220)
Saber_PAES <- grafica_radar(saber19un, clase = "PAES", etiqueta = "PAES", maxi = 220)
Saber_PEAMA <- grafica_radar(saber19un, clase = "PEAMA", etiqueta = "PEAMA", maxi = 220)
Saber_PBM <- grafica_radar(saber19un, clase = "PBM", etiqueta = "PBM", maxi = 220)
Saber_Estrato <- grafica_radar(saber19un, clase = "ESTRATO", etiqueta = "Estrato", maxi = 220)
Saber_Area <- grafica_radar(saber19un, clase = "AREAC_SNIES", etiqueta = "Área SNIES", maxi = 220)

# Salvar htmls

Salvar(Saber_Global, "SaberPro", "General.html")
Salvar(Saber_GlobalZ, "SaberPro", "Generalz.html")
Salvar(Saber_Sede, "SaberPro", "Sede.html")
Salvar(Saber_Sexo, "SaberPro", "Sexo.html")
Salvar(Saber_Tipo, "SaberPro", "Tipo.html")
Salvar(Saber_PAES, "SaberPro", "Paes.html")
Salvar(Saber_PEAMA, "SaberPro", "Peama.html")
Salvar(Saber_PBM, "SaberPro", "Pbm.html")
Salvar(Saber_Estrato, "SaberPro", "Estrato.html")
Salvar(Saber_Area, "SaberPro", "Area.html")


# DATOS PAIS

SBPRO_2019_GEN <- read_delim("Datos/2019 descargaICFES.csv", ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "ISO-8859-1"))

#Defino dataframe ICFES nacional
SBPRO_2019_GEN <- as.data.frame(SBPRO_2019_GEN)


# SELECCIONAR VARIABLES DE INTERÉS

SBPRO_2019_GEN <- SBPRO_2019_GEN %>% select(c(INST_COD_INSTITUCION:INST_NOMBRE_INSTITUCION, 
                                              MOD_RAZONA_CUANTITAT_PUNT:PUNT_GLOBAL))


# RENOMBRAR VARIABLES DEL ICFES

SBPRO_2019_GEN <- SBPRO_2019_GEN %>% rename(PUNTAJE_GLOBAL = PUNT_GLOBAL,
                         `RAZONAMIENTO CUANTITATIVO` = MOD_RAZONA_CUANTITAT_PUNT,
                         INGLÉS = MOD_INGLES_PUNT,
                         `LECTURA CRÍTICA` = MOD_LECTURA_CRITICA_PUNT,
                         `COMPETENCIAS CIUDADANAS` = MOD_COMPETEN_CIUDADA_PUNT,
                         `COMUNICACIÓN ESCRITA` = MOD_COMUNI_ESCRITA_PUNT)


# CREAR VARIABLES DE INTERÉS

# Crear Variable General

SBPRO_2019_GEN <- SBPRO_2019_GEN %>% mutate(SaberPais = "Resultados SaberPRO2019 País")
  
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


# Crear G2

SBPRO_2019_GEN <- SBPRO_2019_GEN %>% 
  mutate(G2 =  case_when(.$INST_COD_INSTITUCION == 1813  ~ "U. de los Andes",
                         .$INST_COD_INSTITUCION %in% c(1101, 1102, 1103, 1104)  ~ "U. Nacional",
                          TRUE ~ "Resto IES"))

# Crear G3

SBPRO_2019_GEN <- SBPRO_2019_GEN %>% 
  mutate(G3 =  case_when(.$INST_COD_INSTITUCION == 1813  ~ "U. de los Andes",
                         .$INST_COD_INSTITUCION == 1101  ~ "UN-Bogota",
                         .$INST_COD_INSTITUCION == 1102  ~ "UN-Medellin",
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





#Convierto columnas en factores
cols.to.factor <- sapply(SBPRO_2019_GEN, function(col) length(unique(col)) < 100)
SBPRO_2019_GEN[cols.to.factor] <- lapply(SBPRO_2019_GEN[cols.to.factor] , factor)


# Gráficos de interes

Saber_Global_Pais<- grafica_radar(SBPRO_2019_GEN, clase = "SaberPais", etiqueta = " ", mini = 0)
Saber_Global_Unal<- grafica_radar(SBPRO_2019_GEN, clase = "Unal", etiqueta = " ", mini = 0)
Saber_Global_G2<- grafica_radar(SBPRO_2019_GEN, clase = "G2", etiqueta = " ", mini = 100, maxi = 220)
Saber_Global_G3<- grafica_radar(SBPRO_2019_GEN, clase = "G3", etiqueta = " ", mini = 100, maxi = 220)
Saber_Global_G12<- grafica_radar(SBPRO_2019_GEN, clase = "G12", etiqueta = " ", mini = 100, maxi = 220)
Saber_Global_G15<- grafica_radar(SBPRO_2019_GEN, clase = "G15", etiqueta = " ", mini = 100, maxi = 220)


# Salvar htmls

Salvar(Saber_Global_Pais, "SaberPro", "GeneralPais.html")
Salvar(Saber_Global_Unal, "SaberPro", "GeneralUnal.html")
Salvar(Saber_Global_G2, "SaberPro", "Grupo2.html")
Salvar(Saber_Global_G3, "SaberPro", "Grupo3.html")
Salvar(Saber_Global_G12, "SaberPro", "Grupo12.html")
Salvar(Saber_Global_G15, "SaberPro", "Grupo15.html")

