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

# Datos país

# Datos UNAL

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

grafica_radar <- function(datos, clase, etiqueta, mini = 100, maxi=300, orden = "normal", opacity = 1, llenar = T)
{
  pruebaspunUN <- c("PUNTAJE_GLOBAL",
                    "RAZONAMIENTO CUANTITATIVO",
                    "INGLÉS",
                    "LECTURA CRÍTICA",   
                    "COMPETENCIAS CIUDADANAS", 
                    "COMUNICACIÓN ESCRITA")
  
  if(llenar == T){
    fillG = "tonext"
  }
  else{
    fillG = "none"
  }
  
  colnames(datos)[colnames(datos) == clase] <- "claseG"
  
  datosgrafica <- data.frame(group_by(datos[c("claseG",pruebaspunUN)],
                                      claseG) %>% mutate (n = n()) %>%  #Cambiar aquí
                               summarise_all(c(mean), na.rm = T))
  grafica <- plot_ly(
    type = 'scatterpolar',
    fill = fillG,
    opacity = opacity,
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
  
  for(i in ordenG){
    
    grafica <- grafica %>%
      add_trace(
        r = matrix(datosgrafica[i,-c(1, ncol(datosgrafica))]),
        theta = pruebaspunUN,
        name = datosgrafica[i,1],
        opacity = opacity
      )
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
      legend = list(title = list(text = etiqueta, font = list(size = 15)), traceorder = ordenleyendas, orientation = "h", xanchor = "center", x = 0)
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

Saber_Global<- grafica_radar(saber19un, clase = "GLOBAL", etiqueta = " ")
Saber_GlobalZ<- grafica_radar(saber19un, clase = "GLOBAL", etiqueta = " ", maxi = 200)
Saber_Sede <- grafica_radar(saber19un, clase = "SEDE_NOMBRE_ADM", etiqueta = "Sede", maxi = 200)
Saber_Sexo <- grafica_radar(saber19un, clase = "SEXO", etiqueta = "Sexo", maxi = 220)
Saber_Tipo <- grafica_radar(saber19un, clase = "TIPO_ADM", etiqueta = "Tipo de admisión", maxi = 220)
Saber_PAES <- grafica_radar(saber19un, clase = "PAES", etiqueta = "PAES", maxi = 220)
Saber_PEAMA <- grafica_radar(saber19un, clase = "PEAMA", etiqueta = "PEAMA", maxi = 220)
Saber_PBM <- grafica_radar(saber19un, clase = "PBM", etiqueta = "PBM", maxi = 220)
Saber_Estrato <- grafica_radar(saber19un, clase = "ESTRATO", etiqueta = "Estrato", maxi = 220)
Saber_Area <- grafica_radar(saber19un, clase = "AREAC_SNIES", etiqueta = "Áreas del conocimiento", maxi = 220)

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








