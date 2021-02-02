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

# Función salvar widges

Salvar <- function(objeto, ruta, nombre){
  saveWidget(objeto,
             file = file.path(str_sub(getwd(), 1, str_length(getwd())),
                              ruta,
                              nombre),
             selfcontained = F, libdir = "libraryjs")
  
}

# Grupos de Investigación ----

Grupos_pais <- tibble(Variable = "GRUPOSP", 
                      YEAR = rep(2002:2020, each = 2),
                      SEMESTRE = rep(1, length(YEAR)),
                      Clase = rep(c("País", "UNAL"), times = length(YEAR)/2),
                      Total = c(544, 105, 809, 105, 1445, 105, 1825, 96, 2148, 412, 2456, 412, 
                                3371, 412, 4075, 577, 4074, 489, 4068, 487, 4055, 487, 3760, 444,
                                3840, 488, 4458, 535, 4459, 535, 4566, 510, 4566, 510, 5353, 578,
                                5353,	578))

# Plot


Grupos_pais <- Plot.Series(datos = Grupos_pais, categoria = "GRUPOSP", col = c("#116BEE", "#E62272"),
            libreria = "highcharter", 
            titulo = "Evolución Grupos de investigación categorizados en SCIENTI",
            labelY = "Número de grupos de investigación",
            estilo    = list(hc.Tema = 4, hc.Slider = TRUE,
                             hc.Credits = "Total País y total Universidad Nacional de Colombia, 2002 -2020")
            )

# Salvar htmls

Salvar(Grupos_pais, "Investigacion", "GI_pais.html")
            
