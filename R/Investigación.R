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
            titulo = "Evolución grupos de investigación categorizados en SCIENTI",
            labelY = "Número de grupos de investigación",
            estilo    = list(hc.Tema = 4, hc.Slider = FALSE,
                             hc.Credits = "Total País y total Universidad Nacional de Colombia, 2002 -2020")
            )

# Salvar htmls

Salvar(Grupos_pais, "Investigacion", "GI_pais.html")
  
          
# Grupos Intrasedes ----

# Grupos de Investigación ----

Grupos_InterUN <- tibble(Variable = "GRUPOSINTERUN", 
                      YEAR = rep(2020, times = 4),
                      SEMESTRE = rep(1, length(YEAR)),
                      Clase = c("Única sede", "2 Sedes", "3 Sedes", "4 o más sedes"),
                      Total = c(466, 91, 17, 4))

# Plot

col <-   c("#6d6666", "#f15a24", "#0071bc", "#8cc63f") 
            
Grupos_Inter <- Plot.Torta(datos = Grupos_InterUN, categoria = "GRUPOSINTERUN", 
                           periodo = 1, colores = col,
                           titulo    = "Grupos categorizados de investigación UNAL de acuerdo a las <br> sedes de base de sus integrantes",
                           estilo    = list(hc.Tema = 4, hc.Credits = "Año 2020"))
                 
