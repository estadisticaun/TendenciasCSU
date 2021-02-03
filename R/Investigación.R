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



# Grupos UNAL ----

# Grupos de Investigación ----

Grupos_UNAL <- tibble(Variable = "GRUPOS", 
                         YEAR = rep(2020, times = 3),
                         SEMESTRE = rep(1, length(YEAR)),
                         Clase = c("Categorizados", "Reconocidos", "Registrados"),
                         Total = c(578, 40, 335))

col <-   c("#f15a24", "#0071bc", "#8cc63f") 
Grupos_UN <- Plot.Barras(datos =  Grupos_UNAL,
             categoria = "GRUPOS",
             ano = "2020",
             periodo = ,  
             vertical  = T,
             colores   = col,
             libreria  = "plotly",
             labelEje  = "Número de grupos")


# Salvar htmls

Salvar(Grupos_UN, "Investigacion", "GI_unal.html")


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

Salvar(Grupos_Inter, "Investigacion", "GI_inter.html")

# Grupos de investigación Áreas OCDE

Grupos_OCDE <- tibble(Variable = "GRUPOSOCDE", 
                         YEAR = rep(2020, times = 6),
                         SEMESTRE = rep(1, length(YEAR)),
                         Clase = c("Ciencias naturales", "Ingeniería y tecnología", "Ciencias sociales", 
                                   "Ciencias médicas y de la salud", "Ciencias agrícolas", "Humanidades"),
                         Total = c(194, 110, 93, 92, 46, 43))

# Plot

col <-   c( "#6d6666",  # gris
            "#fbb03b", # amarillo
            "#29abe2", # azul claro
            "#c1272d",  # rojo
            "#8cc63f",  # verde
            "#93278f") # morado

Grupos_OCDEP <- Plot.Barras(datos = Grupos_OCDE,
               categoria = "GRUPOSOCDE",
               ano       = 2020,
               periodo   = ,
               vertical  = F,
               colores   = col,
               libreria  = "highcharter",
               titulo    = "Distribución de grupos categorizados de investigación UNAL <br> por áreas de la OCDE",
              estilo    = list(hc.Tema = 4, hc.Credits = "Año 2020"))

Salvar(Grupos_OCDEP, "Investigacion", "GI_ocde.html")                 
