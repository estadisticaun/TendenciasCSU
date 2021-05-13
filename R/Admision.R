# Librerías Requeridas

library(UnalData)
library(dplyr)
library(DT)          # version 0.4
library(highcharter) # version 0.5.0.9999

####################-
#ASPIRANTES
####################-

# Seleccionar variables 

Puntaje <- UnalData::Aspirantes %>% 
  filter(NIVEL == "Pregrado", !is.na(MOD_INS), YEAR >= 2019) %>% 
  mutate(Serie = paste(YEAR,SEMESTRE, sep = "-")) %>% 
  select(Serie, PTOTAL, TIPO_INS, INS_SEDE_NOMBRE, ADM_SEDE_NOMBRE, PAES, ESTRATO, SEXO, ADMITIDO)

# Aspirantes - general
Pun_Asp_global <- caja(datos = Puntaje, 
              titulo = "",
              eje = "Puntaje examen de admisión UNAL")
Pun_Asp_global

# Aspirantes - sedes

col <-   c( "#29abe2", # azul claro, Amazonia
            "#8cc63f", # verde, Bogotá
            "#c1272d", # Rojo, Caribe
            "#9e9ac8",  # Morado claro, De la Paz
            "#0071bc", # azul vivo, Manizales
            "#f15a24", # naranja, Medellin
            "#fbb03b", # amarillo, Orinoquia
            "#93278f", # morado, Palmira
            "#6d6666", # gris, Tumaco
            "#2ca25f" # verde esmeralda, Universidad
) 

Pun_Asp_sedes <- caja_n(datos= Puntaje %>% mutate(Agrupacion = INS_SEDE_NOMBRE)%>% filter(!is.na(Agrupacion)),
                        titulo = '', 
                        eje = "Puntaje examen de admisión UNAL", 
                        colores = col)

# Aspirantes - tipo de inscripción

col <-   c( "#f15a24", # naranja, PAES
            "#0071bc", # azul vivo, PEAMA
            "#8cc63f") # verde, Regular

Pun_Asp_tinscrip<- caja_n(datos = Puntaje %>% mutate(Agrupacion = TIPO_INS) %>% filter(!is.na(TIPO_INS)), 
                              titulo = '', 
                              eje = "Puntaje examen de admisión UNAL",
                              colores = col)


# Aspirantes - PAES

col <-   c( "#0071bc", # azul vivo, comunidades indigenas
            "#9e9ac8",  # Morado claro, De La Paz
            "#fbb03b", # amarillo, mejores bachilleres
            "#6d6666", # gris, mejores bachilleres municipios pobres
            "#f15a24", # naranja, población afro
            "#8cc63f") # verde, victimas del conflicto

Pun_Asp_paes <- caja_n(datos = Puntaje %>% mutate(Agrupacion = PAES) %>% filter(!is.na(PAES)),
                          titulo = '',
                          eje = "Puntaje examen de admisión UNAL",
                          colores = col)


# Aspirantes - estrato

col <-   c( "#8cc63f", # verde, estrato 2 o menos
            "#f15a24", # naranja, estrato 3
            "#0071bc", # azul vivo, estrato 4 o más
            "#6d6666") # gris, ND/NE

Pun_Asp_estrato <- caja_n(datos= Puntaje %>% mutate(Agrupacion = ESTRATO), 
                       titulo = '', 
                       eje = "Puntaje examen de admisión UNAL", 
                       colores = col)

# Aspirantes - sexo

col <-   c( "#29abe2", # azul claro, Amazonia
            "#c1272d" # Rojo, Caribe
            ) 

Pun_Asp_sexo <- caja_n(datos= Puntaje %>% mutate(Agrupacion = SEXO), 
                        titulo = '', 
                        eje = "Puntaje examen de admisión UNAL", 
                        colores = col)

####################-
# ADMITIDOS
####################-

# Seleccionar variables 

PuntajeA <- UnalData::Aspirantes %>% 
  filter(ADMITIDO == "Sí", NIVEL == "Pregrado", !is.na(MOD_INS), YEAR >= 2019) %>% 
  mutate(Serie = paste(YEAR,SEMESTRE, sep = "-")) %>% 
  select(Serie, PTOTAL, TIPO_INS, INS_SEDE_NOMBRE, ADM_SEDE_NOMBRE, PAES, ESTRATO, SEXO, ADMITIDO)

# Aspirantes - general
Pun_Adm_global <- caja(datos = PuntajeA, 
                       titulo = "",
                       eje = "Puntaje examen de admisión UNAL")

# Aspirantes - sedes

col <-   c( "#29abe2", # azul claro, Amazonia
            "#8cc63f", # verde, Bogotá
            "#c1272d", # Rojo, Caribe
            "#9e9ac8",  # Morado claro, De la Paz
            "#0071bc", # azul vivo, Manizales
            "#f15a24", # naranja, Medellin
            "#fbb03b", # amarillo, Orinoquia
            "#93278f", # morado, Palmira
            "#6d6666", # gris, Tumaco
            "#2ca25f" # verde esmeralda, Universidad
) 

Pun_Adm_sedes <- caja_n(datos= PuntajeA %>% mutate(Agrupacion = INS_SEDE_NOMBRE)%>% filter(!is.na(Agrupacion)),
                        titulo = '', 
                        eje = "Puntaje examen de admisión UNAL", 
                        colores = col)

# Aspirantes - tipo de inscripción

col <-   c( "#f15a24", # naranja, PAES
            "#0071bc", # azul vivo, PEAMA
            "#8cc63f") # verde, Regular

Pun_Adm_tinscrip <- caja_n(datos = PuntajeA %>% mutate(Agrupacion = TIPO_INS) %>% filter(!is.na(TIPO_INS)), 
                          titulo = '', 
                          eje = "Puntaje examen de admisión UNAL",
                          colores = col)


# Aspirantes - PAES

col <-   c( "#0071bc", # azul vivo, comunidades indigenas
            "#9e9ac8",  # Morado claro, De La Paz
            "#fbb03b", # amarillo, mejores bachilleres
            "#6d6666", # gris, mejores bachilleres municipios pobres
            "#f15a24", # naranja, población afro
            "#8cc63f") # verde, victimas del conflicto

Pun_Adm_paes <- caja_n(datos = PuntajeA %>% mutate(Agrupacion = PAES) %>% filter(!is.na(PAES)),
                       titulo = '',
                       eje = "Puntaje examen de admisión UNAL",
                       colores = col)


# Aspirantes - estrato

col <-   c( "#8cc63f", # verde, estrato 2 o menos
            "#f15a24", # naranja, estrato 3
            "#0071bc", # azul vivo, estrato 4 o más
            "#6d6666") # gris, ND/NE

Pun_Adm_estrato <- caja_n(datos= PuntajeA %>% mutate(Agrupacion = ESTRATO), 
                          titulo = '', 
                          eje = "Puntaje examen de admisión UNAL", 
                          colores = col)

# Aspirantes - sexo

col <-   c( "#29abe2", # azul claro, Amazonia
            "#c1272d" # Rojo, Caribe
) 

Pun_Adm_sexo <- caja_n(datos= PuntajeA %>% mutate(Agrupacion = SEXO), 
                       titulo = '', 
                       eje = "Puntaje examen de admisión UNAL", 
                       colores = col)
