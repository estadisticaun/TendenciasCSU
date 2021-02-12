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
            labelY = "Número de grupos de investigación (k: miles)",
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

# Grupos de investigación Áreas OCDE ----

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
               labelEje  = "Número de grupos",
               titulo    = "Distribución de grupos categorizados de investigación UNAL <br> por áreas de la OCDE",
              estilo    = list(hc.Tema = 4, hc.Credits = "Año 2020"))

Salvar(Grupos_OCDEP, "Investigacion", "GI_ocde.html") 


# Revistas institucionales ----

RevistasP <- tibble(Variable = "REVISTAS",
                      YEAR = rep(2003:2020, each = 2),
                      SEMESTRE = rep(1, length(YEAR)),
                      Clase = rep(c("UNAL", "País"), times = length(YEAR)/2),
                      Total = c(15,	62,	22,	119,	28,	156,	38,	202,	40,	221,	38,	263,
                                42,	309,	44,	372,	45,	466,	44, 513,	39,	515,	43,	526,
                                43,	526,	43,	526,	30,	244,	30,	246,	30,	246,	31,	275))

# Plot

Revistas <- Plot.Series(datos = RevistasP, categoria = "REVISTAS", col = c("#116BEE", "#E62272"),
                           libreria = "highcharter",
                           titulo = "Evolución de las revistas de la UNAL en el índice Bibliográfico PUBLINDEX",
                           labelY = "Total Revistas",
                           estilo    = list(hc.Tema = 4, hc.Slider = FALSE,
                                            hc.Credits = "Comparación país. 2003-2020")
)

Salvar(Revistas, "Investigacion", "Revistas.html")



# Clasificación Revistas ----

RevistasCla <- tibble(Variable = "REVISTASCLA",
                         YEAR = rep(2020, times = 4),
                         SEMESTRE = rep(1, length(YEAR)),
                         Clase = c("A1", "A2", "B", "C"),
                         Total = c(0, 1, 12, 18))

# Plot

col <-   c("#6d6666", "#f15a24", "#0071bc", "#8cc63f")

RevistasClaU <- Plot.Torta(datos = RevistasCla, categoria = "REVISTASCLA",
                           periodo = 1, colores = col,
                           titulo    = "Clasificación Revistas de la Universidad Nacional de Colombia",
                           estilo    = list(hc.Tema = 4, hc.Credits = "Año 2020"))

Salvar(RevistasClaU, "Investigacion", "RevistasClaU.html")

# Productos Comité Puntaje ----

Comite <- tibble(Variable = "TOTAL",
                    YEAR = rep(2004:2019, each = 1),
                    SEMESTRE = rep(1, length(YEAR)),
                    Clase = rep(c("Total"), times = length(YEAR)),
                    Total = c(3300, 3716, 4620, 5412, 5693, 5878, 6254, 6853,
                              6758,	6120,	6259,	5924,	5888,	5997,	5449,	5532))

ComiteP <- Plot.Series(datos = Comite, categoria = "TOTAL", col = c("#116BEE"),
                        libreria = "highcharter",
                        titulo = "Evolución de productos sometidos al Comité de Puntaje <br> de la Universidad Nacional de Colombia",
                        labelY = "Total Productos (k: miles)",
                        estilo    = list(hc.Tema = 4, hc.Slider = FALSE,
                                         hc.Credits = "Periodo: 2004-2019"))

Salvar(ComiteP, "Investigacion", "Comite.html")


# Productos Comité Puntaje por tipologías ----

Comite_Tipo <- tibble(Variable = "COMITET",
                 YEAR = rep(2004:2019, each = 5),
                 SEMESTRE = rep(1, length(YEAR)),
                 Clase = rep(c("Creación artística", "Divulgación científica", "Formación", "Nuevo conocimiento","Premios"), times = length(YEAR)/5),
                 Total = c(84, 602, 384, 2195, 35,
                           94, 685, 413, 2491, 33,
                           133, 884, 592, 2989, 22,
                           126, 1130, 719, 3388, 49,
                           129, 1299, 742, 3488, 35,
                           101, 1487, 845, 3424, 21,
                           124, 1545, 959, 3596, 30,
                           108, 1416, 1324, 3993, 12,
                           93, 1332, 1451, 3864, 18,
                           89, 1186, 1165, 3661, 19,
                           64, 1094, 1588, 3492, 21,
                           66, 1070, 1610, 3167, 11,
                           68, 1010, 1720, 3079, 11,
                           45, 1039, 1882, 3029, 2,
                           26, 856, 1842, 2722, 3,
                           32, 670, 2299, 2525, 6))


# Plot

col <-   c( "#6d6666",  # gris
            "#fbb03b", # amarillo
            "#29abe2", # azul claro
            "#c1272d",  # rojo
            "#8cc63f")  # verde


ComitePT <- Plot.Series(datos = Comite_Tipo, categoria = "COMITET", col = col,
                       libreria = "highcharter",
                       titulo = "Evolución de productos sometidos al Comité de Puntaje <br> de la Universidad Nacional de Colombia por tipología",
                       labelY = "Total Productos (k: miles)",
                       estilo    = list(hc.Tema = 4, hc.Slider = FALSE,
                                        hc.Credits = "Periodo: 2004-2019"))
     
Salvar(ComitePT, "Investigacion", "Comitet.html")


# Programas académicos ----


Programas <- tibble(Variable = "TOTAL",
                 YEAR = rep(1994:2020, each = 1),
                 SEMESTRE = rep(1, length(YEAR)),
                 Clase = rep(c("Total"), times = length(YEAR)),
                 Total = c(189, 203, 211, 231, 247, 259, 267, 279, 303,
                           308,	332, 344, 371, 386, 395, 425, 435, 438,
                           425, 437, 453, 455, 444, 450, 455, 468, 469))

ProgramasU <- Plot.Series(datos = Programas, categoria = "TOTAL", col = c("#116BEE"),
                       libreria = "highcharter",
                       titulo = "Evolución del número de programas académicos en la Universidad Nacional de Colombia",
                       labelY = "Total programas académicos",
                       estilo    = list(hc.Slider = FALSE,
                                        hc.Credits = "Periodo: 1994-2020"))


Salvar(ProgramasU, "Investigacion", "Programas.html")

# Programas académicos por modalidad----

ProgramasN <- tibble(Variable = "MODALIDAD",
                    YEAR = rep(1994:2020, each = 2),
                    SEMESTRE = rep(1, length(YEAR)),
                    Clase = rep(c("Pregrado", "Postgrado"), times = length(YEAR)/2),
                    Total = c(72, 117, 74, 129, 74, 137, 82, 149, 88, 159,
                              91, 168, 93, 174, 94, 185, 94, 209, 94, 214,
                              94, 238, 94, 250, 94, 277, 94, 292, 94, 301, 
                              94, 331, 94, 341, 94, 344, 94, 331, 94, 343,
                              94, 359, 94, 361, 95, 349, 95, 355, 95, 360,
                              102, 366, 102, 367))

# Plot

ProgramasNivel <- Plot.Series(datos = ProgramasN, categoria = "MODALIDAD", col = c("#116BEE", "#E62272"),
                        libreria = "highcharter",
                        titulo = "Evolución del número de programas académicos por modalidad de formación",
                        labelY = "Total programas académicos",
                        estilo    = list(hc.Slider = FALSE, hc.Credits = "Periodo: 1994-2020"))


Salvar(ProgramasNivel, "Investigacion", "Modalidad.html")


# Programas académicos de postgrado ----

Postgrado <- tibble(Variable = "POSTGRADO",
                     YEAR = rep(1994:2020, each = 4),
                     SEMESTRE = rep(1, length(YEAR)),
                     Clase = rep(c("Especialización", "Especialidades médicas", "Maestría", "Doctorado"), times = length(YEAR)/4),
                     Total = c(36, 30, 46, 5, 42, 30, 47, 10, 45, 31, 50, 11, 48, 31, 59, 11,
                               54, 31, 63, 11, 56, 33, 66, 13, 54, 33, 74, 13, 59, 33, 79, 14,
                               79, 33, 80, 17, 76, 34, 85, 19, 93, 34, 90, 21, 95, 35, 92, 28,
                               97, 35, 113, 32, 105, 36, 116, 35, 110, 36, 120, 35, 115, 38, 132,
                               46, 115, 38, 135, 53, 112, 38, 140, 54, 83, 39, 153, 56, 95, 39, 153,
                               56, 101, 40, 160, 58, 98, 40, 161, 62, 80, 40, 165, 64, 83, 40, 167, 
                               65, 84, 40, 168, 68, 88, 40, 169, 69, 89, 40, 169, 69))

# Plot

col <-   c( "#6d6666",  # gris
            "#29abe2", # azul claro
            "#c1272d",  # rojo
            "#8cc63f")  # verde


PostgradoU <- Plot.Series(datos = Postgrado, categoria = "POSTGRADO", col = col,
                              libreria = "highcharter",
                              titulo = "Evolución del número de programas académicos de postgrado",
                              labelY = "Total programas académicos",
                              estilo    = list(hc.Slider = FALSE, hc.Credits = "Periodo: 1994-2020"))

Salvar(PostgradoU, "Investigacion", "Postgrado.html")


# Investigadores por Sexo ----

Inves_sexo <- tibble(Variable = "INVSEXO",
                      YEAR = rep(2020, times = 2),
                      SEMESTRE = rep(2, length(YEAR)),
                      Clase = c("Hombres", "Mujeres"),
                      Total = c(827, 365))

# Plot

col <-   c("#f15a24", "#8cc63f")

Investigadores_sexo <- Plot.Torta(datos = Inves_sexo, categoria = "INVSEXO",
                           periodo = 2, colores = col,
                           titulo    = "Distribución de docentes investigadores por sexo",
                           estilo    = list(hc.Credits = "Año 2020"))

Salvar(Investigadores_sexo, "Investigacion", "InvSexo.html")

# Cobertura bienestar ----

Ubienestar <- tibble(Variable = "BIENESTAR",
                    YEAR = rep(2015:2020, each = 12),
                    SEMESTRE = rep(c(1, 2), each = 6, length(YEAR)/12),
                    Clase = rep(c("Gestión y fomento socioeconómico", "Salud", "Actividad Física y Deporte", "Cultura", "Acompañamiento Integral", "Otras Áreas"), times = length(YEAR)/6),
                    Total = c(6818,	62913,	11909,	8333,	23766,	991,
                              7624,	71254,	16608,	8780,	33459,	991,
                              7468,	61713,	54426,	12383,	29227,	1007,
                              10244,	82122,	32082,	14430,	71238,	1007,
                              9682,	67604,	47925,	21319,	27361,	972,
                              9174,	61379,	87374,	45787,	36510,	972,
                              8922,	53393,	113278,	60501,	37863,	975,
                              10906,	68091,	137278,	51389,	31534,	975,
                              9118,	62970,	105826,	41298,	32262,	961,
                              10026,	56804,	120821,	37508,	34752,	961,
                              6865,	43943,	49636,	11432,	32772,	966,
                              6678,	31451,	53333,	23108,	37461,	966))



# Plot

col <-   c( "#6d6666",  # gris
            "#fbb03b", # amarillo
            "#29abe2", # azul claro
            "#c1272d",  # rojo
            "#8cc63f",  # verde
            "#93278f") # morado


Bienestar <- Plot.Series(datos = Ubienestar, categoria = "BIENESTAR", col = col,
                        libreria = "highcharter",
                        titulo = "Evolución de beneficiarios programas de bienestar <br> de la Universidad Nacional de Colombia por áreas",
                        labelY = "Total beneficiarios",
                        estilo    = list(hc.Tema = 6, hc.Slider = FALSE,
                                         hc.Credits = "Periodo: 2015-2020"))


Salvar(Bienestar, "Investigacion", "Bienestar.html")



# Cobertura G. Socieconómica ----

Ugyf <- tibble(Variable = "ECONÓMICO",
               YEAR = rep(2005:2020, each = 12),
               SEMESTRE = rep(c(1, 2), each = 6, length(YEAR)/12),
               Clase = rep(c("Préstamo estudiantil", "Apoyo para el alojamiento", "Apoyo alimentario", "Apoyo para el transporte", "Apoyo Económico", "Otros apoyos"), times = length(YEAR)/6),
               Total = c(3978,	629,	1207,	NA,	NA,	NA,
                         3798,	527,	1210,	NA,	NA,	NA,
                         3435,	586,	1093,	205,	NA,	144,
                         3131,	597,	1149,	271,	NA,	150,
                         3027,	627,	1122,	401,	NA,	147,
                         2872,	615,	1231,	529,	NA,	1473,
                         2690,	516,	1135,	553,	NA,	309,
                         2630,	531,	1270,	255,	NA,	418,
                         2588,	509,	1355,	443,	NA,	356,
                         2550,	550,	1616,	366,	NA,	379,
                         2485,	521,	1640,	646,	NA,	455,
                         2106,	526,	2238,	699,	NA,	472,
                         1726,	555,	1797,	885,	NA,	605,
                         1580,	676,	2025,	737,	NA,	578,
                         1570,	621,	1823,	1474,	NA,	383,
                         1425,	621,	1881,	1541,	NA,	377,
                         1097,	494,	2715,	1333,	NA,	1511,
                         948,	456,	3614,	1360,	NA,	1410,
                         769,	518,	3935,	1666,	NA,	0,
                         595,	433,	4903,	1584,	NA,	15,
                         450,	498,	3756,	2069,	NA,	0,
                         302,	564,	4732,	1959,	NA,	0,
                         203,	576,	4373,	2167,	149,	0,
                         146,	635,	5206,	3575,	682,	0,
                         64,	640,	4758,	3569,	651,	0,
                         57,	631,	4521,	3625,	340,	0,
                         42,	649,	4642,	2864,	725,	0,
                         15,	639,	5866,	3663,	723,	0,
                         5,	653,	5208,	3030,	222,	0,
                         3,	726,	6027,	3026,	250,	0,
                         1,	490,	3979,	209,	2186,	0,
                         1,	461,	4978,	178,	1060,	0))

# Plot

col <-   c( "#6d6666",  # gris
            "#fbb03b", # amarillo
            "#29abe2", # azul claro
            "#c1272d",  # rojo
            "#8cc63f",  # verde
            "#93278f") # morado


Economica <- Plot.Series(datos = Ugyf, categoria = "ECONÓMICO", col = col,
                         libreria = "highcharter",
                         titulo = "Evolución de beneficiarios programas del <br> Área de Gestión y Fomento Socieoconómica",
                         labelY = "Total beneficiarios",
                         estilo    = list(hc.Tema = 6, hc.Slider = FALSE,
                                          hc.Credits = "Periodo: 2005-2020"))


Salvar(Economica, "Investigacion", "Economica.html")

