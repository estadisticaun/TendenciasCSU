# ____________________________________________________________________________________ #
#                            1). CONSTRUIR TABLA 1 VARIABLE                            #
# ____________________________________________________________________________________ #

Tabla <- function(datos, categoria, encabezado = "Encabezados de los Niveles de la Categoría",
                  leyenda = NULL, tituloPdf = NULL, mensajePdf = "", ajustarNiveles = TRUE,
                  colorHead = "#FFFFFF", colorear = FALSE, estilo) {
  
  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if(missingArg(datos) || missingArg(categoria)) {
    stop("¡Por favor introduzca un conjunto de datos y una categoría dentro de la columna 'Variable'!", call. = F)
  }
  categoria <- toupper(categoria)
  if (!(categoria %in% datos$Variable)) {
    stop("¡Por favor introduzca una categoría que se encuentra dentro de la columna 'Variable'!", call. = F)
  }
  if (!is.logical(ajustarNiveles)) {
    stop("¡El argumento 'ajustarNiveles' debe ser un booleano (TRUE o FALSE)!", call. = F)
  }
  if (!is.character(colorHead)) {
    stop("¡El argumento 'colorHead' debe ser un carácter que indique un color con el nombre ('red'), código hexadecimal ('#FF0000') o RGB (rgb(1, 0, 0))!", call. = F)
  }
  if (!is.logical(colorear)) {
    stop("¡El argumento 'colorear' debe ser un booleano (TRUE o FALSE)!", call. = F)
  }
  if (missingArg(tituloPdf)) { tituloPdf <- encabezado }
  if (is.null(leyenda) == FALSE) {
    leyenda <- htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;',
                                       "Tabla : ", htmltools::em(leyenda))
  }
  AjusteNiveles <- ifelse(ajustarNiveles == TRUE, "compact nowrap hover row-border", "display")
  
  # CREACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA TABLA
  DataFrame <- datos %>%
    # Convertir a columnas las observaciones dispersas en múltiples filas
    filter(Variable == categoria) %>%
    pivot_wider(names_from = Clase, values_from = Total) %>%
    select(-Variable) %>%
    # Creación de la columna Total Global (Total por Fila/Semestre)
    left_join(
      datos %>%
        filter(Variable == categoria) %>%
        group_by(YEAR, SEMESTRE) %>%
        summarise(TotalGlobal = sum(Total, na.rm = TRUE))
    ) %>%
    mutate(
      YEAR = factor(YEAR),
      SEMESTRE = factor(SEMESTRE)
    )
  Categorias <- datos %>%
    filter(Variable == categoria) %>%
    group_by(Clase) %>% distinct(Clase)
  # Custom Table Container (Nombre de los Encabezados)
  sketch = htmltools::withTags(table(
    class = "display",
    thead(
      tr(
        th(rowspan = 2, "Año"),
        th(rowspan = 2, "Periodo"),
        th(colspan = n_groups(Categorias), encabezado),
        th(rowspan = 2, "Total")
      ),
      tr( lapply(Categorias %>% pull(), th) )
    )
  ))
  
  # CREACIÓN DE LA TABLA A RETORNAR
  TablaFinal <- datatable(
    DataFrame,
    class = AjusteNiveles,
    rownames  = FALSE,
    container = sketch,
    caption = leyenda,
    filter  = list(position = "top", clear = TRUE, plain = FALSE),
    extensions = c("Buttons", "KeyTable"),
    options = list(autoWidth = TRUE,
                   columnDefs = list(list(className = "dt-center", targets = 0:(n_groups(Categorias)+2)),
                                     list(width = "65px", targets = 0)),
                   pageLength = 8,
                   order = list(list(0, "desc"), list(1, "desc")),
                   dom  = "Bfrtip",
                   keys = TRUE,
                   searchHighlight = TRUE,
                   scrollX = TRUE,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color':", paste0("'", colorHead, "'"), ", 'color': '#000000'});","}"),
                   language = list(
                     processing     = "Procesando...",
                     lengthMenu     = "Mostrar _MENU_ registros",
                     zeroRecords    = "No se encontraron resultados",
                     emptyTable     = "Ningún dato disponible en esta tabla",
                     info           = "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
                     infoEmpty      = "Mostrando registros del 0 al 0 de un total de 0 registros",
                     infoFiltered   = "(filtrado de un total de _MAX_ registros)",
                     infoPostFix    = "",
                     search         = "Buscar:",
                     url            = "",
                     infoThousands  = ",",
                     loadingRecords = "Cargando...",
                     paginate = list(
                     first    = "Primero",
                     last     = "Último",
                     `next`   = "Siguiente",
                     previous = "Anterior"
                    ),
                    aria = list(
                      sortAscending  = "Activar para ordenar la columna de manera ascendente",
                      sortDescending = "Activar para ordenar la columna de manera descendente"
                    )),
                   buttons = list(list(extend = "copy", text = "Copiar"), "csv", "excel",
                                  list(extend = "pdf", pageSize = "A4", filename = "pdf",
                                       message = mensajePdf, title = tituloPdf),
                                  list(extend = "print", text = "Imprimir", pageSize = "A4",
                                       message = mensajePdf, title = tituloPdf))
    )
  )
  
  if (colorear && missingArg(estilo)) {
    TablaFinal <- TablaFinal %>%
      formatStyle(
        "YEAR", target = "cell", fontWeight = "bold",
        backgroundColor = styleEqual( unique(DataFrame$YEAR), colorRampPalette(brewer.pal(12, "Set3"))(nlevels(DataFrame$YEAR)) )
      ) %>%
      formatStyle(
        "SEMESTRE", target = "cell", fontWeight = "bold",
        color = styleEqual( unique(DataFrame$SEMESTRE), c("#E72837", "#0A7DBF") )
      )
  } else if (!missingArg(estilo)) {
    TablaFinal <- TablaFinal %>%
      formatStyle(
        "YEAR", target = "cell", fontWeight = "bold",
        backgroundColor = styleEqual( unique(DataFrame$YEAR), estilo$PaletaYear )
      ) %>%
      formatStyle(
        "SEMESTRE", target = "cell", fontWeight = "bold",
        color = styleEqual( unique(DataFrame$SEMESTRE), estilo$PaletaSemestre )
      )
  }
  
  return(TablaFinal)
}


# ____________________________________________________________________________________ #
#                         2). CONSTRUIR TABLA EXAMEN SABER PRO                         #
# ____________________________________________________________________________________ #

Tabla.SaberPro <- function(datos, variable, encabezado = "Encabezados de los Niveles de la Categoría",
                           leyenda, tituloPdf = NULL, mensajePdf = "", ajustarNiveles = TRUE,
                           colorHead = "#FFFFFF", colorear = FALSE, estilo) {
  
  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if(missingArg(datos) || missingArg(variable)) {
    stop("¡Por favor introduzca un conjunto de datos y una variable!", call. = F)
  }
  variable <- tolower(variable)
  if (!(variable %in% datos$Variable)) {
    stop("¡Por favor introduzca alguno de los niveles que se encuentran dentro de la columna 'Variable'!", call. = F)
  }
  if (variable == "total") {
    stop("¡Seleccione un nivel diferente a 'total' de la columna 'Variable', pues este sirve como complemento para los demás niveles!", call. = F)
  }
  if (!is.logical(ajustarNiveles)) {
    stop("¡El argumento 'ajustarNiveles' debe ser un booleano (TRUE o FALSE)!", call. = F)
  }
  if (!is.character(colorHead)) {
    stop("¡El argumento 'colorHead' debe ser un carácter que indique un color con el nombre ('red'), código hexadecimal ('#FF0000') o RGB (rgb(1, 0, 0))!", call. = F)
  }
  if (!is.logical(colorear)) {
    stop("¡El argumento 'colorear' debe ser un booleano (TRUE o FALSE)!", call. = F)
  }
  if (missingArg(tituloPdf)) { tituloPdf <- encabezado }
  
  if (missingArg(leyenda)) {
    Leyenda <- htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;',
                                       "Nota: ", htmltools::em("Los valores presentados entre paréntesis hacen referencia a la desviación estándar."),
                                       htmltools::br(htmltools::em("(*) hace referencia al total de estudiantes evaluados.")))
  } else {
    Leyenda <- htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', "Nota: ", htmltools::em(leyenda))
  }
  AjusteNiveles <- ifelse(ajustarNiveles == TRUE, "compact nowrap hover row-border", "display")
  
  # CREACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA TABLA
  DataFrame <- datos %>%
    # Convertir a columnas las observaciones dispersas en múltiples filas
    filter(Variable == variable) %>%
    mutate("Valor" = paste0(Total, " (", desv, ")")) %>%
    select(-c(Variable, Total, desv)) %>%
    pivot_wider(names_from = Componente, values_from = Valor) %>%
    relocate(n, .after = last_col()) %>%
    mutate(YEAR = factor(YEAR), Clase = factor(Clase))
  Total_IESB <- datos %>%
    filter(Variable == "total") %>%
    mutate("Valor" = paste0(Total, " (", desv, ")")) %>%
    select(-c(Variable, Total, desv)) %>%
    pivot_wider(names_from = Componente, values_from = Valor) %>%
    relocate(n, .after = last_col()) %>%
    mutate(YEAR = factor(YEAR), Clase = factor(Clase))
  
  DataFrame <- bind_rows(DataFrame, Total_IESB)
  Componentes <- datos %>%
    filter(Variable == variable) %>%
    group_by(Componente) %>% distinct(Componente)
  # Custom Table Container (Nombre de los Encabezados)
  sketch = htmltools::withTags(table(
    class = "display",
    thead(
      tr(
        th(rowspan = 2, "Año"),
        th(rowspan = 2, "Categoría"),
        th(colspan = n_groups(Componentes), encabezado),
        th(rowspan = 2, "N*")
      ),
      tr( lapply(Componentes %>% pull(), th) )
    )
  ))
  
  # CREACIÓN DE LA TABLA A RETORNAR
  TablaFinal <- datatable(
    DataFrame,
    class = AjusteNiveles,
    rownames  = FALSE,
    container = sketch,
    caption = Leyenda,
    filter  = list(position = "top", clear = TRUE, plain = FALSE),
    extensions = c("Buttons", "KeyTable"),
    options = list(autoWidth = TRUE,
                   columnDefs = list(list(className = "dt-center", targets = 0:(n_groups(Componentes)+2)),
                                     list(targets = 2:(n_groups(Componentes)+1), searchable = FALSE),
                                     list(width = "65px", targets = 0)),
                   pageLength = 8,
                   order = list(list(0, "desc"), list(1, "asc")),
                   dom  = "Bfrtip",
                   keys = TRUE,
                   searchHighlight = TRUE,
                   scrollX = TRUE,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color':", paste0("'", colorHead, "'"), ", 'color': '#000000'});","}"),
                   language = list(
                     processing     = "Procesando...",
                     lengthMenu     = "Mostrar _MENU_ registros",
                     zeroRecords    = "No se encontraron resultados",
                     emptyTable     = "Ningún dato disponible en esta tabla",
                     info           = "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
                     infoEmpty      = "Mostrando registros del 0 al 0 de un total de 0 registros",
                     infoFiltered   = "(filtrado de un total de _MAX_ registros)",
                     infoPostFix    = "",
                     search         = "Buscar:",
                     url            = "",
                     infoThousands  = ",",
                     loadingRecords = "Cargando...",
                     paginate = list(
                       first    = "Primero",
                       last     = "Último",
                       `next`   = "Siguiente",
                       previous = "Anterior"
                     ),
                     aria = list(
                       sortAscending  = "Activar para ordenar la columna de manera ascendente",
                       sortDescending = "Activar para ordenar la columna de manera descendente"
                     )),
                   buttons = list(list(extend = "copy", text = "Copiar"), "csv", "excel",
                                  list(extend = "pdf", pageSize = "A4", filename = "pdf",
                                       message = mensajePdf, title = tituloPdf),
                                  list(extend = "print", text = "Imprimir", pageSize = "A4",
                                       message = mensajePdf, title = tituloPdf))
    )
  )
  
  if (colorear && missingArg(estilo)) {
    TablaFinal <- TablaFinal %>%
      formatStyle(
        "YEAR", target = "cell", fontWeight = "bold",
        backgroundColor = styleEqual( unique(DataFrame$YEAR), colorRampPalette(brewer.pal(12, "Set3"))(nlevels(DataFrame$YEAR)) )
      ) %>%
      formatStyle(
        "Clase", target = "cell", fontWeight = "bold",
        color = styleEqual( unique(DataFrame$Clase), rainbow(nlevels(DataFrame$Clase), v = 0.8) )
      )
  } else if (!missingArg(estilo)) {
    TablaFinal <- TablaFinal %>%
      formatStyle(
        "YEAR", target = "cell", fontWeight = "bold",
        backgroundColor = styleEqual( unique(DataFrame$YEAR), estilo$PaletaYear )
      ) %>%
      formatStyle(
        "Clase", target = "cell", fontWeight = "bold",
        color = styleEqual( unique(DataFrame$Clase), estilo$PaletaCategoria )
      )
  }
  
  return(TablaFinal)
}


# ____________________________________________________________________________________ #
#                            3). CONSTRUIR SERIES DE TIEMPO                            #
# ____________________________________________________________________________________ #

PocentRelativo <- function(x) {
  TotPercent <- function(m){ m*100/sum(m, na.rm = TRUE) }
  RowPorcent <- round( t(apply(x, MARGIN = 1, FUN = TotPercent)), 0 )
  return(as.data.frame(RowPorcent))
}

Plot.Series <- function(datos, categoria, colores, titulo = "", labelX = "Periodo", labelY = "",
                        libreria = c("highcharter", "plotly", "dygraphs"), estilo = NULL) {
  
  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if(missingArg(datos) || missingArg(categoria)) {
    stop("¡Por favor introduzca un conjunto de datos y una categoría dentro de la columna 'Variable'!", call. = FALSE)
  }
  categoria <- toupper(categoria)
  if (!(categoria %in% datos$Variable)) {
    stop("¡Por favor introduzca una categoría que se encuentra dentro de la columna 'Variable'!", call. = FALSE)
  }
  if (!(is.character(titulo) && is.character(labelX) && is.character(labelY))) {
    stop("¡El argumento 'titulo', 'labelX' y 'labelY' deben ser una cadena de texto!", call. = FALSE)
  }
  if (missingArg(libreria)) {
    warning("¡Se usará la librería 'highcharter' por defecto para realizar el plot!", call. = FALSE)
    libreria <- "highcharter"
  } else {
    libreria  <- tolower(libreria)
    '%NotIN%' <- Negate('%in%')
    if (libreria %NotIN% c("highcharter", "plotly", "dygraphs")) {
      stop("¡Por favor introduzca el nombre de una librería valida (paquete usado para realizar la gráfica)!", call. = FALSE)
    }
  }
  LegendTitle <- ifelse(is.null(estilo$LegendTitle), "", estilo$LegendTitle)
  
  # CREACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA GRÁFICA
  DataFrame <- datos %>%
    filter(Variable == categoria) %>%
    mutate(Fecha = paste(YEAR, SEMESTRE, sep = "-")) %>%
    select(-Variable, -YEAR, -SEMESTRE) %>%
    relocate(Fecha)
  
  TablaHorizontal <- DataFrame %>% pivot_wider(names_from = Clase, values_from = Total)
  categorias <- DataFrame %>% select(Clase) %>% distinct() %>% pull()
  
  if (length(categorias)==1L) {
    Relativo <- DataFrame %>%
      mutate(Relativo = Total/Total*100) %>%
      select(-Total)
  } else {
    Relativo <- TablaHorizontal %>% select(-Fecha) %>%
      PocentRelativo() %>% as_tibble() %>%
      mutate(Fecha = TablaHorizontal$Fecha) %>%
      pivot_longer(cols = categorias, names_to = "Clase", values_to = "Relativo")
  }
  TablaFinal <- DataFrame %>% inner_join(Relativo)
  
  if (!(missingArg(colores) || length(colores)==length(categorias))) {
    stop(paste0("¡El número de colores ingresados en el vector 'colores' no corresponde con el número de categorías a colorear!",
                "\n\tNo. colores ingresados = ", length(colores), " != ", "No. de categorías = ", length(categorias)), call. = FALSE)
  }
  if (missingArg(colores)) { colores <- rainbow(length(categorias), alpha = 0.7) }
  
  # CREACIÓN DEL PLOT RETORNAR
  if(libreria == "highcharter") {
    
    if(!(missingArg(estilo) || is.null(estilo$hc.Tema))) {
      ThemeHC <- switch(estilo$hc.Tema,
                        "1" = hc_theme_538(),
                        "2" = hc_theme_alone(),
                        "3" = hc_theme_economist(),
                        "4" = hc_theme_ffx(),
                        "5" = hc_theme_flat(),
                        "6" = hc_theme_ggplot2(),
                        "7" = hc_theme_google(),
                        "8" = hc_theme_monokai(),
                        "9" = hc_theme_darkunica(),
                        "10" = hc_theme_gridlight()
                        )
    } else { ThemeHC <- hc_theme_flat() }
    BoxInfo <- ifelse(!(missingArg(estilo) || is.null(estilo$hc.BoxInfo)), estilo$hc.BoxInfo, TRUE)
    
    PlotSeries <- TablaFinal %>%
      hchart(type = "line", hcaes(x = Fecha, y = Total, group = Clase), color = colores,
             zoomType = list(enabled = FALSE),  resetZoomButton = TRUE) %>%
      hc_chart(type = "datetime", zoomType = "x") %>%
      hc_plotOptions(line = list(marker = list(enabled = FALSE, symbol = "square", radius = 1))) %>%
      
      hc_title(text = titulo, style = list(fontWeight = "bold",
                                           fontSize   = "22px",
                                           color      = "#333333",
                                           useHTML    = TRUE)
               ) %>%
      hc_xAxis(title = list(text   = labelX,
                            offset = 70,
                            style  = list(fontWeight = "bold",
                                          fontSize   = "18px",
                                          color      = "black")
                            ),
               align = "center", lineColor = "#787878", opposite  = FALSE,
               labels = list(style = list(fontWeight = "bold", color = "black", fontSize = "18px"))
               ) %>%
      hc_yAxis(title = list(text   = labelY,
                            offset = 70,
                            style  = list(fontWeight = "bold",
                                          fontSize   = "18px",
                                          color      = "black")
                            ),
               lineColor = "#787878", opposite  = FALSE, lineWidth = 1, min = 0,
               labels    = list(style = list(fontWeight = "bold", color = "black", fontSize = "18px"))
               ) %>%
      # https://github.com/jbkunst/highcharter/issues/331
      hc_exporting(enabled = TRUE, filename = paste0("PlotSeries_", categoria)) %>%
      hc_legend(enabled = TRUE, align = "center", verticalAlign = "bottom", layout = "horizontal",
                title = list(text = LegendTitle, style = list(textDecoration = "underline")),
                x = 42, y = 0, itemStyle = list(fontWeight = "bold",
                                                color      = "black",
                                                fontSize   = "18px")) %>%
      hc_tooltip(crosshairs = TRUE, shared = BoxInfo,
                 pointFormat = '<span style="color:{series.color}">\u25CF </span><b>{series.name}: {point.y}</b> ({point.Relativo}%)<br/>',
                 backgroundColor = hex_to_rgba("#BAAEAE", 0.7),
                 borderColor = "#6D6666", borderWidth = 5, useHTML = TRUE) %>%
      hc_add_theme(ThemeHC)
    
    if (!missingArg(estilo) && estilo$hc.Slider==TRUE) {
      PlotSeries <- PlotSeries %>%
        hc_navigator(height = 15, margin = 5, maskFill = "rgba(255,16,46,0.6)",
                     enabled = TRUE, series = list(color     = "#999999",
                                                   lineWidth = 30,
                                                   type      = "areaspline",
                                                   fillColor = "#999999")
                     ) %>%
        hc_rangeSelector(enabled = TRUE, inputEnabled = FALSE, labelStyle = list(display = "none"),
                         buttonPosition = list(align = "left"), floating = FALSE,
                         buttons = list(list(type = "all", text = "Restaurar")))
    }
    if (!(missingArg(estilo) || is.null(estilo$hc.Credits))) {
      PlotSeries <- PlotSeries %>%
        hc_subtitle(text = estilo$hc.Credits, align = "left", style = list(color = "#2B908F", fontWeight = "bold"))
    }
    
  } else if (libreria == "plotly") {
    
    if (!(missingArg(estilo) || is.null(estilo$ply.LegendPosition))) {
      ParmsLegend <- estilo$ply.LegendPosition
    } else {
      ParmsLegend <- list(x = 1, y = 0.5, orientation = "v")
    }
    if (!(missingArg(estilo) || is.null(estilo$ply.Credits))) {
      ParmsCredits <- estilo$ply.Credits
    } else {
      ParmsCredits <- list(x = 0.2, y = 1, text = "")
    }
    Hovermode <- ifelse(!(missingArg(estilo) || is.null(estilo$ply.Interaction)), estilo$ply.Interaction, "x unified")
    
    FreqRelativa <- Relativo %>% pivot_wider(names_from = Clase, values_from = Relativo)
    PlotSeries <- plot_ly(data = TablaHorizontal)
    for (i in 1:length(categorias)) {
      df_Temp    <- data.frame(X = TablaHorizontal$Fecha, Y = TablaHorizontal[[categorias[i]]], Text = FreqRelativa[[categorias[i]]])
      PlotSeries <- add_trace(PlotSeries, x = ~X, y = ~Y, data = df_Temp, text = ~ Text,
                              name = categorias[i], type = "scatter", mode = "markers+lines",
                              line = list(color = colores[i], width = 3),
                              marker =  list(color = colores[i], size = 6, line = list(width = 1.2, color = "#787878")),
                              hovertemplate = paste('%{y}', '(%{text:.2s}%)'),
                              textposition = "outside")
    }
    # Arial | Open Sans | Courier New, monospace
    FamilyAxis  <- list(family = "Old Standard TT, serif", size = 16, color = "#525252")
    FamilyTitle <- list(family = "Open Sans", size = 24, color = "#333333")
    
    Title <- list(text = paste0("<b>", titulo, "</b>"), font = FamilyTitle, y = 0.96)
    Xaxis <- list(title = labelX,
                  zeroline = FALSE,
                  showline = TRUE,
                  showgrid = FALSE,
                  showticklabels = TRUE,
                  linecolor = "#787878",
                  linewidth = 2.5,
                  autotick  = FALSE,
                  ticks     = "outside",
                  tickwidth = 2.5,
                  ticklen   = 10,
                  tickcolor = "#CCCCCC",
                  tickangle = -45,
                  tickfont  = FamilyAxis)
    Yaxis <- list(title = labelY,
                  zeroline = TRUE,
                  showline = TRUE,
                  showgrid = TRUE,
                  showticklabels = TRUE,
                  linecolor = "#787878",
                  linewidth = 3,
                  separatethousands = TRUE,
                  tickfont  = FamilyAxis)
    
    PlotSeries <- PlotSeries %>%
      layout(title = Title, xaxis = Xaxis, yaxis = Yaxis,
             autosize = TRUE, showlegend = TRUE,
             legend = append(ParmsLegend, list(traceorder = "normal", title = list(text = paste0("<b>", LegendTitle, "</b>")))),
             hovermode = Hovermode,
             annotations = append(ParmsCredits, list(showarrow = FALSE, xref = "paper", yref = "paper",
                                                     xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
                                                     font = list(size = 12, color = "#CCCCCC")))
             ) %>% config(locale = "es")
    
  } else if (libreria == "dygraphs") {
    
    LegendWidth <- ifelse(!(missingArg(estilo) || is.null(estilo$dyg.LegendWidth)), estilo$dyg.LegendWidth, 250)
    
    Periodos <- TablaHorizontal %>% select(Fecha) %>% distinct() %>% pull()
    Periodos <- gsub("-2", "-7", Periodos)
    TableHorizontal <- TablaHorizontal
    TableHorizontal$Fecha <- as.Date(as.yearmon(Periodos, "%Y-%m"))
    TableHorizontal <- xts(x = TableHorizontal[,-1], order.by = TableHorizontal$Fecha)
    
    getSemestre <- 'function(d) {
                      var monthNames = ["I", "", "", "", "", "","II", "", "", "", "", ""];
                      date = new Date(d);
                      if (date.getMonth() == 0 || date.getMonth() == 6) {
                        return date.getFullYear() + "-" + monthNames[date.getMonth()];
                      } else {
                        return "";
                      }
                   }'
    dyUnzoom <- function(dygraph) {
      dyPlugin(
        dygraph = dygraph,
        name = "Unzoom",
        path = system.file("plugins/unzoom.js", package = "dygraphs")
      )
    }
    
    PlotSeries <- dygraph(TableHorizontal, main = paste0("<span style='color:", "#333333", ";'>", titulo, "</span>")) %>%
      dyOptions(drawPoints = TRUE, pointSize = 2,
                strokeWidth = 2, colors = colores, includeZero = TRUE,
                axisTickSize = 3, axisLineColor = "#787878",
                axisLabelColor = "#525252", axisLabelFontSize = 16,
                drawGrid = TRUE, gridLineColor = "lightblue") %>%
      dyLegend(show = "always", width = LegendWidth, hideOnMouseOut = TRUE) %>%
      dyAxis("x", label = labelX, axisLabelFormatter = JS(getSemestre), axisLineWidth = 4) %>%
      dyAxis("y", label = labelY, axisLineWidth = 4) %>%
      dyRangeSelector(height = 30, strokeColor = "") %>%
      dyUnzoom()
    
    if (!(missingArg(estilo) || is.null(estilo$dyg.Resaltar))) {
      if (estilo$dyg.Resaltar) {
        PlotSeries <- PlotSeries %>%
          dyHighlight(highlightCircleSize = 5,
                      highlightSeriesBackgroundAlpha = 0.5,
                      highlightSeriesOpts = list(strokeWidth = 2.5),
                      hideOnMouseOut = TRUE)
      }
    }
  }
  
  return(PlotSeries)
}


# ____________________________________________________________________________________ #
#                           4). CONSTRUIR DIAGRAMA DE TORTA                            #
# ____________________________________________________________________________________ #

Plot.Torta <- function(datos, categoria, ano, periodo, colores, titulo = "", addPeriodo = TRUE,
                       libreria = c("highcharter", "plotly"), estilo = NULL) {
  
  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if(missingArg(datos) || missingArg(categoria)) {
    stop("¡Por favor introduzca un conjunto de datos y una categoría dentro de la columna 'Variable'!", call. = FALSE)
  }
  categoria <- toupper(categoria)
  if (!(categoria %in% datos$Variable)) {
    stop("¡Por favor introduzca una categoría que se encuentra dentro de la columna 'Variable'!", call. = FALSE)
  }
  if (!is.character(titulo)) {
    stop("¡El argumento 'titulo' debe ser una cadena de texto!", call. = FALSE)
  }
  if (missingArg(ano) && missingArg(periodo)) {
    stop("¡Por favor introduzca al menos uno de los dos argumentos (o ambos), sea 'ano' o 'periodo'!", call. = FALSE)
  }
  if (missingArg(libreria)) {
    warning("¡Se usará la librería 'highcharter' por defecto para realizar el plot!", call. = FALSE)
    libreria <- "highcharter"
  } else {
    libreria  <- tolower(libreria)
    '%NotIN%' <- Negate('%in%')
    if (libreria %NotIN% c("highcharter", "plotly")) {
      stop("¡Por favor introduzca el nombre de una librería valida (paquete usado para realizar la gráfica)!", call. = FALSE)
    }
  }
  LegendTitle <- ifelse(is.null(estilo$LegendTitle), "", estilo$LegendTitle)
  Etiqueta    <- ifelse(is.null(estilo$hc.Etiqueta), "Número de graduados", estilo$hc.Etiqueta)
  
  # CREACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA GRÁFICA
  DataFrame  <- ungroup(datos) %>% filter(Variable == categoria) %>% select(-Variable)
  categorias <- DataFrame %>% select(Clase) %>% distinct() %>% pull()
  
  if (missingArg(ano)) {
    TablaFinal <- DataFrame %>% select(-YEAR) %>% filter(SEMESTRE == periodo) %>% group_by(Clase) %>% summarise(Total = sum(Total))
  } else if (missingArg(periodo)) {
    TablaFinal <- DataFrame %>% select(-SEMESTRE) %>% filter(YEAR == ano) %>% group_by(Clase) %>% summarise(Total = sum(Total))
  } else {
    titulo     <- ifelse(!missingArg(titulo) && addPeriodo, paste0(titulo, " (PERIODO ", ano, "-", periodo, ")"), titulo)
    TablaFinal <- DataFrame %>% filter(YEAR == ano, SEMESTRE == periodo)
  }
  if (!(missingArg(colores) || length(colores)==length(categorias))) {
    stop(paste0("¡El número de colores ingresados en el vector 'colores' no corresponde con el número de categorías a colorear!",
                "\n\tNo. colores ingresados = ", length(colores), " != ", "No. de categorías = ", length(categorias)), call. = FALSE)
  }
  if (missingArg(colores)) { colores <- rainbow(length(categorias), alpha = 0.7) }
  
  # CREACIÓN DEL PLOT RETORNAR
  if(libreria == "highcharter") {
    
    if(!(missingArg(estilo) || is.null(estilo$hc.Tema))) {
      ThemeHC <- switch(estilo$hc.Tema,
                        "1" = hc_theme_538(),
                        "2" = hc_theme_alone(),
                        "3" = hc_theme_economist(),
                        "4" = hc_theme_ffx(),
                        "5" = hc_theme_flat(),
                        "6" = hc_theme_ggplot2(),
                        "7" = hc_theme_google(),
                        "8" = hc_theme_monokai(),
                        "9" = hc_theme_darkunica(),
                        "10" = hc_theme_gridlight()
      )
    } else { ThemeHC <- hc_theme_flat() }
    
    PlotTorta <- TablaFinal %>%
      hchart(type = "pie", hcaes(x = Clase, y = Total), name = Etiqueta, showInLegend = TRUE) %>%
      hc_title(text = titulo, style = list(fontWeight = "bold",
                                           fontSize   = "22px",
                                           color      = "#333333",
                                           useHTML    = TRUE)
               ) %>%
      hc_plotOptions(pie = list(allowPointSelect = TRUE,
                                colorByPoint = TRUE,
                                colors = colores,
                                dataLabels = list(enabled = TRUE,
                                                  format = "<b>{point.name}</b>: {point.percentage:.1f} %",
                                                  style = list(fontWeight = "bold",
                                                               color      = "black",
                                                               fontSize   = "18px")
                                                  )
                                )
                     ) %>%
      hc_exporting(enabled = TRUE, filename = paste0("PlotTorta_", categoria)) %>%
      hc_legend(enabled = TRUE, align = "center", verticalAlign = "bottom",
                title = list(text = LegendTitle, style = list(textDecoration = "underline")),
                itemStyle = list(fontWeight = "bold",
                                 color      = "black",
                                 fontSize   = "18px")) %>%
      hc_add_theme(ThemeHC)
    
    if (!(missingArg(estilo) || is.null(estilo$hc.Credits))) {
      PlotTorta <- PlotTorta %>%
        hc_subtitle(text = estilo$hc.Credits, align = "left", style = list(color = "#2B908F", fontWeight = "bold"))
    }
    
  } else if (libreria == "plotly") {
    
    if (!(missingArg(estilo) || is.null(estilo$ply.Credits))) {
      ParmsCredits <- estilo$ply.Credits
    } else {
      ParmsCredits <- list(x = 0.2, y = 1, text = "")
    }
    
    FamilyTitle <- list(family = "Open Sans", size = 24, color = "#333333")
    Title  <- list(text = paste0("<b>", titulo, "</b>"), font = FamilyTitle, y = 0.95)
    Margen <- list(l = 50, r = 50, t = 110, b = 0)                                      # l = left; r = right; t = top; b = bottom
    
    if (!missingArg(estilo) && estilo$ply.Leyenda=="inside") {
      PlotTorta <- plot_ly(TablaFinal, labels = ~Clase, values = ~Total, type = "pie",
                           textposition = "inside", textinfo = "label+value+percent",   # "label+percent"
                           insidetextfont = list(color = "#FFFFFF", size = 20), hoverinfo = "label+value",
                           insidetextorientation = "radial",                            # "horizontal", "radial", "tangential", "auto"
                           marker = list(colors = colores, line = list(color = "#000000", width = 1.5))) %>%
        layout(title = Title, showlegend = FALSE,  autosize = TRUE, margin = Margen)
    } else {
      PlotTorta <- plot_ly(TablaFinal, labels = ~Clase, values = ~Total, type = "pie",
                           textinfo = "label+percent",
                           marker = list(colors = colores, line = list(color = "#FFFFFF", width = 1.5))) %>%
        layout(title = Title, showlegend = TRUE,  autosize = TRUE, margin = Margen)
    }
    PlotTorta <- PlotTorta %>%
      layout(annotations = append(ParmsCredits, list(showarrow = FALSE, xref = "paper", yref = "paper",
                                                     xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
                                                     font = list(size = 12, color = "#2B908F")))
             ) %>% config(locale = "es")
  }
  
  return(PlotTorta)
}


# ____________________________________________________________________________________ #
#                           5). CONSTRUIR DIAGRAMA DE BARRAS                           #
# ____________________________________________________________________________________ #
Plot.Barras <- function(datos, categoria, ano, periodo, vertical = TRUE, ordinal = FALSE, colores, libreria = c("highcharter", "plotly"),
                        titulo = "", labelEje = "Número de Graduados", addPeriodo = TRUE, textInfo = labelEje, estilo = NULL) {
  
  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if(missingArg(datos) || missingArg(categoria)) {
    stop("¡Por favor introduzca un conjunto de datos y una categoría dentro de la columna 'Variable'!", call. = FALSE)
  }
  categoria <- toupper(categoria)
  if (!(categoria %in% datos$Variable)) {
    stop("¡Por favor introduzca una categoría que se encuentra dentro de la columna 'Variable'!", call. = FALSE)
  }
  if (!(is.character(titulo) && is.character(labelEje) && is.character(textInfo))) {
    stop("¡El argumento 'titulo', 'labelEje' y 'textInfo' deben ser una cadena de texto!", call. = FALSE)
  }
  if (missingArg(libreria)) {
    warning("¡Se usará la librería 'highcharter' por defecto para realizar el plot!", call. = FALSE)
    libreria <- "highcharter"
  } else {
    libreria  <- tolower(libreria)
    '%NotIN%' <- Negate('%in%')
    if (libreria %NotIN% c("highcharter", "plotly")) {
      stop("¡Por favor introduzca el nombre de una librería valida (paquete usado para realizar la gráfica)!", call. = FALSE)
    }
  }
  
  # CREACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA GRÁFICA
  DataFrame  <- ungroup(datos) %>% filter(Variable == categoria) %>% select(-Variable) %>% filter(is.na(Clase)!=TRUE)
  categorias <- DataFrame %>% select(Clase) %>% distinct() %>% pull()
  
  if (missingArg(ano)) { TablaFinal <- DataFrame %>% select(-YEAR) %>% filter(SEMESTRE == periodo) }
  else if (missingArg(periodo)) { TablaFinal <- DataFrame %>% select(-SEMESTRE) %>% filter(YEAR == ano) }
  else {
    titulo     <- ifelse(!missingArg(titulo) && addPeriodo, paste0(titulo, " (PERIODO ", ano, "-", periodo, ")"), titulo)
    TablaFinal <- DataFrame %>% filter(YEAR == ano, SEMESTRE == periodo)
  }
  
  if (!(missingArg(colores) || length(colores)==length(categorias))) {
    stop(paste0("¡El número de colores ingresados en el vector 'colores' no corresponde con el número de categorías a colorear!",
                "\n\tNo. colores ingresados = ", length(colores), " != ", "No. de categorías = ", length(categorias)), call. = FALSE)
  }
  if (missingArg(colores)) { colores <- rainbow(length(categorias), alpha = 0.7) }
  
  if (!ordinal) {
    TablaFinal <- bind_cols(TablaFinal, "Colores" = colores)
    TablaFinal <- TablaFinal %>% arrange(desc(Total))
    MyColors   <- TablaFinal$Colores
  } else { MyColors <- colores }
  
  # CREACIÓN DEL PLOT RETORNAR
  if(libreria == "highcharter") {
    
    if(!(missingArg(estilo) || is.null(estilo$hc.Tema))) {
      ThemeHC <- switch(estilo$hc.Tema,
                        "1" = hc_theme_538(),
                        "2" = hc_theme_alone(),
                        "3" = hc_theme_economist(),
                        "4" = hc_theme_ffx(),
                        "5" = hc_theme_flat(),
                        "6" = hc_theme_ggplot2(),
                        "7" = hc_theme_google(),
                        "8" = hc_theme_monokai(),
                        "9" = hc_theme_darkunica(),
                        "10" = hc_theme_gridlight()
      )
    } else { ThemeHC <- hc_theme_elementary() }
    
    Orientacion <- ifelse(vertical, "column", "bar")
    PlotOptions <- list(colorByPoint = TRUE, colors = MyColors, dataLabels = list(enabled = TRUE, style = list(fontWeight = "bold",
                                                                                                              color      = "black",
                                                                                                              fontSize   = "18px"))
                        )
    
    PlotBarras <- highchart() %>%
      hc_add_series(TablaFinal, type = Orientacion, hcaes(x = paste(Clase, "-", round(Total*100/sum(Total),1), "%"), y = Total),
                    name = textInfo, showInLegend = FALSE) %>%
      hc_title(text = titulo, style = list(fontWeight = "bold",
                                           fontSize   = "22px",
                                           color      = "#333333",
                                           useHTML    = TRUE)
               ) %>%
      hc_plotOptions(bar = PlotOptions, column = PlotOptions) %>%
      hc_xAxis(categories = TablaFinal$Clase,
               labels = list(style = list(fontWeight = "bold", color = "black", fontSize = "18px"))
               ) %>%
      hc_yAxis(title  = list(text = labelEje, style = list(fontWeight = "bold", color = "black", fontSize = "18px")),
               labels = list(style = list(fontWeight = "bold", color = "black", fontSize = "18px"))
               ) %>%
      
      hc_exporting(enabled = TRUE, filename = paste0("PlotBarras_", categoria)) %>%
      hc_add_theme(ThemeHC)
    
    if (!(missingArg(estilo) || is.null(estilo$hc.Credits))) {
      PlotBarras <- PlotBarras %>%
        hc_subtitle(text = estilo$hc.Credits, align = "left", style = list(color = "#2B908F", fontWeight = "bold"))
    }
    
  } else if (libreria == "plotly") {
    
    # PocentRelativo <- function(x) { return(as.vector(round(x*100/sum(x, na.rm = TRUE), 2))) }
    # FreqRelativo   <- PocentRelativo(TablaFinal$Total)
    if (!(missingArg(estilo) || is.null(estilo$ply.Credits))) {
      ParmsCredits <- estilo$ply.Credits
    } else {
      ParmsCredits <- list(x = 0.11, y = 1.1, text = "")
    }
    ShowLeyenda <- ifelse(!(missingArg(estilo) || is.null(estilo$ply.Legend)), estilo$ply.Legend, TRUE)
    
    FamilyTitle <- list(family = "Open Sans", size = 24, color = "#333333")
    Title  <- list(text = paste0("<b>", titulo, "</b>"), font = FamilyTitle, y = 0.95)
    if (titulo == "") { Margen <- NULL } else { Margen <- list(l = 50, r = 50, t = 110, b = 0) }
    
    if (vertical) {
      if (ordinal) { EjeX <- "Clase"; EjeY <- "Total" } else { EjeX <- "reorder(Clase, Total)"; EjeY <- "Total" }
      PlotBarras <- plot_ly(TablaFinal, x = ~eval(parse(text = EjeX)), y = ~eval(parse(text = EjeY)),
                            type = "bar", color = ~Clase, orientation = "v",
                            hovertemplate = ~paste0(Total, " (", scales::percent(Total/sum(Total)), ")"),
                            marker = list(color = colores, line = list(color = "#3A4750", width = 1.5))) %>%
        layout(title = Title, xaxis = list(title = ""), yaxis = list(title = labelEje),
               showlegend = ShowLeyenda, autosize = TRUE, margin = Margen)
    } else {
      if (ordinal) { EjeX <- "Total"; EjeY <- "Clase" } else { EjeX <- "Total"; EjeY <- "reorder(Clase, Total)" }
      PlotBarras <- plot_ly(TablaFinal, x = ~eval(parse(text = EjeX)), y = ~eval(parse(text = EjeY)),
                            type = "bar", color = ~Clase, orientation = "h",
                            hovertemplate = ~paste0(Total, " (", scales::percent(Total/sum(Total)), ")"),
                            marker = list(color = colores, line = list(color = "#3A4750", width = 1.5))) %>%
        layout(title = Title, xaxis = list(title = labelEje), yaxis = list(title = ""),
               showlegend = ShowLeyenda, autosize = TRUE, margin = Margen)
    }
    
    PlotBarras <- PlotBarras %>%
      layout(annotations = append(ParmsCredits, list(showarrow = FALSE, xref = "paper", yref = "paper",
                                                     xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
                                                     font = list(size = 12, color = "#2B908F")))
             ) %>% config(locale = "es")
  }
  
  return(PlotBarras)
}


caja <- function(datos, titulo, eje){
  datos_2 <- datos %>% filter(Serie != "2008-1")
  gfa <- hcboxplot(x = datos_2$PTOTAL, var = datos_2$Serie, outliers = FALSE) %>%
    hc_title(style = list( fontWeight = "bold",fontSize = "25px" ), text = titulo) %>%
    hc_plotOptions( boxplot = list(
      colorByPoint = F, color="#00a703" ))%>%
    hc_exporting(enabled = TRUE, filename = "export") %>% 
    hc_yAxis( min = 0, max = 1000,
              title = list(style = list( fontWeight = "bold",
                                         color = "black",
                                         fontSize = "18px"), text = eje),
              labels = list(
                style = list(
                  fontWeight = "bold",
                  color = 'black',
                  fontSize = '18px'
                ))
    ) %>% 
    hc_xAxis(
      title = list(style = list( fontWeight = "bold",
                                 color = "black",
                                 fontSize = "18px"), text = "Periodo"),
      labels = list(
        style = list(
          fontWeight = "bold",
          color = 'black',
          fontSize = '18px'
        ))
    ) %>% 
    hc_add_theme(hc_theme_elementary()) %>%
    hc_chart(type = "column")
  return(gfa)
}

caja_n <- function(datos, titulo, eje, colores ){
  n <- datos %>% select(Agrupacion) %>% distinct() %>% nrow()
  colo <- colores[1:n]
  datos_2 <- datos %>% filter(Serie != "2008-1")
  gfa <- hcboxplot(x = datos_2$PTOTAL, var = datos_2$Serie, var2 = datos_2$Agrupacion, outliers = FALSE, color=colo) %>%
    hc_title(style = list( fontWeight = "bold",fontSize = "25px" ), text = titulo) %>%
    hc_exporting(enabled = TRUE, filename = "export") %>% 
    hc_yAxis(lineColor = '#787878', lineWidth = 1, min = 0, max = 1000,
             title = list(style = list( fontWeight = "bold",
                                        color = "black",
                                        fontSize = "18px"), text = eje),
             labels = list(
               style = list(
                 fontWeight = "bold",
                 color = 'black',
                 fontSize = '18px'
               ))
    ) %>% 
    hc_xAxis(lineColor = '#787878', lineWidth = 1,
             title = list(style = list( fontWeight = "bold",
                                        color = "black",
                                        fontSize = "18px"), text = "Periodo"),
             labels = list(
               style = list(
                 fontWeight = "bold",
                 color = 'black',
                 fontSize = '18px'
               ))
    )  %>%
    hc_add_theme(hc_theme_elementary()) %>%
    hc_legend(enabled = TRUE, align = "center",layout = "horizontal",
              x = 42, y = 0,
              itemStyle = list(
                fontWeight = "bold",
                color = 'black',
                fontSize = '18px'
              )) %>%
    hc_chart(type = "column") 
  return(gfa)
}
