# OlitoSleep UI - La mejor app del lab  -----
ui <- navbarPage(
    "OlitoSleep", id = "TablasApp",

    # | -----------------------------------------------------------------------
    # | ------ Panel 1 - ARCHIVOS --------------------------------- -----------
    tabPanel("Archivos",
        # | Seccion: Seleccion y visualizacion archivos -----------------------
        fluidRow(
            column(9, align="left", 
                
                # | --- Seleccionar archivo -----------------------------------
                fluidRow(
                    column(12,
                        h4("Seleciona el directorio de trabajo"),
                        fluidRow(
                            column(8,
                                # Muestra ruta
                                verbatimTextOutput("pathText", placeholder = TRUE),
                            ),
                            column(4,
                                # Carga ruta
                                actionButton("pathBoton", "Buscar...", icon = icon("folder"), width = "110px")
                            )
                        )
                    )
                ),
                hr(),
                
                # | --- Contenido del directorio ------------------------------
                fluidRow(
                    
                    # | ------ Status de los archivos -------------------------
                    column(7, align = "left", offset = 1,
                        h4("Estado de los archivos"),
                        tableOutput("dfdir")
                    ),
                    
                    # | ------ Filtrado de archivos ---------------------------
                    column(4, align = "left",
                           # Seleccion de filtro
                           h4("Filtrar según Status"),
                           radioButtons("filterDir", label = NULL, choices = c("No procesado", 
                                        "En edicion", "Terminado", "Con error", "Todos"),
                                        selected = "Todos"),
                           br(),
                           # Tabla de recuentos
                           h4("Recuentos"),
                           tableOutput("tableDir")
                    )
                )
            ),
            
            # | Seccion: Lateral con info del settings --------------------
            column(3,
                h4("Parámetros para detección"),
                htmlOutput("showSet"),
                hr(),
                h4("Procesado masivo"),
                p("Para ejecutar el análisis inicial de todos los ficheros AWD sin procesar."),
                actionButton("massProc", label = "Procesar", icon = icon("warning"))
            )
        )
    ),



    # | ----
    # | ------ Panel 2 - ACTOGRAMA ----------------------------- --------------
    tabPanel("Actograma",
        fluidRow(
            # | Seleccion de Sujetos + Ejecutar -------------------------------
            column(3,
                fluidRow(
                    column(12,
                        h4("Seleccionar sujeto y edición")
                    )
                ),
                # Botones de edición
                fluidRow(
                    column(4,
                        actionButton("edFin.btn", "Finalizar", width = "90", icon = icon("save"))
                    ),
                    column(4,
                        actionButton("edEdit.btn", "Editar", width = "90", icon = icon("edit"))
                    ),
                    column(4,
                        # actionButton("edActo.btn", "Actograma", width = "90")
                    )
                ),
                hr(),
                # Seleccion de sujetos
                uiOutput("subjInput")
            ),
            
            # | Actograma -----------------------------------------------------
            column(9,
                fluidRow(
                    div(style = "margin-top:-50px; position:relative; z-index:1; ",
                    uiOutput("actoUI"))
                )
            )
        )
    ),
    
    
    
    # | ----
    # | ------ Panel 3 - EDICION DE ARCHIVOS -------------- -------------------
    tabPanel("Edición",
        fluidRow(
            # | Sección: sujetos y períodos -----------------------------------
            column(2,
                h4("Sujeto y períodos"),
                verbatimTextOutput("SubjEdicion", placeholder = TRUE),
                hr(),
                uiOutput("perSelection")
            ),
            
            # | Sección Grafico y Edición -------------------------------------
            column(10, 
                # | -- Plot y Slider ----
                fluidRow(align = "center",
                    plotOutput("periodPlot", height = 150, width = "100%"),
                    uiOutput("sliderEdicion")
                ),
                
                # | -- Header filtro y controles grafico ----
                fluidRow(
                    # Header filtro
                    column(4,
                        h4("Detalle del filtro"),
                        verbatimTextOutput("filtroH"),
                    ),
                    # Inicio y fin de registor
                    column(4,
                        h4("Inicio y fin del registro"),
                        verbatimTextOutput("filtroIniFin")
                    ),
                    # Ancho de linea
                    column(1, offset = 1,
                        h4("Volver"),
                        actionButton("volverActo", label = "Actograma", width = "95px")
                    ),
                    column(1, # offset = 2,
                        h4("Linea"),
                        numericInput("ldNum", value = 1, min = 1, max = 10, step = 1, 
                                     label = NULL, width = "80px"),
                    ),
                    # Resetear el gráfico
                    column(1,
                        h4("Reset"),
                        actionButton("resetBtn", label = "Reset", icon = NULL, width = "80px")
                    )
                    
                ),
                
                # | Sección: Panel de ediciones -------------------------------
                fluidRow(
                    # | -- Tabla de filtros -----
                    column(4,
                        tableOutput("filtroDF")
                    ),
                    
                    # | Pestañas de edición
                    column(8,
                        tabsetPanel(
                            # | -- 1. Inicio y fin ----
                            tabPanel("Limites Registro",
                                fluidRow(
                                    column(12, style = "padding-left: 25px; padding-top: 15px;",
                                        h4("Determinar inicio y fin del registro"),
                                        p("Seleccionar desde la lista desplegable, los valores corresponden al inicio o fin de estados de sueño o vigilia"), 
                                        
                                        hr(),
                                        fluidRow(
                                            # Determinar inicio
                                            column(3,
                                                strong("Inicio del registro"),
                                                uiOutput("inifin.iniUI"),
                                                actionButton("inifin.iniset", label = "Set Inicio", 
                                                             icon = icon("angle-double-right"), width = "120px")
                                            ),
                                            # Determinar fin
                                            column(3,
                                                strong("Término del registro"),
                                                uiOutput("inifin.finUI"),
                                                actionButton("inifin.finset", label = "Set Término", 
                                                             icon = icon("angle-double-left"), width = "120px")
                                            ),
                                            column(6)
                                        ),
                                        hr(),
                                        
                                        p("El inicio y término del registro es útil para descartar extremos que no se utilizan, 
                                          sobre todo determinar el primer período al momento de calcular las estadísticas.")
                                    )
                                )
                            ),
                            
                            
                            # | -- 2. Edición de períodos -------------------------
                            tabPanel("Filtrar Períodos",
                                fluidRow(
                                    column(12, style = "padding-left: 25px; padding-top: 15px;",
                                        h4("Seleccionar dia, noche o ambos"),
                                        p("El período se excluirá de los análisis y estadísticas"),
                                        
                                        hr(),
                                        p("El período contiene las siguientes fechas:"),
                                        splitLayout(cellWidths = c("28%", "2%", "20%", "50%"),
                                            # Fecha
                                            verbatimTextOutput("selectedPer1", placeholder = TRUE),
                                            p(" "),
                                            # Selector
                                            checkboxGroupInput("dianoc", label= NULL, choices = c("Noche", "Dia"), inline = TRUE),
                                            # Boton
                                            actionButton("cambia_periodo", label = "Filtrar", icon = icon("filter"))
                                        ),
                                        hr(),
                                        
                                        p("La noche se encuentra a la izquierda del gráfico y el día a la derecha. 
                                          La sección marcada no será considada en la creación de las estadísticas")
                                    )
                                )
                            ),
                            
                            # | -- 3. Edicion de actividad --------------------------
                            tabPanel("Editar actividad",
                                fluidRow(
                                    column(12, style = "padding-left: 25px; padding-top: 15px;",
                                        h4("Seleccionar estado de sueño a modificar"),
                                        p("Se agregará actividad simulada al estado de sueño seleccionada para transformar en vigilia"),
                                        
                                        hr(),
                                        fluidRow(
                                            column(4,
                                                # Le nuevo radiobutton  <"editAct.data">
                                                uiOutput("editActUI"),
                                            ),
                                            column(2,
                                                # Boton de edicion
                                                strong("Duración episodio"),
                                                verbatimTextOutput("editActDur", placeholder = TRUE),
                                                actionButton("editAct.btn", label = "Editar", icon = icon("filter"), width = "120px")
                                            ),
                                            column(6),
                                        ),
                                        hr(),
                                        
                                        # Pie de página
                                        p("Esta edición ", strong("transforma Sueño en Vigilia"), " y sólo se debe usar cuando 
                                           el sujeto se retira el actígrafo por un corto período de tiempo")
                                    )
                                )
                            ),
                            
                            # | -- 4. Mover la noche -------------------------------------
                            tabPanel("Editar límite",
                                fluidRow(
                                    column(12, style = "padding-left: 25px; padding-top: 15px;",
                                        h4("Indicar el inicio o fin de un período"),
                                        # Elegir escenario
                                        fluidRow(
                                            column(12,
                                                radioButtons("moveNightEscena", label = NULL, inline = TRUE,
                                                             choices = c("Inicio Vigilia", "Fin Vigilia", "Inicio Sueño", "Fin Sueño"))
                                            )    
                                        ),
                                        hr(),
                                        fluidRow(
                                            # Listado de inicios de sueño
                                            column(3,
                                                uiOutput("moveNightUI")
                                            ),
                                            
                                            # Botón
                                            column(3,
                                                strong("Duración episodio"),
                                                verbatimTextOutput("moveNight.Dur", placeholder = TRUE),
                                                actionButton("moveNight.btn", "Mover Noche", icon = icon("moon"))
                                            ),
                                            
                                            column(6)
                                        ),
                                        hr(),
                                        p(strong("Se usa en 4 escenarios para rescatar datos:")),
                                        p(code("AM-1"), "Se puso el actigrafo al despertar", code("[Inicio Vigilia]"), br(), 
                                          code("AM-2"), "Se sacó el actígrafo al despertar", code("[Fin Sueño]")),
                                        p(code("SetN"), "Mover la noche, existe un", code("[S]"), "largo inicio noche", code("[Inicio Sueño]")),
                                        p(code("PM-1"), "Se sacó el actígrafo antes de dormir", code("[Fin Vigilia]"), br(),
                                          code("PM-2"), "Se puso el actigrafo antes de dormir", code("[Inicio Sueño]"))
                                    )
                                )
                            ),
                                
                            # | -- 5. Borrar filtro --------------------------------------
                            tabPanel("Borrar filtro",
                                fluidRow(
                                    column(12, style = "padding-left: 25px; padding-top: 15px;",
                                        h4("Marcar el filtro a borrar"),
                                        p("El registro será eliminado del archivo de filtros.", br(), code("id | ini | fin | tipo")),
                                            
                                        # Controles e inputs num-reactivenum-boton
                                        hr(),
                                        fluidRow(
                                            column(5,
                                                # Filtro seleccionado
                                                uiOutput("dropChoose")
                                            ),
                                            column(7,
                                                # Boton y seleccion
                                                # numericInput("borraFiltroNum", label = "Id del Filtro", value=1, min=1, step=1, width = "100px"),
                                                actionButton("borraFiltroBtn", label = "Borrar", icon = icon("trash"), width = "100px")
                                            ),
                                            # column(4,
                                            # )
                                        ),
                                        hr()
                                    )
                                )
                            ),
                            
                            # | -- 6. Estados de sueño  ---------------------------------
                            tabPanel("Estados",
                                fluidRow(
                                    column(12, style = "padding-left: 25px; padding-top: 15px;",
                                        # div(style = "overflow-y: scroll; height: 350px",
                                            h4("Estados de sueño y vigilia del período"),
                                            tableOutput("estadosTabla")
                                        # )
                                    )
                                )
                            )
                        )
                    )
                ) 
            )
        )
    ),
    
    
    # | ----
    # | ------ Panel 4 - EPISODIOS -------------------------------- ------
    tabPanel("Episodios",
        # | -- Fila de filtros ----
        fluidRow(
            column(1),
            column(3,
                checkboxGroupInput("estadoFilter", choices = c("W", "S"), 
                                   inline = TRUE, label = "Filtrar por Estado")
            ),
            column(3,
                checkboxGroupInput("dianocFilter", choices = c("Dia", "Noche"),
                                   inline = TRUE, label = "Filtrar por Dia|Noche")
            ),
            column(3,
                uiOutput("periodFilter")
            ),
            column(3)
        ),
             
        # | -- Tabla epi -----
        fluidRow(
            column(12,
                splitLayout(cellWidths = c("5%", "90%", "5%"),
                           p(" "), tableOutput("epi"), p("")
                )
            )
        )
    ),

    
    # Panel 5 - ESTADISTICAS --------------------------------------------------
    tabPanel("Informes",
        verbatimTextOutput("test1"),
        verbatimTextOutput("test2")
    )
    
)