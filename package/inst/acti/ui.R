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
                        h3("Seleciona el directorio de trabajo"),
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
                    column(6, align = "left", offset = 1,
                        h4("Estado de los archivos en el directorio de trabajo"),
                        tableOutput("dfdir")
                    ),
                    
                    # | ------ Filtrado de archivos ---------------------------
                    column(4, align = "left", offset = 1,
                           # Seleccion de filtro
                           h4("Filtrar según Status"),
                           radioButtons("filterDir", label = NULL, 
                                        choices = c("Todos", "No procesado", "En edicion", "Terminado"),
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
                h3("Parámetros para la detección"),
                p("Valores y parámetros disponibles para la detección con el algoritmo de MiniMitter y la posterior asignación de los períodos de dia y noche."),
                hr(),
                htmlOutput("showSet"),
                hr(),
                p("Para cambiar los parámetros se debe modificar el archivo", strong("settings.sleep"), "que se creó en el directorio de los archivos AWD."),
                strong("Si no se reflejan los cambios recargar la app."),
                hr(),
                h3("Procesado masivo"),
                p("Para ejecutar el análisis inicial de todos los ficheros AWD sin procesar."),
                actionButton("massProc", label = "Procesar", icon = icon("warning")),
                hr()
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
                    # div(style = "margin-top:-50px; position:relative; z-index:1; ",
                    div(style = "margin-top:-70px;",
                        uiOutput("actoUI")
                    )
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

                
                # | Sección: Panel de ediciones -------------------------------
                fluidRow(
                    # | -- Tabla de filtros -----
                    column(4,
                        # Controles del grafico
                        fluidRow(
                           column(4, style = "text-align: center;",
                                  strong("Volver a..."),
                                  actionButton("volverActo", label = "Actograma", width = "100%")    # 95px
                           ),
                           column(4, style = "text-align: center;",
                                  strong("Ancho de línea"),
                                  numericInput("ldNum", value = 1, min = 1, max = 10, step = 1, 
                                               label = NULL, width = "100%"),    # 80px
                           ),
                           column(4, style = "text-align: center;",
                                  strong("Reset gráfico"),
                                  actionButton("resetBtn", label = "Reset", icon = NULL, width = "100%")   # 80px
                           )   
                        ),
                        
                        # Limites del registro y Tabla de filtros
                        fluidRow(
                            column(12, 
                                strong("Inicio y fin del registro"),
                                verbatimTextOutput("filtroIniFin"),
                                strong("Tabla de filtros"),
                                tableOutput("filtroDF")
                            )
                        )
                    ),
                    
                    # | Pestañas de edición
                    column(8,
                        tabsetPanel(
                            # | -- 1. Inicio y fin ----
                            tabPanel("Limites Registro",
                                fluidRow(
                                    column(12, style = "padding-left: 25px; padding-top: 15px;",
                                        h4("Determinar inicio y fin del registro"),
                                        p("Seleccionar desde la lista desplegable fecha y hora que marcan los límites del regstro."), 
                                        
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
                                        
                                        p("Se puede sobre escribir y quedará marcado con una linea color morado, lo que esté antes y despues de los limites no se analizará")
                                    )
                                )
                            ),
                            
                            # | -- 2. Edicion de actividad --------------------------
                            tabPanel("Editar actividad",
                                fluidRow(
                                    column(12, style = "padding-left: 25px; padding-top: 15px;",
                                        h4("Seleccionar estado de sueño a modificar"),
                                        p("Se agregará actividad simulada al estado de sueño seleccionado y se transforma en vigilia"),
                                        
                                        hr(),
                                        fluidRow(
                                            column(4,
                                                # Le nuevo radiobutton  <"editAct.data">
                                                uiOutput("editActUI"),
                                            ),
                                            column(3,
                                                # Boton de edicion
                                                strong("Duración del episodio"),
                                                verbatimTextOutput("editActDur", placeholder = TRUE),
                                                actionButton("editAct.btn", label = "Editar", icon = icon("filter"), width = "120px")
                                            ),
                                            column(5),
                                        ),
                                        hr(),
                                        
                                        # Pie de página
                                        p("Esta edición ", strong("transforma Sueño en Vigilia"), " y sólo se debe usar cuando 
                                           el sujeto se retira el actígrafo por un corto período de tiempo.")
                                    )
                                )
                            ),
                            
                            # | -- 3. Mover la noche -------------------------------------
                            tabPanel("Mover Periodo",
                                fluidRow(
                                    column(12, style = "padding-left: 25px; padding-top: 15px;",
                                        h4("Seleccionar episodio que inicia el período"),
                                        p("Para mover la noche o el día según lo que indique el análisis o bitácora."),
                                        # Elegir escenario
                                        fluidRow(
                                            column(12,
                                                radioButtons("moveNightEscena", label = NULL, inline = TRUE,
                                                             choices = c("Mover Noche", "Mover Dia"), selected = "Mover Noche")
                                            )    
                                        ),
                                        # hr(),
                                        fluidRow(
                                            # Listado de inicios de sueño
                                            column(3,
                                                strong("Episodios disponibles"),
                                                uiOutput("moveNightUI")
                                            ),
                                            # Boton
                                            column(3,
                                                strong("Duración episodio"),
                                                verbatimTextOutput("moveNight.Dur", placeholder = TRUE),
                                                actionButton("moveNight.btn", "Mover Periodo", icon = icon("directions"))
                                            ),
                                            column(6)
                                        ),
                                        hr(),
                                        p("Este filtro sólo se debe utilizar cuando hay dos períodos consecutivos válidos y entre ellos
                                          se mueve la noche o día.")
                                    )
                                )
                            ),
                            
                            
                            # | -- 4. Excluir -----------------------------------------
                            tabPanel("Excluir sección",
                                fluidRow(
                                    column(12, style = "padding-left: 25px; padding-top: 15px;",
                                        # Crear el filtro
                                        h4("Filtro para excluir sección"),
                                        p("Los limites de exclusión se determinan por el fin del episodio. 
                                           Los cambios se verán reflejados cuando inicio y fin sean valores válidos."),
                                        hr(),
                                        
                                        fluidRow(
                                            # Fines de episodios
                                            column(3,
                                                # strong("Selección último episodio"),
                                                uiOutput("ExcludeUI")
                                            ),
                                                
                                            column(9,
                                                # Informacion de seleccion
                                                fluidRow(
                                                    column(4,
                                                        strong("Episodio seleccionado"),
                                                        verbatimTextOutput("Exclude.stInfo", placeholder = TRUE)
                                                    ),
                                                    column(4,
                                                        strong("Duración"),
                                                        verbatimTextOutput("Exclude.durInfo", placeholder = TRUE)
                                                    )
                                                ),
                                                hr(),
                                                # Mostrar y confirmar
                                                fluidRow(
                                                    column(2,
                                                        strong("Filtros"),
                                                        uiOutput("ExcludeFiltros")
                                                    ),
                                                    column(4,
                                                        strong("Determinar inicio"),
                                                        verbatimTextOutput("Exclude.IniText", placeholder = TRUE),
                                                        actionButton("Exclude.IniSet", "Set Inicio")
                                                    ),
                                                    column(4,
                                                        strong("Determinar Fin"),
                                                        verbatimTextOutput("Exclude.FinText", placeholder = TRUE),
                                                        actionButton("Exclude.FinSet", "Set Final")
                                                    )
                                                ),
                                                hr(),
                                                fluidRow(
                                                    strong("Importante: Respetar las reglas del archivo de configuracion."),
                                                    p("El período que sigue a una exclusión debe comenzar con un episodio (adecuado) mayor o igual la duración mínima")
                                                )
                                            )
                                        ),
                                    )
                                )     
                            ),
                            
                                
                            # | -- 5. Borrar filtro --------------------------------------
                            tabPanel("Borrar filtro",
                                fluidRow(
                                    column(12, style = "padding-left: 25px; padding-top: 15px;",
                                        h4("Marcar el filtro a borrar"),
                                        p("El registro será eliminado del archivo de filtros. Seleccionar, confirmar y borrar"),
                                        hr(),
                                        fluidRow(
                                            # Seleccionar filtro
                                            column(3,
                                                strong("Filtros disponibles"),
                                                uiOutput("dropChoose")
                                            ),
                                            # Confirmar y borrar
                                            column(6,
                                                strong("Filtro seleccionado"),
                                                tableOutput("todrop.info"),
                                                actionButton("borraFiltroBtn", label = "Borrar", icon = icon("trash"), width = "100px")
                                            ),
                                        ),
                                        hr()
                                    )
                                )
                            ),
                            
                            # | -- 6. Tabla de estados  -----------------------
                            tabPanel("Estados",
                                fluidRow(
                                    column(12, style = "padding-left: 25px; padding-top: 15px;",
                                        fluidRow(
                                            column(8,
                                                # Header del filtro
                                                h4("Filtro y tabla de estados"),
                                                verbatimTextOutput("filtroH"),
                                                # Tabla de estados
                                                tableOutput("estadosTabla")
                                            ),
                                            # Segmentacion
                                            column(4,
                                                h4("Segmentación"),
                                                radioButtons("tablaStage", label = "Estado", choices = c("Todo", "W", "S"), selected = "Todo"),
                                                radioButtons("tablaLength", label = "Duración", choices = c("Todo", "Mayor a duración mínima"))
                                            )
                                        )
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
    # | ------ Panel 4 - ESTADISTICAS -------------- -------------------
    tabPanel("Informes",
        verbatimTextOutput("test1"),
        verbatimTextOutput("test2")
    )
    
    


    
    
)
