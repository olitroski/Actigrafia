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
                # Seccion para buscar el directorio
                h4("Seleciona el directorio de trabajo"),
                
                div(style="display: inline-block; vertical-align: top",
                    actionButton("saveDir", "Guardar")),
                
                div(style="display: inline-block; vertical-align: middle; width: 650px",
                    verbatimTextOutput("wdFolderTxt")),
                
                div(style="display: inline-block; vertical-align: top",
                    shinyDirButton(id="BrowsePath", label="Buscar...", title = "Directorio de los AWD" )),
                
                # div(style="display: inline-block; vertical-align: top",
                #     actionButton("btn_cargar", "Cargar")),
                
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
                h4("Seleccionar sujeto a editar"),
                radioButtons("accion_choice", label = NULL, 
                         choices = c("Editar", "Actograma", "Finalizar"),
                         selected = "Editar", inline = TRUE),
                actionButton("accion_button", "Ejecutar"),
                hr(),
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
                    plotOutput("periodPlot", height = 140, width = "100%"),
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
                    column(2,
                        h4("Ancho de linea"),
                        numericInput("ldNum", value = 1, min = 1, max = 10, step = 1, 
                                     label = NULL, width = "80px"),
                    ),
                    # Resetear el gráfico
                    column(2,
                        h4("Reset gráfico"),
                        actionButton("resetBtn", label = "Reset", icon = NULL)
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
                            tabPanel("Ini | Fin",
                                fluidRow(
                                    column(12, style = "padding-left: 25px; padding-top: 15px;",
                                        h4("Determinar inicio y fin del registro"),
                                        p("Procurar que sea un inicio de estado de sueño o vigilia, ver:", code("Estados")), 
                                        
                                        # Periodo seleccionado
                                        fluidRow(
                                            column(5,
                                                strong("Seleccionar fecha"), br(),
                                                uiOutput("inifin.dateUI")
                                            ),
                                            column(5, offset = 2,
                                                strong("Ejemplo"), br(),
                                                code("10:48")
                                            )
                                        ),
                                        fluidRow(
                                            # Determinar inicio
                                            column(3,
                                                strong("Hora de Inicio del registro"),
                                                uiOutput("inifin.iniUI"),
                                                actionButton("inifin.iniset", label = "Set Inicio", 
                                                             icon = icon("angle-double-right"), width = "120px")
                                            ),
                                            # Determinar fin
                                            column(9,
                                                strong("Hora de Término del registro"),
                                                uiOutput("inifin.finUI"),
                                                actionButton("inifin.finset", label = "Set Término", 
                                                             icon = icon("angle-double-left"), width = "120px")
                                            ),
                                        ),
                                        hr(),
                                        p("El inicio y término del registro es útil para descartar extremos que no se utilizan, 
                                          sobre todo determinar el primer período al momento de calcular las estadísticas.")
                                    )
                                )
                            ),
                            
                            
                            # | -- 2. Edición de períodos -------------------------
                            tabPanel("Edición de periodos",
                                fluidRow(
                                    column(12, style = "padding-left: 25px; padding-top: 15px;",
                                        h4("Seleccionar dia, noche o ambos"),
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
                            tabPanel("Edición de actividad",
                                fluidRow(
                                    column(12, style = "padding-left: 25px; padding-top: 15px;",
                                        h4("Escribir hora de inicio y fin para la corrección"),
                                        p("El intervalo no debe cruzar las ", code("00:00:")),
                                        
                                        # La fila de cosas
                                        splitLayout(cellWidths = c("28%", "2%", "10%", "10%", "2%", "48%"),
                                            # Fecha
                                            verbatimTextOutput("editAct.date"),
                                            p(" "),
                                            # Inputs
                                            textInput("editAct.ini", label = NULL),
                                            textInput("editAct.fin", label = NULL),
                                            p(" "),
                                            # Botón
                                            actionButton("editAct.btn", label = "Editar", icon = icon("filter"))
                                        ),
                                        splitLayout(cellWidths = c("28%", "2%", "10%", "10%", "2%", "48%"),
                                                         style = "text-align: center; margin-top: -15px;",
                                            # Indicaciones de formato
                                            p(" "), p(" "), strong("Inicio"), strong("Final"), p(" "), p(" ")
                                        ),
                                        
                                        # Pie de página
                                        br(),
                                        shiny::HTML("Las horas deben tener 4 digitos, por ejemplo: <code>08:57</code> o <code>23:17</code>"),
                                        hr(),
                                        p("Esta edición ", strong("transforma Sueño en Vigilia"), " y sólo se debe usar cuando 
                                           el sujeto se retira el actígrafo por un corto período de tiempo")
                                    )
                                )
                            ),
                            
                            # | -- 4. Mover la noche -------------------------------------
                            tabPanel("Mover la noche",
                                fluidRow(
                                    column(12, style = "padding-left: 25px; padding-top: 15px;",
                                        h4("Indicar inicio de la noche"),
                                        p("Indicar una hora aproximada en la que comienza la noche"),
                                        
                                        # La fila de cosas
                                        splitLayout(cellWidths = c("28%", "2%", "10%", "2%", "58%"),
                                            # Fecha
                                            verbatimTextOutput("moveNight.date", placeholder = TRUE),
                                            p(" "),
                                            # Hora <edicion>
                                            textInput("moveNight.hora", label = NULL, value = "hh:mm"),
                                            p(" "),
                                            # Boton
                                            actionButton("moveNight.btn", "Mover Noche", icon = icon("moon"))
                                        )
                                    )
                                )
                            ),
                                
                            # | -- 5. Borrar filtro --------------------------------------
                            tabPanel("Borrar filtro",
                                fluidRow(
                                    column(12, style = "padding-left: 25px; padding-top: 15px;",
                                        h4("Indicar el id del filtro a borrar"),
                                        p("El registro será eliminado del archivo de filtros."),
                                            
                                        # Controles e inputs num-reactivenum-boton
                                        fluidRow(
                                            column(12,
                                                tableOutput("borraFiltroTxt")
                                            )
                                        ),
                                        fluidRow(
                                            column(2,
                                                numericInput("borraFiltroNum", label=NULL, value=1, min=1, step=1)
                                            ),
                                            column(10,
                                                actionButton("borraFiltroBtn", label = "Borrar", icon = icon("trash"), width = "100px")
                                            )
                                        )
                                    )
                                )
                            ),
                            
                            # | -- 6. Estados de sueño  ---------------------------------
                            tabPanel("Estados",
                                fluidRow(
                                    column(12, style = "padding-left: 25px; padding-top: 15px;",
                                        div(style = "overflow-y: scroll; height: 350px",
                                            h4("Estados de sueño y vigilia del período"),
                                            tableOutput("estadosTabla")
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
