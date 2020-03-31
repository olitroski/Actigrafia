# OlitoSleep UI - La mejor app del lab  ññññ.
ui <- navbarPage(
    "OlitoSleep", id = "TablasApp",

    # Panel 1 - ARCHIVOS ----------------------------------------------- ------
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




    # Panel 2 - ACTOGRAMA ------------------------------------------- ---------
    tabPanel("Actograma",
        fluidRow(
            column(12,
                h4("Seleccionar sujeto a editar")    
            )
        ),     
             
        fluidRow(
            # | Seccion: Seleccion de Sujetos ---------------------------------
            # column(3,
            #     h4("Seleccionar sujeto"),
            #     textOutput("statsSubj"),
            #     br(),
            #     uiOutput("subjInput")
            # ),
            
            column(3,
                radioButtons("accion_choice", label = NULL, 
                         choices = c("Editar", "Actograma", "Finalizar"),
                         selected = "Actograma", inline = TRUE),
                actionButton("accion_button", "Ejecutar"),
                hr(),
                uiOutput("subjInput")
            ),
            
            # | --- Acciones y actograma --------------------------------------
            column(9,
                # fluidRow(
                #     div(style = "position:relative; z-index:2;", h4("Acciones a tomar")  )
                # ),
                # # Acciones a tomar
                # fluidRow(
                #     splitLayout(cellWidths = c("10%", "40%", "50%"),
                #         # Boton de acción
                #         div(style = "position:absolute; z-index:2;", 
                #             actionButton("accion_button", "Proceder")),
                #         # Seleccionar acction
                #         div(style = "position:absolute; z-index:2;",                        
                #             radioButtons("accion_choice", label = NULL, 
                #                          choices = c("Analizar", "Editar", "Actograma", "Terminar Sujeto"),
                #                          selected = "Editar", inline = TRUE), p(" "))
                #     )
                #     
                # ),
                
                # | --- Sección: Actograma ------------------------------------------------
                fluidRow(
                    div(style = "margin-top:-100px; position:relative; z-index:1; ",
                    uiOutput("actoUI"))
                )
            )
        )
    ),
    
    
    # Panel 3 - EDICION DE ARCHIVOS ----------------------------- -----------
    tabPanel("Edición",

        # | Fila para sujeto + periodo + linea + reset  -----------------------
        fluidRow(
            
            # | -- Col3: Sujeto -----------------------------------------------
            column(3, 
                h4("Sujeto en edición"),
                verbatimTextOutput("SubjEdicion", )       
            ),
            
            # | -- Col4: selectInput (outputUI) + Boton -----------------------
            column(4, 
                fluidRow(
                    column(12,
                        h4("Seleccionar el período a editar")
                    )
                ),
                
                # Fila para el input y el botón
                fluidRow(
                    column(8,  
                        # selectInput
                        uiOutput("perSelection")
                    ),
                    
                    column(3, style = "text-align: center;",
                        # Y un botón
                        actionButton("cargaSemip", label = "Cargar")
                    ),
                    column(1, p(" "))
                )
            ),
            
            # | -- Col5: Tamaño Linea + Reset ---------------------------------
            column(5, 
                # Selector de lw
                column(4, 
                   h4("Ancho Linea"),
                   numericInput("ldNum", value = 1, min = 1, max = 10, step = 1, 
                                label = NULL, width = "80px")
                ),
                
                # Botón de reset
                column(8, 
                   h4("Resetear el gráfico"),
                   actionButton("resetBtn", label = "Reset", icon = NULL)
                )
            )
        ),

        
        # | Fila para el plot y slider ----------------------------------------
        fluidRow(
            column(12, align = "center",
                # El grafico primero
                plotOutput("periodPlot", height = 140, width = "90%"),
                
                # Input: Slider for the number of bins
                uiOutput("sliderEdicion")
            )
        ),

        
        # | Fila para el panel edicion ----------------------------------------
        fluidRow(
            # | -- Archivo de filtro ------------------------------------------
            # << Hay que crear una funcion que haga solo eso >>
            column(4,
                verbatimTextOutput("filtroH"),
                tableOutput("filtroDF")
                # verbatimTextOutput("filtroDF")
            ),
            
            # | -- Pestaña de edición -----------------------------------------
            column(8,
                tabsetPanel(
                    # | ---- 1. Edición de períodos ---------------------------
                    tabPanel("Edición de periodos",
                        fluidRow(
                            column(12, style = "padding-left: 25px; padding-top: 15px;",
                                h4("Seleccionar dia, noche o ambos"),
                                p("Se está editando la siguiente fecha:"),
                                splitLayout(cellWidths = c("28%", "2%", "20%", "50%"),
                                    # Fecha
                                    verbatimTextOutput("selectedPer1", placeholder = TRUE),
                                    p(" "),
                                    # Selector
                                    checkboxGroupInput("dianoc", label= NULL, 
                                                    choices = c("Dia", "Noche"), 
                                                    inline = TRUE),
                                    # Boton
                                    actionButton("cambia_periodo", label = "Filtrar", 
                                                 icon = icon("filter"))
                                )
                            )
                        ),
                        
                        fluidRow(
                            column(2, style = "padding-left: 25px; padding-top: 10px;" ,
                                strong("Periodo a filtrar")
                            ),
                            column(6, style = "padding-top: 10px;",
                                verbatimTextOutput("toFilter1", placeholder = TRUE)
                            )
                        )
                    ),
                    
                    # | ---- 2. Edicion de actividad --------------------------
                    tabPanel("Edición de actividad",
                        fluidRow(
                            column(12, style = "padding-left: 25px; padding-top: 15px;",
                                h4("Escribir hora de inicio y fin para la corrección"),
                                p("El intervalo no debe cruzar las 00:00:"),
                                
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
                                    actionButton("editAct.btn", label = "Editar", 
                                                 icon = icon("filter"))
                                ),
                                
                                splitLayout(cellWidths = c("28%", "2%", "10%", "10%", "2%", "48%"),
                                    style = "text-align: center; margin-top: -15px;",
                                    p(" "), p(" "), strong("Inicio"), strong("Final"), p(" "), p(" ")),
                                
                                # Pie de página
                                br(),
                                shiny::HTML("Las horas deben tener 4 digitos, por ejemplo: 
                                            <code>08:57</code> o <code>23:17</code>")
                            )
                        )
                    ),
                    
                    # | ---- 3. Mover la noche -------------------------------------
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
                    
                    # | ---- 4. Borrar filtro --------------------------------------
                    tabPanel("Borrar filtro",
                        fluidRow(
                            column(12, style = "padding-left: 25px; padding-top: 15px;",
                                h4("Indicar el id del filtro a borrar"),
                                p("El registro será eliminado del archivo de filtros."),
                                
                                # Controles e inputs num-reactivenum-boton
                                fluidRow(
                                    column(2,
                                        numericInput("borraFiltroNum", label=NULL, value=1, min=1, step=1, width="100%"),
                                        actionButton("borraFiltroBtn", label = "Borrar", icon = icon("trash"))
                                    ),
                                    column(6,
                                        tableOutput("borraFiltroTxt")
                                    ),
                                    column(4,
                                        p(" ")
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    ),
    
    
    
    # Panel 4 - EPISODIOS --------------------------------------------------
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
