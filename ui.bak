# OlitoSleep UI - La mejor app del lab.
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
                htmlOutput("showSet")
            )
        )
    ),




    # Panel 2 - ACTOGRAMA -------------------------------------------- --------
    tabPanel("Actograma",
        
        # | Seccion: Seleccion de Sujetos -------------------------------------
        fluidRow(
            # | --- Pegar o seleccionar sujeto --------------------------------
            column(3,
                h4("Pegar o seleccionar sujeto"),
                textInput("awd_paste", label = NULL, value = "Pegar"),
                uiOutput("subjInput")
            ),
            
            # | --- Muestra seleccion y status  -------------------------------
            column(3,
                h4("Selección"),
                textOutput("selectedSubj"),
                br(),
                textOutput("statsSubj")
            ),
            
            # | --- Acciones a tomar para seguir ------------------------------
            column(6, 
                h4("Acciones"),
                radioButtons("accion_choice", label = NULL, 
                            choices = c("Analizar", "Editar", "Actograma", "Terminar Sujeto"),
                            selected = "Editar", inline = TRUE),
                actionButton("accion_button", "Proceder")
            )
        ),
        

        
        # | Sección: Actograma ------------------------------------------------
        fluidRow(
            column(1),
            column(10,
                uiOutput("actoUI")
            ),
            column(1)
            
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
                                    checkboxGroupInput("dianoc", label=NA, 
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
                                p("Se está editando la siguiente fecha:"),
                                
                                # La fila de cosas
                                splitLayout(cellWidths = c("28%", "2%", "10%", "10%", "2%", "48%"),
                                    # Fecha
                                    verbatimTextOutput("selectedPer2"),
                                    p(" "),
                                    # Inputs
                                    textInput("ini", value = "hh:mm", label = NULL),
                                    textInput("fin", value = "hh:mm", label = NULL),
                                    p(" "),
                                    # Botón
                                    actionButton("cambia_actividad", label = "Editar", 
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
                        "contents",
                        verbatimTextOutput("selectedPer3")
                        

                        
                    ),
                    
                    # | ---- 4. Borrar filtro --------------------------------------
                    tabPanel("Borrar filtro",
                        "holi"
                    )
                )
            )
        )
    ),
    
    
    
    # Panel 4 - ESTADISTICAS --------------------------------------------------
    tabPanel("Estadísticas"
        
        
    )
    
    # Pa los mensajes
    #useShinyalert()
)