# OlitoSleep UI - La mejor app del lab.
ui <- navbarPage(
    "OlitoSleep", id = "TablasApp",

    # Panel 1 - ARCHIVOS --------------------------------------------------
    tabPanel("Archivos",
        # | Seccion: Seleccion y visualizacion archivos -------------------
        fluidRow(
            column(9, align="left", 
    
                # | --- Seleccionar archivo -------------------------------
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

                # | --- Contenido del directorio --------------------------
                fluidRow(
                    
                    # | ------ Status de los archivos ---------------------
                    column(7, align = "left", offset = 1,
                        h4("Estado de los archivos"),
                        tableOutput("dfdir")
                    ),
                    
                    # | ------ Filtrado de archivos -----------------------
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




    # Panel 2 - ACTOGRAMA -----------------------------------------------------
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
                            choices = c("Analizar", "Editar", "Cargar Actograma"),
                            selected = NA),
                actionButton("accion_button", "Proceder")
            )
        ),
        

        
        # | Sección: Actograma ------------------------------------------------
        fluidRow(
            column(1),
            column(10,
                plotOutput("actograma", width = "100%")
            ),
            column(1)
            
        )
    ),
    
    
    # Panel 3 - EDICION DE ARCHIVOS -------------------------------------------
    tabPanel("Edición",
        # | Fila para el selector de periodo ----------------------------------
        fluidRow(
            # Mostrar el sujeto
            column(4, 
                h4("Sujeto en edición"),
                verbatimTextOutput("SubjEdicion")       
            ),
            
            # Mostar un select input (deberá ser outputUI)
            column(8,
                fluidRow(
                    column(12,
                        h4("Seleccionar el período a editar")
                    )
                ),
                
                # Fila para el input y el botón
                fluidRow(
                    column(4,
                        # selectInput("periodEdicion", label = NA, choices = c("A", "B", "C"))
                        uiOutput("perSelection")
                    ),
                    
                    column(8, align = "left",
                        # Y un botón
                        actionButton("cargaSemip", label = "Cargar")
                    )
                )
            )
        ),
        
        
        # | Fila para el plot y slider ----------------------------------------
        fluidRow(
            column(12, align = "center",
            
                # El plot
                plotOutput("periodPlot", height = 300, width = "90%"),
                
                # El Slider
                sliderInput("sliderEdicion", label = NA,
                            min = 0, max = 100, value = c(0,100), width = "90%")
                
            )
        ),
        
        
        # | Fila para el panel edicion ----------------------------------------
        fluidRow(
            # | -- Archivo de edición -----------------------------------------
            # << Hay que crear una funcion que haga solo eso >>
            column(4,
                h4("Archivo de edición"),
                verbatimTextOutput("editFile")
            ),
            
            column(8,
                # La fecha del grafico
                fluidRow(
                    column(12,
                        h4("El período en edición es:"),
                        verbatimTextOutput("periodoenedicion")
                    )
                ),
                
                # Los segmentos de edición
                fluidRow(
                    # | -- Edición de períodos ----------------------------------------
                    column(4,
                        h4("Edición de periodos"),
                        p("Seleccionar dia, noche o ambos"),
                        checkboxGroupInput("dianoc", label=NA, 
                                          choices = c("Día", "Noche"), 
                                          inline = TRUE),
                        actionButton("cambia_periodo", label = "Filtrar", icon = icon("refresh"))
                    ),
                    
                    
                    # | -- Edicion de actividad ---------------------------------------
                    column(8,
                        h4("Edición de actividad"),
                        p("Seleccionar hora de inicio, hora de fin y el tipo de edición"),
                        
                        fluidRow(
                            column(3,
                                # p("fin"),
                                textInput("ini", value = "hh:mm", label = "Inicio"),
                                actionButton("cambia_actividad", label = "Editar", icon = icon("refresh"))
                            ),
                            
                            column(3,
                                # p("fin"),
                                textInput("fin", value = "hh:mm", label = "Fin")
                            ),
                            
                            column(6,
                                selectInput("editActiv", label = "Seleccionar tipo de edición", 
                                           choices = c("Seleccionar", "Sueño a Vigilia", "Vigilia a sueño"))
                                
                            )
                            
                        )

                    )

                )

            )
        )
    ),
    
    
    
    # Panel 4 - ESTADISTICAS --------------------------------------------------
    tabPanel("Estadísticas"
        
        
    )
)