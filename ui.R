# OlitoSleep UI - La mejor app del lab.
ui <- navbarPage(
    "OlitoSleep",

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
                               h4("Filtrar según Status"),
                               radioButtons("filterDir", label = NULL, choices = c("No procesado", 
                                            "En edición", "Terminado", "Con error", "Todos"),
                                            selected = "Todos"),
                               br(),
                               h4("Recuentos"),
                               tableOutput("tableDir")
                        )
                    
                    )
                ),
                
                # | Seccion: Lateral con info del settings --------------------
                column(3,
                    h4("Parámetros para detección"),
                    htmlOutput("showSet1")
                    
                    # Seleccionar archivo de parametros
                )
                
                
            )
            
        ),
    

    

        # Panel 2 - EDICION DE ARCHIVOS ---------------------------------------
        tabPanel("Actograma",
            # | Seccion: Seleccion de Sujetos ---------------------------------
            fluidRow(
                
                # | --- Pegar sujeto ------------------------------------------
                column(3,
                    h4("Pegar o seleccionar sujeto"),
                    textInput("awdfile1", label = NULL, value = "Pegar"),
                    uiOutput("subjInput")
                ),
                
                # | --- Seleccionar el sujeto ---------------------------------
                column(3,
                    h4("Selección"),
                    textOutput("selectedSubj"),
                    verbatimTextOutput("statsSubj")
                    
                    
                    
                ),
                
                column(6, 
                    h4("Botones")
                )
                
            ),
            
            hr(),
            
            # | Sección: Actograma --------------------------------------------
            fluidRow(
                column(12,
                    h4("Actograma"),
                    plotOutput("actograma")
                )
                
            )
        )
)
