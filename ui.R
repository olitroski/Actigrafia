# OlitoSleep UI - La mejor app del lab.
ui <- navbarPage(
    "OlitoSleep",

        # Panel 1 - ARCHIVOS --------------------------------------------------
        tabPanel("Archivos",
            fluidRow(
                # | Seccion: Seleccion y visualizacion archivos ---------------
                column(9, align="left", 
        
                    # | --- Seleccionar archivo -------------------------------
                    # Seccion para buscar el directorio
                    h4("Seleciona el directorio de trabajo"),
                    
                    div(style="display: inline-block; vertical-align: top",
                        actionButton("saveDir", "Defecto")),

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
                                            "En edición", "Terminado", "Con error")),
                               br(),
                               h4("Recuentos"),
                               tableOutput("tableDir")
                        )
                    
                    )
                ),
                
                # | Seccion: Lateral con info del settings -----------------------------
                column(3,
                    h4("Parámetros para detección"),
                    htmlOutput("showSet1")
                    
                    # Seleccionar archivo de parametros
                )
                
                
            )
            
        ),
    

    

        # Panel 2 - EDICION DE ARCHIVOS ---------------------------------------
        tabPanel("Edición",
        
            h1("holi")
            
        
        )
               
    
)
