
# Diseno de barra de navegacion
ui <- navbarPage(
    "OlitoSleep",

        # ##### Panel 1 ########################################################### #
        tabPanel("Archivos",
            fluidRow(

                
                # El restor
                column(9, align="left", 
        
                    # Seccion para buscar el directorio
                    h4("Seleciona el directorio de trabajo"),
                    
                    div(style="display: inline-block; vertical-align: top",
                        shinyDirButton(id="awdfolder", label="Buscar...", title = "Directorio de los AWD" )),

                    div(style="display: inline-block; vertical-align: middle; width: 650px",
                        verbatimTextOutput("ledir")),
                    
                    div(style="display: inline-block; vertical-align: top",
                        actionButton("saveDir", "Defecto")),
                
                    hr(),
                    
                    
                    # Mostrar el contenido del directorio
                    p(strong("Estado de los archivos")),
                    tableOutput("dfdir")
                ),
                
                # Lateral con info del settings                
                column(3,
                    h4("Parámetros para detección"),
                    htmlOutput("showSet1"),
                    textOutput("wd")
                )
                
                
            )
            
        ),
    

    



        # ##### Panel 2 ########################################################### #
        tabPanel("EdiciÃ³n",
        
            h1("holi")
            
        
        )
               
    
)
