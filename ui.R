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
                
                div(style="display: inline-block; vertical-align: top",
                    actionButton("btn_cargar", "Cargar")),

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




    # Panel 2 - EDICION DE ARCHIVOS ---------------------------------------
    tabPanel("Actograma",
        
        # test #################################################### 
        fluidRow(
             column(12,
                    verbatimTextOutput("test")
             )
         ),
        
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
                            selected = "Editar"),
                actionButton("accion_button", "Proceder")
            )
        ),
        

        
        # | Sección: Actograma --------------------------------------------
        fluidRow(
            column(1),
            column(10,

                plotOutput("actograma", height = "1400px")
            ),
            column(1)
            
        )
    )
)
