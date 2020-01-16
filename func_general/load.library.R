# Cargador de librer?as
# Instala librer?as si no est?n.
load.library <- function(packlist = c("kableExtra", "Hmisc", "openxlsx", "fs", "shinyFiles","shiny",
									  "rmarkdown","haven", "stringr", "purrr", "lubridate", "dplyr")){
    
    new.packages <- packlist[!(packlist %in% installed.packages()[,"Package"])]
    if(length(new.packages)>0){
        install.packages(new.packages)
    }
    
    for (lib in packlist){
        eval(parse(text=paste("library(",lib,")",sep="")))
    }
    
    # un awdfile nulo para que cargue al env y se pueda trabajar desde el server
    awdfile <<- NULL
}
