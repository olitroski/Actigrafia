#' @title Lanza la aplicacion olitosleep
#' @description Esta funcion carga la aplicacion de shiny.
#' @param inter Si no tiene internet pasar a FALSO para utilizar, de lo contrario se realizaran chequeos.
#' @export
#' @examples
#' # olitosleep()
#' @importFrom utils install.packages
#' @importFrom utils installed.packages
#' @importFrom utils packageVersion
# ----------------------------------------------------------------------------- #
# ----- Funcion necesaria para cargar desde el formato package ---------------- #
# ----------------------------------------------------------------------------- #
olitosleep <- function(inter = TRUE){
    installed.packages <- install.packages <- NULL

    # Librerias
    packlist <- c("utf8", "sourcetools","tidyselect","fastmap","xtable", "httpuv","zip",
                  "backports","assertthat","tibble","pkgconfig","R6", "kableExtra", "Hmisc",
                  "openxlsx", "fs", "shinyFiles","shiny","rmarkdown","haven","stringr",
                  "purrr","lubridate","dplyr")
    new.packages <- packlist[!(packlist %in% installed.packages()[,"Package"])]

    # Instalar si no estan
    if (length(new.packages) > 0) {
        install.packages(new.packages)
    }
    # Cargar
    for (lib in packlist) {
        eval(parse(text = paste0("library(",lib,")")))
    }


    # Acá se dirige al path
    appDir <- system.file("acti", package = "olitosleep")

    # En esta sección se puede agregar algo para controlar la version
    if (appDir == "") {
        stop("Could not find myapp. Try re-installing `mypackage`.", call. = FALSE)

    # Y se corre la app
    } else {
        
        # Primero internet
        if (inter == TRUE){
            # Version actual
            ver_actual <- packageVersion("olitosleep")
            ver_actual <- as.character(ver_actual[[1]])

            # Version del github
            cat("\nConectando a GitHub..... \n")
            link <- url("https://raw.githubusercontent.com/olitroski/Actigrafia/master/package/DESCRIPTION")
            git_version <- readLines(link)
            close(link)
            git_version <- git_version[3]
            git_version <- strsplit(git_version, ": ")
            git_version <- git_version[[1]][2]
            
            # Si hay nueva version
            if (ver_actual < git_version){
                stop("Existe una nueva version de olitoSleep. Reinstale la libreria como se indica en GitHub:")
                cat("\n https://github.com/olitroski/Actigrafia#reinstalar-la-aplicaci%C3%B3n \n")
                
            # Si la version es igual
            } else if (ver_actual == git_version){
                cat("\n La version de olitoSleep esta actualizada")
                shiny::runApp(appDir, display.mode = "normal")
            
            # Si es mayor a la de internet    
            } else {
                stop("Existe un error en la version, re-instale la aplicacion.")
            }

        # Cargar sin internet
        } else {
            cat("Cargando la app sin internet, ojo que la version puede estar obsoleta")
            shiny::runApp(appDir, display.mode = "normal")
        }
    }
}
