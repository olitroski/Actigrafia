#' @title Lanza la aplicacion olitosleep
#' @description Esta funcion carga la aplicacion de shiny.
#' @export
#' @examples
#' # olitosleep()
#' @importFrom utils install.packages
#' @importFrom utils installed.packages
# ----------------------------------------------------------------------------- #
# ----- Funcion necesaria para cargar desde el formato package ---------------- #
# ----------------------------------------------------------------------------- #
olitosleep <- function(){
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

    # En esta sección se puede agregar algo para controlar la versión
    if (appDir == "") {
        stop("Could not find myapp. Try re-installing `mypackage`.", call. = FALSE)

    # Y se corre la app
    } else {
        shiny::runApp(appDir, display.mode = "normal")
    }
}
