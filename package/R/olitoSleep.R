#' @title Lanza la aplicacion olitosleep
#' @description Esta funcion carga la aplicacion de shiny.
#' @export
#' @examples
#' # olitosleep()
# ----------------------------------------------------------------------------- #
# ----- Funcion necesaria para cargar desde el formato package ---------------- #
# ----------------------------------------------------------------------------- #
olitosleep <- function(){
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