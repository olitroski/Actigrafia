# ----------------------------------------------------------------------------- #
# ----- Script para cargar todo lo necesario para ejecutar la Aplicación ------ #
# ----- para el entorno de pruebas porque en Pakcage way no es necesario ------ #
# ----------------------------------------------------------------------------- #
# Cargador de todas las cosas

# | -- Library Loader ---------------------------------------------------------
# Listar lo instalado
packlist <- c("utf8", "sourcetools","tidyselect","fastmap","xtable", "httpuv","zip","backports","assertthat", "tibble", "pkgconfig", 
              "R6", "kableExtra", "Hmisc", "openxlsx", "fs", "shinyFiles","shiny",
              "rmarkdown","haven", "stringr", "purrr", "lubridate", "dplyr")
new.packages <- packlist[!(packlist %in% installed.packages()[,"Package"])]

# Instalar si no estan
if (length(new.packages) > 0) {
    install.packages(new.packages) 
}

# Cargar
for (lib in packlist) {
    eval(parse(text = paste0("library(",lib,")")))
}

# Estos al final
library("lubridate")
library("dplyr")

rm(packlist, new.packages, lib)


# | -- Cargar funciones al Gloval Env -----------------------------------------
func.folders <- dir()[dir.exists(dir())]
func.folders <- func.folders[grep("func_", func.folders)]

for (f in func.folders) {
    print(paste("---Loading folder---", f))
    setwd(file.path(mainfolder, f))
    
    funciones <- dir()[grep("[.Rr]$", dir())]
    for (fun in funciones) {
        print(paste("Loading function", fun))
        source(fun)
    }    
}
rm(f, fun, func.folders, funciones)


# | -- Cargar algunas cosas ---------------------------------------------------
setwd(mainfolder)

