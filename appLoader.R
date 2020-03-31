# Cargador de todas las cosas
# | -- Library Loader ---------------------------------------------------------
packlist <- c("kableExtra", "Hmisc", "openxlsx", "fs", "shinyFiles","shiny",
              "rmarkdown","haven", "data.table", "stringr", "purrr")
new.packages <- packlist[!(packlist %in% installed.packages()[,"Package"])]

if (length(new.packages) > 0) {
    install.packages(new.packages) 
}
for (lib in packlist) {
    eval(parse(text = paste0("library(",lib,")")))
}
# Estos al final
library("lubridate")
library("dplyr")

rm(packlist, new.packages, lib)


# | -- Functions --------------------------------------------------------------
func.folders <- dir()[dir.exists(dir())]
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
print("<<< Cargando los settings >>>")
set <- getset()




