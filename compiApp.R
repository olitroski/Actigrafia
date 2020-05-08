# Ejecutar los pasos para actualizar la app
# Los pasos están documentados en el OneNote.
# El sha1
rm(list=ls())
library(digest)
library(dplyr)
library(stringr)

maindir <- "D:/OneDrive/INTA/Actigrafia"
setwd(maindir)

# Preguntar para confirmar
confirmar <- readline("Va a modificar la librería 'olitoSleep', continuar? [y|n]: ")
if (confirmar %in% c("Y", "y", "yes", "YES", "Yes", "si", "s", "S", "Si", "SI", "si")){
    # ---- Respaldar librería -------------------------------------------------
    # Sacar version para el file name
    setwd(file.path(maindir, "package"))
    descfile <- readLines("DESCRIPTION")
    version <- descfile[grep("Version:", descfile)]
    version <- str_split(version, ": ", simplify = TRUE)
    version <- version[1, 2]
    
    zipname <- paste0("olitoSleep_", version, ".zip")
    backpath <- file.path(maindir, "archivos")
    zippath <- file.path(backpath, zipname)
    
    # Rutas y zipear
    packpath <- file.path(maindir, "package")
    packfiles <- list.files(packpath, recursive = TRUE)
    zip(zippath, packfiles)
    
    # Limpiar
    rm(backpath, descfile, packfiles, packpath, version, zippath, zipname)
    
    
    # ---- Cambiar versión ----------------------------------------------------
    options(warn = -1)
    # Capturar el file y la version
    setwd(file.path(maindir, "package"))
    descfile <- readLines("DESCRIPTION")
    version <- descfile[grep("Version:", descfile)]
    version <- str_split(version, ": ", simplify = TRUE)
    version <- version[1, 2]
    
    # Capturar y validar nueva version
    new.version <- readline(paste("Versión actual", version, "indicar valor nuevo: "))
    check.ver <- str_split(new.version, "\\.", simplify = TRUE)
    check.ver <- as.numeric(check.ver)

    if (sum(is.na(check.ver)) > 0){
        stop(paste("Error, version no numérica", new.version))
    } else {
        conf.ver <- readline(paste("Confirmar versión", new.version, "[y|n]: "))
        if (conf.ver != "y"){
            stop("...Cancelando actualización...")
        }
    }
    options(warn = 0)
    rm(check.ver, conf.ver, confirmar)
    
    # Modificar el description file
    descfile[3] <- paste("Version:", new.version)
    writeLines(descfile, "DESCRIPTION")
    rm(version, new.version, descfile)
    
    
    # ---- Guardar paths 'Nuevos' ---------------------------------------------
    # Bien estático, solo lso folders de funciones
    
    # R files nuevos
    setwd(maindir)
    rpath <- dir.exists(dir())
    rpath <- dir()[rpath]
    rpath <- rpath[grep("func_", rpath)]
    
    rfiles <- NULL
    for (path in rpath){
        listado <- list.files(file.path(maindir, path))
        listado <- file.path(maindir, path, listado)
        rfiles <- c(rfiles, listado)
    }
    rfiles <- rfiles[grep("[.rR]$", rfiles)]
    rm(listado, path, rpath)
    
    # Shiny files nuevos
    appfiles <- c(file.path(maindir, "ui.R"), 
                  file.path(maindir, "server.R"),
                  file.path(maindir, "olitoSleep.R"))
    
    # En uno solo
    newfiles <- c(rfiles, appfiles)
    rm(rfiles, appfiles)
    
    # Data frame con nuevos datos
    newfiles <- data.frame(file = basename(newfiles), 
                           path = newfiles, 
                           # hash = sapply(newfiles, sha1), 
                           fecha = file.mtime(newfiles), stringsAsFactors = FALSE)
    row.names(newfiles) <- NULL
    
    
    # ---- Guardar paths 'Viejos' ---------------------------------------------
    # Los R
    setwd(file.path(maindir, "package", "R"))
    rfiles <- file.path(maindir, "package", "R", dir())
    
    # Los Shiny
    setwd(file.path(maindir, "package", "inst", "leapp"))
    appfiles <- file.path(maindir, "package", "inst", "leapp", dir())
    
    # En uno solo
    oldfiles <- c(rfiles, appfiles)
    rm(rfiles, appfiles)
    
    # En un df
    oldfiles <- data.frame(file = basename(oldfiles),
                           path = oldfiles,
                           # hash = sapply(oldfiles, sha1),
                           fecha = file.mtime(oldfiles), stringsAsFactors = FALSE)
    row.names(oldfiles) <- NULL
    
    
    # ---- Mueve --------------------------------------------------------------
    # Borrar viejos y copiar nuevos
    for (i in 1:nrow(oldfiles)){
        file.remove(oldfiles[i, "path"])
    }
    
    # In with the new
    app <- filter(newfiles, file == "ui.R" | file == "server.R")
    for (i in 1:nrow(app)){
        file.copy(app[i, "path"], file.path(maindir, "package", "inst", "leapp", app[i, "file"]))
    }
    
    erres <- filter(newfiles, file != "ui.R" & file != "server.R")
    for (i in 1:nrow(erres)){
        file.copy(erres[i, "path"], file.path(maindir, "package", "R", erres[i, "file"]))
    }
    
    

# No confirmó
} else {
    cat("Ok, que le vaiga lindo\n")
}

