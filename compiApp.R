# ----------------------------------------------------------------------------- #
# ----- Script para actualizar la app, manda todo a la carpeta de la app ------ #
# ----- hace la actualizacion, creo ------------------------------------------- #
# ----------------------------------------------------------------------------- #
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
confirmar <- readline("Va a modificar la libreria 'olitoSleep', continuar? [y|n]: ")
# confirmar <- "Y"
if (confirmar %in% c("Y", "y", "yes", "YES", "Yes", "si", "s", "S", "Si", "SI", "si")){
    # ---- Respaldar librería -------------------------------------------------

    # Sacar version del DESCRIPTION
    setwd(file.path(maindir, "package"))
    descfile <- readLines("DESCRIPTION")
    version <- descfile[grep("Version:", descfile)]
    version <- str_split(version, ": ", simplify = TRUE)
    version <- version[1, 2]

    # Rutas y zipear
    zipname <- paste0("olitoSleep_", version, ".zip")
    packfiles <- list.files(file.path(maindir, "package"), recursive = TRUE)
    zippath <- file.path(maindir, "archivos", zipname)
    zip::zipr(zippath, packfiles)

    # Limpiar
    rm(packfiles, zippath, zipname)


    # ---- Cambiar versión ----------------------------------------------------
    options(warn = -1)
    # Capturar y validar nueva version
    new.version <- readline(paste("Version actual", version, "indicar valor nuevo: "))
    # new.version <- "0.0.921"
    check.new <- str_split(new.version, "\\.", simplify = TRUE)
    check.new <- as.numeric(check.new)

    if (sum(is.na(check.new)) > 0){
        stop(paste("Error, version no numérica", new.version))

    } else if (new.version < version) {
        stop(paste("Error, la nueva version debe ser superior la actual:", version))

    } else {
        conf.ver <- readline(paste("Confirmar version", new.version, "[y|n]: "))
        # conf.ver <- "y"
        if (conf.ver != "y"){
            # Salir
            stop("...Cancelando actualización...")
            
        } else {
            # Modificar el description file
            descfile[3] <- paste("Version:", new.version)
            writeLines(descfile, "DESCRIPTION")
            cat("Version actualizada", new.version, "\n")
        }
    }
    options(warn = 0)
    rm(check.new, conf.ver, version, new.version, descfile)


    # ---- Guardar paths 'Nuevos' ---------------------------------------------
    # Bien estático, solo lso folders de funciones

    # R files nuevos
    setwd(maindir)
    rpath <- dir.exists(dir())
    rpath <- dir()[rpath]
    rpath <- rpath[grep("funciones", rpath)]
    
    listado <- list.files(file.path(maindir, rpath))
    listado <- file.path(maindir, rpath, listado)
    listado <- listado[grep("[.rR]$", listado)]
    
    # Shiny files
    listado <- c(listado, 
                 file.path(maindir, "olitoSleep.R"), 
                 file.path(maindir, "ui.R"),
                 file.path(maindir, "server.R"))
    
    # Data frame con nuevos datos
    newfiles <- data.frame(file = basename(listado),
                           path = listado,
                           size = file.info(listado)$size, stringsAsFactors = FALSE)
    row.names(newfiles) <- NULL

    # Limpiar
    rm(rpath, listado)
    

    # ---- Guardar paths 'Viejos' ---------------------------------------------
    # Los R
    setwd(file.path(maindir, "package", "R"))
    rfiles <- file.path(maindir, "package", "R", dir())

    # Los Shiny
    setwd(file.path(maindir, "package", "inst", "acti"))
    appfiles <- file.path(maindir, "package", "inst", "acti", dir())

    # En uno solo
    oldfiles <- c(rfiles, appfiles)
    rm(rfiles, appfiles)

    # En un df
    oldfiles <- data.frame(file = basename(oldfiles),
                           path = oldfiles,
                           size = file.info(oldfiles)$size, stringsAsFactors = FALSE)
    row.names(oldfiles) <- NULL


    # ---- comparar para validar ----------------------------------------------
    source(file.path(maindir, "funciones", "omerge.R"))
    archivos <- omerge(newfiles, oldfiles, byvar = "file", keep = TRUE, output = FALSE)
    archivos <- archivos$all
    archivos <- mutate(archivos,
                       size.y = ifelse(is.na(size.y), 0, size.y),
                       size.x = ifelse(is.na(size.x), 0, size.x),
                       igual = (size.x == size.y))
    archivos <- filter(archivos, igual == FALSE)

    # Si no hubiera nada que hacer
    if (nrow(archivos) == 0){
        cat("No hay archivos nuevos....")

    # Si hay para copiar
    } else {
        archivos$app <- archivos$file %in% c("server.R", "ui.R")
        
        # Un confirme primero
        cat("Archivos a copiar: \n")
        print(archivos$file)
        # confirmar <- readline("Copiar los archivos, continuar? [y|n]: ")
        confirmar <- "y"
        if (confirmar == "y"){
            # Los de la app van a otro directorio
            app <- filter(archivos, app == TRUE)
            if (nrow(app) > 0){
                for (i in 1:nrow(app)){
                    cmd <- paste0("cp ", "'", app$path.x[i], "' '", file.path(maindir, "package", "inst", "acti", app$file[i]), "'")
                    system(cmd)
                    cat("Archivo: ", app$file[i], "actualizado \n")
                }
            }
            
            # Los que se van a R
            erre <- filter(archivos, app == FALSE)
            if (nrow(erre) > 0){
                for (i in 1:nrow(erre)){
                    cmd <- paste0("cp ", "'", erre$path.x[i], "' '", file.path(maindir, "package", "R", erre$file[i]), "'")
                    system(cmd)
                    cat("Archivo: ", erre$file[i], "actualizado \n")
                }
            }
            
        } else {
            cat("Ok, que le vaiga lindo\n")
        }
    }

# No confirmo
} else {
    cat("Ok, que le vaiga lindo\n")
}

# Dejar todo limpiecito
rm(list=ls())


