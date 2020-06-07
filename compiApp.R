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

    # Sacar version del DESCRIPTION
    setwd(file.path(maindir, "package"))
    descfile <- readLines("DESCRIPTION")
    version <- descfile[grep("Version:", descfile)]
    version <- str_split(version, ": ", simplify = TRUE)
    version <- version[1, 2]

    # Rutas y zipear
    zipname <- paste0("olitoSleep_", version, ".zip")
    zippath <- file.path(maindir, "archivos", zipname)
    packfiles <- list.files(file.path(maindir, "package"), recursive = TRUE)
    zip(zippath, packfiles)

    # Limpiar
    rm(packfiles, zippath, zipname)


    # ---- Cambiar versión ----------------------------------------------------
    options(warn = -1)
    # Capturar y validar nueva version
    new.version <- readline(paste("Versión actual", version, "indicar valor nuevo: "))

    check.new <- str_split(new.version, "\\.", simplify = TRUE)
    check.new <- as.numeric(check.new)

    if (sum(is.na(check.new)) > 0){
        stop(paste("Error, version no numérica", new.version))

    } else if (new.version < version) {
        stop(paste("Error, la nueva version debe ser superior la actual:", version))

    } else {
        conf.ver <- readline(paste("Confirmar versión", new.version, "[y|n]: "))

        if (conf.ver != "y"){
            # Salir
            stop("...Cancelando actualización...")

        } else {
            # Modificar el description file
            descfile[3] <- paste("Version:", new.version)
            writeLines(descfile, "DESCRIPTION")
            cat("Versión actualizada", new.version, "\n")
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
    rpath <- rpath[grep("func_", rpath)]

    rfiles <- NULL
    for (path in rpath){
        listado <- list.files(file.path(maindir, path))
        listado <- file.path(maindir, path, listado)
        rfiles <- c(rfiles, listado)
    }
    rfiles <- rfiles[grep("[.rR]$", rfiles)]
    rfiles <- c(rfiles, file.path(maindir, "olitoSleep.R"))
    rm(listado, path, rpath)

    # Shiny files
    appfiles <- c(file.path(maindir, "ui.R"),
                  file.path(maindir, "server.R"))

    # En uno solo
    newfiles <- c(rfiles, appfiles)
    rm(rfiles, appfiles)

    # Data frame con nuevos datos
    newfiles <- data.frame(file = basename(newfiles),
                           path = newfiles,
                           size = file.info(newfiles)$size, stringsAsFactors = FALSE)
    row.names(newfiles) <- NULL


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


    # ---- Combinar para validar ----------------------------------------------
    library(olibrary)
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
        confirmar <- readline("Copiar los archivos, continuar? [y|n]: ")

        if (confirmar == "y"){
            # Los de la app
            app <- filter(archivos, app == TRUE)
            if (nrow(app) > 0){
                for (i in 1:nrow(app)){
                    # file.remove(app$path.y[i])
                    # file.copy(app$path.x[i], app$path.y[i])
                    cmd <- paste0("cp ", "'", app$path.x[i], "' '", file.path(maindir, "package", "inst", "acti", app$file[i]), "'")
                    system(cmd)
                    cat("Archivo: ", app$file[i], "actualizado \n")
                }
            }

            # Los que se van a R
            erre <- filter(archivos, app == FALSE)
            if (nrow(erre) > 0){
                for (i in 1:nrow(erre)){
                    # file.remove(erre$path.y[i])
                    # file.copy(erre$path.x[i], erre$path.y[i])
                    cmd <- paste0("cp ", "'", erre$path.x[i], "' '", file.path(maindir, "package", "R", erre$file[i]), "'")
                    system(cmd)
                    cat("Archivo: ", erre$file[i], "actualizado \n")
                }
            }

        } else {
            cat("Ok, que le vaiga lindo\n")
        }
    }

# No confirmó
} else {
    cat("Ok, que le vaiga lindo\n")
}




