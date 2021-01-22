#' @title Eventos o Episodios validos
#'
#' @description Funcion que toma un archivo EPI con el formato del actividorm
#'   con ligeras modificaciones y excluye algunos periodos con base en: 
#'   1. Descarta el "Dia 01", aunque hay existe la opcion "drop"
#'   2. Borra las lineas repetidas (considerar que esta funcion viene de analizar los epi antiguos)
#'   3. Evalua que los periodos tengan al menos 3 episodios
#'   4. Evalua que el primer episodio sea correcto
#'
#' @param epi Corresponde al objeto EPI 
#' @param drop Por defecto en FALSE determina si incluye o no el "Dia 01"
#' @return Devuelve una lista con los datos correctos con data management y los excluidos
#' 
#' @export
#' @examples
#' # Calcular los antecedentes, ojo el el edit.RDS proviene del analisis hecho en la aplicacion,
#' # ir a esa funcion para ver como usar por fuera del programa
#' # setwd("D:/OneDrive/INTA/Actigrafia/testfolder")
#' # set <- getset(getwd())
#' # acvedit <- check.acvfilter("2058-001-368 JRG Baseline.AWD", set)
#' # filter <-  readRDS("2058-001-368 JRG Baseline.edit.RDS")
#' # epi <- create.epi(acvedit, filter, set)
#' # epi <- epi$epiviejo
#' #
#' # Y evaluar como queda
#' # epi <- function_ValidEvents(epi)               
#' # drop <- epi$drop
#' # epi <- epi$datos
#' #
#' @importFrom utils data
#' 

function_ValidEvents <- function(epi = NULL, drop = TRUE){
    N <- data <- days <- dia.noc <- epi_estado <- fec.hora <- hi <- hidate <- inidate <- periodo <- realini <- seq.dia <- transito <- NULL
    mean_act_min <- hora <- dur_min <- dia <- NULL
	## <<< Funcion para sacar datos por sujeto >>> 
    # id <- 10648; subjdata <- epi; p <- "Noche 02"
    
    epi.todo <- function(subjdata = NULL, id = NULL){
        hora <- dur_min <- mean_act_min <- actividad <- num_epi <- str_split_fixed <- X1 <- X2 <- N <- NULL
        # print(id)
        # Nulo para registrar Dropeos del id
        id.drop <- NULL
        
        # Filtraje del id <datos>
        datos <- subjdata[subjdata$id == id, ]
        datos <- select(datos, id, periodo, hora, estado, dur_min, mean_act_min, actividad, num_epi)
        
        
        # <<< 1. Dia 01 >>> WRONG porque el Dia 01 viene despues de la Noche 01 ---------
        # Se saca el periodo "Dia 01" porque no es un dia completo.
        # --- 20.08.2020 --- Modificacion incluye Dia 01--
        # --- 22.01.2020 --- Pelotudo, era 00
        if (drop == TRUE){
            datos <- filter(datos, periodo != "Dia 00")
            id.drop <- bind_rows(id.drop, data.frame(id = id, drop = "Dia 00", stringsAsFactors = FALSE))
        }

                
        # <<< 2. Repetidos >>> se evaluan los repetidos segun hora y dia ----------------
        # Se ordena por la fechora, se crea una 2da var desplazada 1 fila pa abajo, por si hay repetido
        temp <- nrow(datos)
        datos <- arrange(datos, hora)
        datos <- distinct(datos, id, periodo, hora, estado, dur_min, .keep_all = TRUE)
        
        # Registrar si hay repetidos
        if (temp > nrow(datos)){
            id.drop <- bind_rows(id.drop, 
                                 data.frame(id = id, drop = paste0(temp, " -> ", nrow(datos)), stringsAsFactors = FALSE))
        }
        rm(temp)
        
        
        # <<< 3. Crear variables del periodo -------------------------------------------
        datos <- select(datos, -num_epi)
        datos <- bind_cols(datos, data.frame(str_split_fixed(datos$periodo, " ", n = 2), stringsAsFactors=FALSE))
        datos <- rename(datos, dia.noc = X1, seq.dia = X2)

        
        # <<< 4. Minimo 3 eventos por dia o noche >>> -----------------------------------
        # Se quita, ahora se usa todo, pero se registra
        # Cada periodo (dia o noche completo) debe tener minimo 3 episodios para poder hacer calculos
        datos <- group_by(datos, periodo)
        datos <- mutate(datos, count = n())

        # Registrar los periodos con menos de 3 episodios
        temp <- dplyr::summarize(datos, N = n())
        temp <- filter(temp, N < 3)
        if (nrow(temp) > 0){
            id.drop <- bind_rows(id.drop,
                                 data.frame(id = id, 
                                            drop = paste(temp$periodo, "=", temp$N, collapse = " - "), 
                                            stringsAsFactors = FALSE))
        }
        rm(temp)
        datos <- as.data.frame(datos)
        datos <- select(datos, -count)      
        
        # <<< 5. Primer evento correcto segun dia o noche >>>
        # Esta cosa (actividorm) determina noche o dia cuando ocurre un periodo despues de cierta hora
        # Pero aca interesa (en esta funcion) que en dia inicie con "W" y noche con "S", aca se arregla si no
        temp <- NULL
        periodo <- unique(datos$periodo)      # p <- periodo[3]
        # Por la chuta este loop es mega innecesario... algun dia lo arreglo
        for (p in periodo){
            # Captura el periodo
            filtro <- filter(datos, periodo == p)
            filtro <- arrange(filtro, hora)
            estado <- filtro$estado[1]
            dianoc <- filtro$dia.noc[1]
            
            # Guardar el epoch 
            if (dianoc == "Noche" & estado == "W"){
                # Registar error
                id.drop <- bind_rows(id.drop, 
                                     data.frame(id = id, drop = paste(filtro$dia.noc[1], filtro$seq.dia[1], "Bad Ini"), stringsAsFactors = FALSE))
                
            } else if (dianoc == "Dia" & estado == "S"){
                # Registrar el error
                id.drop <- bind_rows(id.drop, 
                                     data.frame(id = id, drop = paste(filtro$dia.noc[1], filtro$seq.dia[1], "Bad Ini"), stringsAsFactors = FALSE)) 
            
            # Si está ok lo compila
            } else {
                temp <- rbind(temp, filtro)  
            }
            
            # Limpiar
            rm(filtro, estado, dianoc)
        }
        datos <- temp
        
        # Y salir
        return(list(datos = datos, drop = id.drop))
    }

    
    # ---- Parsear a todos los sujetos ----------------------------------------
    data <- epi
    sujetos <- unique(data$id)
    datos.todo <- NULL
    datos.drop <- NULL
    contador <- 0

    for (subj in sujetos){
        # Contador
        if (contador < 50){
            cat(".")
            contador <- contador + 1
        } else {
            cat("\n")
            contador <- 0
        }
        
        temp <- epi.todo(data, id = subj)
        datos.todo <- rbind(datos.todo, temp$datos)
        datos.drop <- rbind(datos.drop, temp$drop)
        rm(temp)
    }

    
    # Terminar la base
    datos.todo <- rename(datos.todo, mean_act = mean_act_min, fec.hora = hora) %>% select(-periodo)
    
    Sys.setlocale("LC_ALL", "English")
    datos.todo <- mutate(datos.todo, hora = hour(fec.hora) + minute(fec.hora)/60, 
                         dia =  wday(fec.hora, label = TRUE),
                         hora = round(hora, 3), 
                         hora.abs = floor(hora))
    datos.todo <- mutate(datos.todo, dia = as.character(dia))
    
    
    # 6. Dia semana en transito
    datos.todo <- arrange(datos.todo, id, fec.hora)
    datos.todo <- mutate(datos.todo, periodo = paste(dia.noc, seq.dia))
    
    datos.todo <- group_by(datos.todo, id, periodo)
    datos.todo <- mutate(datos.todo, hi = min(fec.hora), hidate = as_date(hi), hi = hour(hi) + minute(hi)/60)
    datos.todo <- as.data.frame(datos.todo)
    
    datos.todo <- mutate(datos.todo, inidate = ifelse(hi > 0 & hi < 12 & dia.noc == "Noche", 1, 0))
    datos.todo <- mutate(datos.todo, realini = as_date(ifelse(inidate == 1, hidate - days(1), hidate)))

    datos.todo <- mutate(datos.todo, transito = paste0(wday(realini, label = TRUE), "-", wday(realini+days(1), label = TRUE)))
    datos.todo <- mutate(datos.todo, transito = ifelse(dia.noc == "Dia", as.character(wday(realini, label = TRUE)), transito))
    
    datos.todo <- mutate(datos.todo, dia = transito)
    datos.todo <- select(datos.todo, -transito, -realini, -inidate, -hidate, -hi, -periodo)
    
    
    # 7. Asegurar Minutos con decimales x problema con la expansion
    datos.todo <- mutate(datos.todo, dur_min = floor(dur_min))  
    
    # Ajuste segun funcion
    cat("\n")
    print(paste("dim Epi", dim(data)[1], " - dim Datos", dim(datos.todo)[1]))
    print(paste("Se perdieron:", dim(data)[1]-dim(datos.todo)[1], "filas"))
    
    # Retorna todo o un trozo
    if (drop == FALSE){
        return(datos.todo)
    } else {
        return(list(datos = datos.todo, drop = datos.drop))
    }
}
