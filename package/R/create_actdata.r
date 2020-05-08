# ---------------------------------------------------------------------------- #
# --- Funcion para tomar un awd de la función acv y agrupar según cantidad --- #
# --- minutos, fabricando tramos y calculando estadsiticas con la actividad -- #
# --- como fuente para hacer análisis de actividad - O.Rojas 02.12.19 -------- #
# ---------------------------------------------------------------------------- #
create_actdata <- function(acv = NULL, epi = NULL, corte = 60){

 corte <- 15
    awd <- acv 
        
    # Separar el perí­odo
    epi <- cbind(epi, 
                 data.frame(str_split(epi$periodo, " ", simplify = TRUE), 
                            stringsAsFactors = FALSE))
    epi <- rename(epi, dianoc = X1, nper = X2)
    epi <- select(epi, stage, duracion, periodo, dianoc, nper)

    # Merge epi-awd
    awd <- omerge(awd, epi, byvar = "stage", keep = TRUE, output = FALSE)       # Se va el primer dia
    awd <- awd$match
    awd <- arrange(awd, index) %>% select(-merge, -act2)
    awd <- mutate(awd, hour = as.integer(dec))
    
   
    # Colapsar la base según los cortes
    ## Crear variable de corte para hacer el group by
    if (corte == 60){
        awd <- mutate(awd, corte = hour(fec))
    } else if (corte %in% c(5, 10, 15, 20, 30)){
        cut <- seq(from = 0, to = 60, by = corte)
        cut[1] <- -Inf      # Para el label del cut
        cut[length(cut)] <- +Inf
        
        lab <- seq(corte, 60, by = corte)
        lab <- paste(lab - corte, lab, sep = "-")
        
        temp <- minute(awd$fec)
        temp <- cut(temp, breaks = cut, right = FALSE, labels = lab)
        awd$corte <- temp
    } else {
        stop("El corte debe ser 5, 10, 15, 20, 30 o 60 minutos")
    }

    # juntar la hora con los cortes, excepto si corte == 60
    head(awd)
    awd$epoch <- paste(ifelse(awd$hour<10, paste("0", awd$hour, sep=""), awd$hour), awd$corte)
    
    
    # Funcion para las stats desde un group_by de dplyr
    stats <- function(grupbai = NULL){
        temp <- summarize(grupbai, sum = sum(act3), mean = mean(act3), sd = sd(act3),
                d1 = quantile(act3, 0.1), d9 = quantile(act3, 0.9),
                q1 = quantile(act3, 0.25), q2 = quantile(act3, 0.5), q3 = quantile(act3, 0.75))
        return(temp)
    }
    
    all <- stats(group_by(awd, epoch))
    sw  <- stats(group_by(awd, acti2, epoch))
    swd <- stats(group_by(awd, acti2, dianoc, epoch))
    
    
    write.csv(swd, "swd.csv")
    
    
    head(awd)
        
    
}
