# ------------------------------------------------------------------------------------- #
# ---- Script para crear semi periodos (dia o noche) a partir del "inidia" e "ininoc" - #
# ---- Lab. Sueño - INTA - U.Chile - O.Rojas - oliver.rojas.b@gmail.com - 11.01.2020 -- #
# ------------------------------------------------------------------------------------- #
# Crea una lista con cada semi periodo, no son puntos de corte, es el acv trozado
# Sugerencia sacar entre 20:00 y 06:00 porque el proceso del epi seri­a entre esas horas
# noc = set$ininoc; dia = set$inidia
create.semiper <- function(acv, noc = set$ininoc, dia = set$inidia){
	# cat(paste("exec(create.semiper) - noc", noc, "- dia", dia, "\n"))
    # head(acv)
    
    # Construir los lugares de corte & asegurar el time zone para evitar error.
    noche <- paste(hour(noc),":",minute(noc), ":00", sep = "")
    noche <- paste(unique(date(acv$time)), noche)
    noche <- ymd_hms(noche)
    noche <- noche[-length(noche)]      # Ultimo no vale
    noche <- with_tz(c(noche, (noche[length(noche)] + hours(24))), "UTC")   # uno mas para el loop
    
    dia <- paste(hour(dia), ":", minute(dia), ":00", sep = "")
    dia <- paste(unique(date(acv$time)), dia)
    dia <- ymd_hms(dia)
    dia <- dia[-1]            # primero no vale
    dia <- with_tz(dia, "UTC")
    
    acv <- mutate(acv, time = with_tz(time, "UTC"))
    
    
    # Evaluar que noche y dia tengan mismo length
    if (length(noche) - length(dia) != 1){
        stop("dia y noche tienen diferente largo")
    }

    # Si hay antes de las 20 del primer dia es "dia" p0, si no hay fue no más.
    inicio <- filter(acv, time < noche[1])
    if (nrow(inicio) > 0){
        d0 <- inicio
    } else {
        cat("No hay registro antes de la primera noche\n")
    }
        
    # El resto
    for (i in 1:(length(dia))){
        n <- filter(acv, time >= noche[i] & time < dia[i])
        d <- filter(acv, time >= dia[i] & time < noche[i+1]) 
        
        eval(parse(text = paste("n", i, " <- n", sep = "")))
        eval(parse(text = paste("d", i, " <- d", sep = "")))
        rm(d, n)
    }
    
    # Meterlo a una lista
    pes <- ls()[grep("n[0-9]|d[0-9]", ls())]
    peslist <- paste("pes <- list(",  paste(pes, "=", pes, collapse = ", "), ")", sep ="")
    eval(parse(text = peslist))
    
    return(pes)
}
