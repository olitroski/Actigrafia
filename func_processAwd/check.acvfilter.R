# ------------------------------------------------------------------------------------- #
# ---- Script para chequear si el acv.edit.RDS y el filter.RDS calzan y son iguales --- #
# ---- Lab. Sueño - INTA - U.Chile - O.Rojas - oliver.rojas.b@gmail.com - 27.01.2020 -- #
# ------------------------------------------------------------------------------------- #
# Check del archivo de filtro y de acv.edit para verificar que tienen lo mismo y 
# que además deje un output listo para hacer los gráficos individuales.
# Funcionamiento
# 1. Leer ambos archivos
# 2. Checar que son iguales
# 3. Procesar el ACV para crear los semiperiodos con la funcion create.semiper
# El input es el reactive awdfile()
# El output es el list del create.semiper
check.acvfilter <- function(awdfile){
	cat("exec(check.acvfilter)")
    # Cargar acv y filter ----------------------------------------------------------
    acvfile <- paste(awdfile, "_acv.edit.RDS", sep = "")
    acvfile <- readRDS(acvfile)
    
    filterfile <- paste(awdfile, ".edit.RDS", sep = "")
    filterfile <- readRDS(filterfile)
    filterfile <- filterfile[["filter"]]
    
    # filterfile <- filterfile[6:length(filterfile)]
    # filterfile <- str_split(filterfile, ": ", simplify = TRUE)[,2]
    # filterfile <- as.data.frame(str_split(filterfile, " - ", simplify = TRUE), stringsAsFactors = FALSE)
    # names(filterfile) <- c("ini", "fin")
    # filterfile <- filterfile %>% mutate(ini = ymd_hm(ini), fin = ymd_hm(fin)) %>% arrange(ini)
    
    # Chequeo que en acv$filter haya algo diferente de NA  # i <- 1
    time <- acvfile$time
    filtro <- acvfile$filter
    filtroERROR <- rep(NA, nrow(filterfile))
    filtroNA <- NULL
    
    for (i in 1:nrow(filterfile)){
        indx <- which(time >= filterfile$ini[i] & time <= filterfile$fin[i])
        
        # Solo deben haber numeros, nada de NA
        if (sum(is.na(filtro[indx])) != 0){
            filtroERROR <- c(filtroERROR, i)            
        }
        
        # Guarda el index para la otra rueba
        filtroNA <- c(filtroNA, indx)
    }
    
    # Testear lo de afuera del filtro, que sea == NA
    NAtest <- 1:nrow(acvfile)
    NAtest <- NAtest[-filtroNA]
    NAtest <- sum(!is.na(filtro[NAtest]))
    
    if (NAtest != 0){
        filtroNA <- 1
    } else {
        filtroNA <- 0
    }

    # Usar el acv.edit para crear los semiperiodos con la función ---------
    semiper <- create.semiper(acvfile)
    
    # Redistirbuir los semiperiodos (con codigo del create.actogram())
    # Combinar [noche -> dia] mismo periodo    p <- "1" ------
    per <- unique(str_sub(names(semiper), 2, 2))
    perlist <- list()
    for (p in per){
        # Capturar lo que termine en p
        i <- grep(paste(p, "$", sep = ""), names(semiper))
        
        if (length(i) == 2){
            temp <- bind_rows(semiper[[i[2]]], semiper[[i[1]]]) %>% arrange()
        } else {
            temp <- semiper[[i[1]]]
        }
        
        if (as.numeric(p) < 10){p <- paste("0", p, sep ="")}
        cmd <- paste("perlist <- append(perlist, list(per", p, " = temp))", sep = "")
        eval(parse(text=cmd))
    }

    # Corregir la hora continua decimal acá porque en la función del grafico da problema
    # Hora decimal continua ------
    lim <- as.numeric(set$ininoc)/3600
    perlist <- base::lapply(X = perlist, 
                            function(x) mutate(x, xscale = ifelse(hrdec < lim, 
                                                                  hrdec + 24, 
                                                                  hrdec)))

    # Capturar el valor inicial de cada lista para usarla en el selectInput
    timelist <- function(df){ return(min(df$time)) }
    timelist <- sapply(X = perlist, FUN = timelist)
    timelist <- data.frame(time = timelist)
    
    timelist <- timelist %>% mutate(period = row.names(timelist), 
                                    time = as_datetime(time, lubridate::origin),
                                    tlist = format(date(time), "%A %d/%m/%y"))
    

    # Y listo... tenemos el filtroNA, filtroERROR, el semiper, y el timelist
    return(list(semiper = perlist,
                filtroNA = filtroNA,
                filtroERROR = filtroERROR,
                timelist = timelist))
}
