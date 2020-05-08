# Cargar el savedir, nada más
load.savedir <- function(mainfolder){
    # Nos vamos al mainfolder
    cd <- getwd()
	setwd(mainfolder)
	savedir <- readLines("savedir.lab")
	
	# Por si no hubiera nada deja el mainfolder
	if (length(savedir) == 0){
	    savedir <- mainfolder
	} else if (dir.exists(savedir) == FALSE){
	    savedir <- Sys.getenv("HOME")
	} 
	
	# Volvemos a donde iniciamos -vale verga porque de los shiny, pero iwalz-
	setwd(cd)
	return(savedir)
}
    
