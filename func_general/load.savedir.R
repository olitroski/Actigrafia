# Cargar el savedir, nada más

load.savedir <- function(mainfolder){
	cd <- getwd()
	setwd(mainfolder)
	savedir <- readLines("savedir.lab")
	
	# Por si no hubiera nada deja el mainfolder
	if (length(savedir) == 0){
	    savedir <- mainfolder
	}
	
	setwd(cd)
	return(savedir)
}
    
