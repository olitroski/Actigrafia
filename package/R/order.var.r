#' @title Olito's OrderVar para ordenar variables
#'
#' @description Ordena variables dentro de un data.frame
#'
#' @param datos Un data.frame
#' @param variable o variables a mover (en un vector char)
#' @param after Después de cual variable se dese mover (char or index), default = 1
#'
#' @return Data frame con las variables ordenadas
#'
#' @examples
#' # Get some data and save order
#' # data(mtcars)
#' # nameberfore <- names(mtcars)
#'
#' # Ordenar
#' # mtcars <- ordervar(mtcars, "mpg", "am")
#' # nameberfore; names(mtcars)
#'
#' # mtcars <- ordervar(mtcars, c("gear", "carb"), after = "wt")
#'


## Stata order function or kinda
# Just because I like tidy data frames... It's my life ok!!
ordervar <- function(datos, varmove, after=1){
	require(dplyr)
	varlist <- names(datos)

	# drop from varlist the variables to move
	varlist <- varlist[!(varlist %in% varmove)]

	# Check some stuff before, in after=1 move to begining.
	if (after == 1){
		varlist <- c(varmove, varlist)

	} else if (class(after)!="character"){
		cat("After variable must be character\n")

	} else if (length(after) > 1){
		cat("Only one after variable is needed\n")

	} else {
	# Cut the varlist in two pieces, before and after, the after variable
		index  <- grep(after, varlist)
		data.first <- varlist[1:index]
		data.last  <- varlist[index+1:length(varlist)]

		# Combine all pieces of variables
		varlist <- c(data.first, varmove, data.last)
		varlist <- varlist[!is.na(varlist)]
	}

	# We got now the new order of variables, I use this vector to build a string
	# with the syntax for "select" from "dplyr", and put the result in object "ordered".
	vartext <- paste(varlist, collapse=", ")
	command <- paste("ordered <- select(datos, ", vartext, ")", sep="")
	# yeah, I know it's not pro. blah blah blah, parse(not pro jajaj)... but is simple
	eval(parse(text=command))
	return(ordered)
	# And beautiful =)
}

