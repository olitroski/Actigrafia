#' @title Olito's keep funcion, for rm() objects
#' 
#' @description Esta función borra o conserva objetos del gloval environment, se puede conservar objetos puntuales por nombre y por clase, el resto se borra del entorno.
#' 
#' @param obj Character vector with the name of the objects to keep
#' @param dframe Logical, set TRUE for keep all "data frames" in the environment
#' @param fun Logical, set TRUE for keep all "functions" in the environment
#' @param char Logical, set TRUE for keep all "character objects" in the environment
#' @param num Logical, set TRUE for keep all "numeric objects" in the environment
#' 
#' @return Just drop any object not included in the parameters
#' 
#' @examples
#' # Create some objects and keep one and data frames
#' cars <- mtcars
#' a <- 1
#' b <- 2
#' c <- "qwerty"
#' d <- data.frame(d = 123)
#' e <- "asdfg"
#' f <- TRUE
#' 
#' # And use the function
#' okeep(obj = c("a", "f"), dframe = TRUE)
#' 
okeep <- function(obj = NULL, dframe = FALSE, fun = FALSE, char = FALSE, num = FALSE){
     keep <- obj     

     # Funcion para dejar objetos, en un vector de character o por tipo
     if (dframe == TRUE){
          clase <- Filter(function(x) inherits(get(x), "data.frame"), ls(envir=.GlobalEnv))
          clase <- clase[!(clase %in% keep)]
          keep <- c(keep, clase)
      } 
     
     if (fun == TRUE){
          clase <- Filter(function(x) inherits(get(x), "function"), ls(envir=.GlobalEnv))
          clase <- clase[!(clase %in% keep)]
          keep <- c(keep, clase)
      } 
     
     if (char == TRUE){
          clase <- Filter(function(x) inherits(get(x), "character"), ls(envir=.GlobalEnv))
          clase <- clase[!(clase %in% keep)]
          keep <- c(keep, clase)
      }      
     
      if (num == TRUE){
          clase <- Filter(function(x) inherits(get(x), "numeric"), ls(envir=.GlobalEnv))
          clase <- clase[!(clase %in% keep)]
          keep <- c(keep, clase)
      }     
     
   
     # Pasa la lista de objetos y ejecuta
     objetos <- ls(envir=.GlobalEnv)
	objetos <- objetos[!(objetos %in% keep)]
	objetos <- paste(objetos, collapse = ", ")
	objetos <- paste("rm(", objetos, ")", sep = "")
	eval(parse(text = objetos), envir=.GlobalEnv)
}