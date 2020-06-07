#' @title Olitos's Summary, descriptives
#' @description Esta funcion calcula estadisticas descriptivas de una variable numerica y en forma grupal, tiene version corta y extensa y de clip al porta papeles.
#' @param numvar Variable o Vector de variables en String
#' @param grpvar Variable de agrupacion, puede ser un string, factor o numerico, si no esta no se usa
#' @param data Data frame
#' @param rnd Numero de decimales
#' @param clip Clipboard option
#' @return Devuelve un data frame que ademas se pasa al portapepeles
#' @export
#' @examples
#' # numvar <- "mpg"
#' # numvar <- c("mpg", "disp", "wt")
#' # grpvar <- "am"
#' # grpvar <- c("am", "vs")
#' # osumm(numvar, data = mtcars)
#' # osumm(numvar, grpvar, data = mtcars)
#' @import dplyr
#' @importFrom stats reshape
#' @importFrom stats sd
#' @importFrom stats shapiro.test
#' @importFrom utils write.table
#'

osumm <- function(numvar, grpvar = NULL, data, rnd = 2, clip = FALSE ){
    # numvar <- "mpg"
    # numvar <- c("mpg", "disp", "wt")
    # grpvar <- "am"
    # grpvar <- c("am", "vs")
    # data <- mtcars

    # Declarar variables
    temp <- shapi <- variable <- med <- sw <- NULL

    # Algunos chequeos
    for (num in numvar){
        if (class(data[[num]]) != "numeric"){
            stop(paste("Variable:", num, "es -no numerica-"))
        }
    }


    # Determinar la base de datos, segun exista grupo o no
    if (class(grpvar) == "NULL"){
        df <- data[numvar]

    } else {
        df <- data[, c(numvar, grpvar)]
        cmd <- paste0("df <- group_by(df, ", paste(grpvar, collapse = ", "), ")")
        eval(parse(text=cmd))
    }
    resultado <- NULL


    # Medias
    for (num in numvar){
        cmd <- paste0("temp <- summarize(df, ", paste0('mean.', num), " = mean(", num, ", na.rm = TRUE))")
        eval(parse(text = cmd))
        resultado <- bind_cols(resultado, temp)
    }

    # Sd
    for (num in numvar){
        cmd <- paste0("temp <- summarize(df, ", paste0('sd.', num), " = sd(", num, ", na.rm = TRUE))")
        eval(parse(text = cmd))
        resultado <- bind_cols(resultado, temp)
    }

    # Mediana
    for (num in numvar){
        cmd <- paste0("temp <- summarize(df, ", paste0('med.', num), " = median(", num, ", na.rm = TRUE))")
        eval(parse(text = cmd))
        resultado <- bind_cols(resultado, temp)
    }

    # N
    for (num in numvar){
        cmd <- paste0("temp <- summarize(df, ", paste0('N.', num), " = n())")
        eval(parse(text = cmd))
        resultado <- bind_cols(resultado, temp)
    }

    # Missing
    for (num in numvar){
        cmd <- paste0("temp <- summarize(df, ", paste0('miss.', num), " = sum(is.na(", num, ")))")
        eval(parse(text = cmd))
        resultado <- bind_cols(resultado, temp)
    }

    # Shapiro
    shapi <- function(vari){
        n <- length(vari)
        if (n > 5000){
            return(NA)
        } else {
            p <- shapiro.test(vari)$p.value
            return(p)
        }
    }
    for (num in numvar){
        cmd <- paste0("temp <- summarize(df, ", paste0('sw.', num), " = shapi(", num, "))")
        eval(parse(text = cmd))
        resultado <- bind_cols(resultado, temp)
    }


    # Resultado dependiendo de si hay grupo
    if (class(grpvar) == "NULL"){
        # Esta es directa y sencilla
        resultado <- as.data.frame(resultado)
        resultado <- reshape(resultado, varying = 1:ncol(resultado), timevar = "variable", direction = "long")
        row.names(resultado) <- NULL
        resultado$id <- NULL

    } else {
        # Borrar las variables de grupo extra
        indx <- sapply(grpvar, function(x) grep(x, names(resultado)))
        indx <- as.data.frame(indx[-1, ])
        jvec <- NULL
        for (j in 1:ncol(indx)){
            jvec <- c(jvec, indx[, j])
        }
        resultado <- resultado[, -jvec]

        # Ahora si dar vuelta
        inicio <- length(grpvar) + 1
        resultado <- as.data.frame(resultado)
        resultado <- reshape(resultado, varying = inicio:ncol(resultado), timevar = "variable", direction = "long")
        row.names(resultado) <- NULL
        resultado$id <- NULL
        resultado <- bind_cols(select(resultado, variable), select(resultado, -variable))
    }


    # Hermoseado
    resultado <- mutate(resultado, mean = round(mean, rnd), sd = round(sd, rnd), med = round(med, rnd), sw = round(sw, 3))
    resultado <- rename(resultado, median = med, SW = sw)

    # Clip
    if (clip == TRUE){
        write.table(resultado, "clipboard-128", sep="\t", row.names=FALSE)
    }

    return(resultado)
}
# numvar <- "mpg"
# numvar <- c("mpg", "disp", "wt")
# grpvar <- "am"
# grpvar <- c("am", "vs")
# osumm(numvar, data = mtcars)
# osumm(numvar, grpvar, data = mtcars, clip = TRUE, rnd = 2)
