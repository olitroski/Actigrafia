% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03_function_hi.R
\name{function_hi}
\alias{function_hi}
\title{Calculos Hora de Inicio}
\usage{
function_hi(epi)
}
\arguments{
\item{epi}{Un objeto data.frame epi que haya pasado la funcion de chequeo}
}
\value{
data.frame con los resultados
}
\description{
Funcion para capturar la data de la hora de inicio de cada
periodo en dia y noche. Se entiende como hora de inicio y segun el ppt al
Wakeup time, sleep time como la hora del primer evento sueno o vigilia del
dia o noche segun corresponda. v1.0 08.03.2019.
}
\examples{
# check.epidata(epi)
# HI <- function_hi(epi)
}
