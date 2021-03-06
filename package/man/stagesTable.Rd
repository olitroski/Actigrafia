% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stagesTable.R
\name{stagesTable}
\alias{stagesTable}
\title{Crear tabla de estados de sleep wake}
\usage{
stagesTable(acveditRDS, per)
}
\arguments{
\item{acveditRDS}{es una lista que saca la funcion check.acvfilter}

\item{per}{el periodo para seleccionarlo desde la lista}
}
\value{
un data frame con la info del periodo
}
\description{
Para cada periodo tipo per01 muestra periodos.
}
\examples{
# setwd("D:/OneDrive/INTA/Actigrafia/testfolder")
# awdfile <- "2086-308-045 CHT Visit2.AWD"
# acveditRDS <- check.acvfilter(sub(".AWD", "", awdfile))
# acveditRDS <- acveditRDS$semiper
# stagesTable(acveditRDS, "per01")
}
