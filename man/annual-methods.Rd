% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annual.r
\docType{methods}
\name{annual}
\alias{annual}
\alias{annual,Dataset-method}
\alias{annual,ts-method}
\title{Annualizza l'oggetto}
\usage{
annual(x)

\S4method{annual}{Dataset}(x)

\S4method{annual}{ts}(x)
}
\arguments{
\item{x}{Dataset da annualizzare}
}
\value{
un dataset annualizzato
}
\description{
Se le serie contenute sono gia' annuali, non fa nulla e
lascia la serie cosi' com'e'
}
