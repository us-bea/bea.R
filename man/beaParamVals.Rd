% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/beaParamVals.r
\name{beaParamVals}
\alias{beaParamVals}
\title{Gives list of values possible for a given dataset's parameters}
\usage{
beaParamVals(beaKey, setName, paramName)
}
\arguments{
\item{beaKey}{Your API key}

\item{setName}{Name of BEA dataset (e.g., NIPA)}

\item{paramName}{Name of BEA dataset parameter (e.g., TableName)}
}
\value{
A metadata object of class 'list' of several dimensions. View list structure using 'str(yourList)'.
}
\description{
Gives list of values possible for a given dataset's parameters
}
\examples{
\dontshow{if (interactive()  && Sys.getenv("BEA_API_KEY") != "") (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
beaParamVals(Sys.getenv("BEA_API_KEY"), 'NIPA', 'TableName')
\dontshow{\}) # examplesIf}
}
\keyword{metadata}
