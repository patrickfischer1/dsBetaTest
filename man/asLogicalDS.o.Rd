% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/asLogicalDS.o.R
\name{asLogicalDS.o}
\alias{asLogicalDS.o}
\title{Coerces an R object into class numeric}
\usage{
asLogicalDS.o(x.name)
}
\arguments{
\item{x.name}{the name of the input object to be coerced to class
numeric. Must be specified in inverted commas. But this argument is
usually specified directly by <x.name> argument of the clientside function
{ds.aslogical.o}}
}
\value{
the object specified by the <newobj> argument (or its default name
<x.name>.logic) which is written to the serverside. For further
details see help on the clientside function {ds.asLogical.o}
}
\description{
this function is based on the native R function {as.numeric}
}
\details{
See help for function {as.logical} in native R
}
\author{
Amadou Gaye, Paul Burton for DataSHIELD Development Team
}
