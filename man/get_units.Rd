% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_units.R
\name{get_units}
\alias{get_units}
\title{Get unit NUTS codes.}
\usage{
get_units(
  parentId = "",
  level = NULL,
  sort = c("id", "-id", "name", "-name"),
  lang = c("pl", "en"),
  ...
)
}
\arguments{
\item{parentId}{A 12 character NUTS id code of parent unit.
Use \code{\link{search_units}} to find unit id code.}

\item{level}{A number from 0 to 6, filters the returned unit by its level.
If \code{NULL} (default) no level filters apply. Use \code{\link{get_levels}} to find more info.}

\item{sort}{A type of sorting, "id" (default), "-id", "name", "-name"}

\item{lang}{A language of returned data, "pl" (default), "en"}

\item{...}{Other arguments passed on to \code{\link[httr]{GET}}. For example
a proxy parameters, see details.}
}
\value{
A dataset as a tibble.
}
\description{
Retrieve all unit codes or sub to given unit,
}
\details{
To get all units skip the \code{parentId} parameter. \emph{Warning!} Downloading
  all unit can take around 1 minute.

  To use a proxy to connect, a \code{\link[httr]{use_proxy}} can be
  passed to \code{\link[httr]{GET}}. For example
  \code{get_request(id, filters,
  config = httr::use_proxy(url, port, username, password))}.
}
\examples{
    # get_units(level = 2)
    # get_units("010000000000")
}
\keyword{search}
\keyword{units}
\keyword{utilities}
