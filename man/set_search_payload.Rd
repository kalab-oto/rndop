% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_search_payload.R
\name{set_search_payload}
\alias{set_search_payload}
\title{Generate a template for user search payload for POST request}
\usage{
set_search_payload(rfTaxon, rfCeledi, rfKategorie)
}
\value{
\code{list}
}
\description{
Allows to generate a template of list of keys and values of user POST request for \code{\link{ndop_download}} function. This is useful for users who wants experiment, or are familiar with the content of the POST request sent to the.
}
\examples{
# Prepare search payload for 6175 KFME field
pld <- set_search_payload()
pld$rfKvadrat <- 6175
}
