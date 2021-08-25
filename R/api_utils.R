#' FMP Cloud V3 Path
#'
#' @description Get data fro FMP cloud using API v3.
#' @param path Path added to Fmp cloud host
#' @param v API version
#' @param ... Query params to GET request
#' @import httr
#' @import data.table
#' @return Result of GET request
#' @export
fmpv_path <- function(path = "earning_calendar", v = "v3", ...) {

  # query params
  query_params <- list(...)

  # define url
  url <- paste0("https://financialmodelingprep.com/api/", v, "/", path)

  # get data
  p <- GET(url, query = query_params)
  result <- rbindlist(httr::content(p), fill = TRUE)

  return(result)
}
