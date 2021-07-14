#' FMP Cloud V3 Path
#'
#' @description Get data fro FMP cloud using API v3.
#' @param path Path added to Fmp cloud host
#' @param ... Query params to GET request
#' @import httr
#' @import data.table
#' @return Result of GET request
#' @examples
#' fmpv3_path("earning_calendar", from = Sys.date() - 5, to = Sys.date(), apikey = Sys.getenv("APIKEY-FMPCLOUD"))
fmpv3_path <- function(path = "earning_calendar", ...) {

  # query params
  query_params <- list(...)

  # define url
  url <- paste0(url_fmp_v3, path)

  # get data
  p <- GET(url, query = query_params)
  result <- rbindlist(httr::content(p), fill = TRUE)

  return(result)
}

