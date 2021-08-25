#' Get Tick Data from FMP cloud
#'
#' @description Get and save tick data from FMP cloud
#' @param symbol Stock symbol
#' @param date End date
#' @import httr
#' @import data.table
#'
#' @return Data frame with columns timestamp (SIP Unix Timestamp), ask (Ask price), bid (Bid price), bsize (Bid size - lots), asize (Ask size - lots)
#'
#' @export
get_tick_data <- function(symbol, date) {

  # extract first data for date
  url <- paste0("https://financialmodelingprep.com/api/v4/historical-price-tick/", symbol, "/", date)
  p <- content(GET(url, query = list(limit = 999, apikey = Sys.getenv("APIKEY-FMPCLOUD"))))
  tick_data <- rbindlist(p$results)

  # if there is not data for today print a message and return NULL
  if (nrow(tick_data) == 0) {
    print("There is no data for given date.")
    return(NULL)
  }

  # add symbol to data and extract last date that will be used for other date
  tick_data <- cbind(p$symbol, tick_data)
  last_t <- tail(tick_data$t, 1)
  format(last_t, scientific = FALSE)

  # scrap everything else
  last_datetime <- as.numeric(as.POSIXct(paste0(date, " 23:59:59"), tz = "America/New_York")) * 1000000000
  format(last_datetime, scientific = FALSE)
  steps <- 1
  tick_data_l <- list()
  while (last_t < last_datetime & steps < 10000 & !is.null(tick_data) && nrow(tick_data) > 1) {
    p <- RETRY("GET", url, query = list(limit = 999, ts = format(last_t, scientific = FALSE), apikey = Sys.getenv("APIKEY-FMPCLOUD")), times = 5)
    p <- content(p)
    tick_data <- rbindlist(p$results)

    # if df is empty, break to see whats happend
    # if (nrow(tick_data) == 0) {
    #   print('Checked what happened')
    #   # break()
    # }

    # save to list and extract new timestamp
    tick_data_l[[steps]] <- cbind(p$symbol, tick_data)
    last_t <- tail(tick_data$t, 1)
    steps <- steps + 1
  }
  tick_data_day <- rbindlist(tick_data_l[-length(tick_data_l)])

  return(tick_data_day)
}
