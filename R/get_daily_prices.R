#' Get Daily prices
#'
#' @description Get daily prices for specified stocks
#' @param symbols Start date
#' @param start_date End dateevents
#' @param end_date End date
#' @param blob_file Name of blob file. If NA, the file wan't be saved on Azure Store blob.
#' @param blob_cont Azure storage blob container
#' @import httr
#' @import data.table
#' @import AzureStor
#' @return Result of GET request
#' @export
get_daily_prices <- function(symbols,
                             start_date = Sys.Date() - 30,
                             end_date = Sys.Date(),
                             blob_file = NA,
                             blob_cont = "fundamentals") {


  # get data
  url <- "https://financialmodelingprep.com/api/v3/historical-price-full/"
  results <- lapply(symbols, function(s) {
    url_ <- paste0(url, s)
    p <- content(RETRY("GET", url_, query = list(from = start_date, to = end_date, apikey = Sys.getenv("APIKEY-FMPCLOUD"))), time = 3)
    x <- rbindlist(p$historical, fill = TRUE)
  })
  names(results) <- symbols
  results_merged <- rbindlist(results, idcol = "symbol", fill = TRUE)

  # clean data
  results_merged$date <- as.Date(results_merged$date)
  results_merged$label <- NULL
  results_merged <- unique(results_merged)

  # save file to Azure blob
  if (!is.na(blob_file)) {
    save_blob_files(results_merged, file_name = blob_file, container = blob_cont)
    print(paste0("Data saved to blob file ", blob_file))
  }

  return(results_merged)
}
