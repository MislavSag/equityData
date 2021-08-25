#' Get Earning Surprises Data
#'
#' @description Get earnings surprises data from FMP cloud
#' @param symbols Stock symbols
#' @param blob_file Name of blob file
#' @import httr
#' @import data.table
#' @import AzureStor
#' @return Data table with earning surprises
#' @export
get_earning_surprises <- function(symbols,
                                  blob_file = NA) {

  # load all earnings surprises for  given symbols
  es <- lapply(symbols, function(s) {
    fmpv_path(paste0("earnings-surprises/", s), apikey = Sys.getenv("APIKEY-FMPCLOUD"))
  })
  results <- rbindlist(es)

  # check if there are data available for timespan
  if (nrow(results) == 0) {
    print("No data for earning announcements.")
    return(NULL)
  }

  # clean data
  results$date <- as.Date(results$date)
  results <- unique(results)

  # save file to Azure blob if blob_file is not NA
  if (!is.na(blob_file)) {
    save_blob_files(results, file_name = blob_file, container = "fundamentals")
    print(paste0("Data saved to blob file ", blob_file))
  }

  return(results)
}
