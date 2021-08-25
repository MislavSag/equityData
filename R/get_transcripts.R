#' Get Transcripts
#'
#' @description Get Earning Call Transcript from FMP cloud.
#' @param symbols stock symbols
#' @param years years to extract data from.
#' @param blob_cont blob container
#' @param overwrite_existing_files should existing files be overwritten
#' @import httr
#' @import data.table
#' @import AzureStor
#' @return Result of GET request
#' @export
get_transcripts <- function(symbols,
                            years = 1992:2021,
                            blob_cont = "transcripts",
                            overwrite_existing_files = FALSE) {

  # remove downloaded files
  if (!overwrite_existing_files) {
    existing_files <- gsub("\\.rds", "", get_all_blob_files(blob_cont)[, 1])
    symbols <- setdiff(symbols, existing_files)
  }

  # get data
  transcripts <- lapply(symbols, function(s) {
    inner <- lapply(years, function(y) {
      fmpv_path(paste0("batch_earning_call_transcript/", s), v = "v4", year = y, apikey = Sys.getenv("APIKEY-FMPCLOUD"))
    })
    inner <- rbindlist(inner)
    if (nrow(inner) > 0) {
      inner$date <- as.POSIXct(inner$date, tz = "EST")
      save_blob_files(inner, file_name = paste0(s, ".rds"), container = blob_cont)
    } else {
      print("There are no transcripts data.")
    }
  })
}
