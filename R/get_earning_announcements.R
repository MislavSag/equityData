#' Get Earning Announcement Data
#'
#' @description Get earnings announcements data from FMP cloud
#' @param start_date Start date
#' @param end_date End date
#' @param blob_file Name of blob file
#' @import httr
#' @import data.table
#' @import AzureStor
#' @return Result of GET request
#' @export
get_earning_announcements <- function(start_date = Sys.Date() - 10,
                                      end_date = Sys.Date(),
                                      blob_file = NA) {

  # define listing dates
  dates_from <- seq.Date(start_date, end_date, by = 3)
  dates_to <- dates_from + 3

  # get data
  ea <- lapply(seq_along(dates_from), function(i) {
    fmpv_path("earning_calendar", from = dates_from[i], to = dates_to[i], apikey = Sys.getenv("APIKEY-FMPCLOUD"))
  })
  results <- rbindlist(ea)

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
