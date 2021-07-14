#' Get Earning Announcement Data
#'
#' @description Get earnings announcements data from FMP cloud
#' @param start_date Start date
#' @param end_date End date
#' @param apikey Fmp cloud API key
#' @param local_file Path to local file. Use only if you want to save file localy.
#' @param blob_file Name of blob file
#' @param update Should existing file be updated.
#' @import httr
#' @import data.table
#' @import AzureStor
#' @return Result of GET request
#' @export
get_earning_announcements <- function(start_date = Sys.Date() - 1,
                                      end_date = Sys.Date(),
                                      local_file = NA,
                                      blob_file = "earning-calendar.rds",
                                      update = TRUE) {

  # import existing file from blob
  if (update) {
    history <- get_blob_file(blob_file)
    if (is.null(history)) {
      return(NULL)
    }
    dates_from <- seq.Date(min(as.Date(history$date)), end_date, by = 3)
    dates_to <- dates_from + 3
  } else {
    dates_from <- seq.Date(start_date, end_date, by = 3)
  }
  dates_to <- dates_from + 3

  # get data
  ea <- lapply(seq_along(dates_from), function(i) {
    fmpv3_path("earning_calendar", from = dates_from[i], to = dates_to[i], apikey = Sys.getenv("APIKEY-FMPCLOUD"))
  })
  results <- rbindlist(ea)

  # save file localy or to Azure blob
  if (!is.na(local_file)) {
    print("Save to path...")
    # fwrite(results, paste0("D:/fundamental_data/earnings_announcement/ea-", Sys.Date(), ".csv"))
  } else if (!is.na(blob_file)) {
    save_blob_files(results, file_name = "earnings-calendar", container = "fundamentals")
    print(paste0("Data saved to blob file ", blob_file))
  }

  return(ea)
}
