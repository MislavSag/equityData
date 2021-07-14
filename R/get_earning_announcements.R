#' Get Earning Announcement Data
#'
#' @description Get earnings announcements data from FMP cloud
#' @param start_date Start date
#' @param end_date End date
#' @param local_file Path to local file. Use only if you want to save file localy.
#' @param blob_file Name of blob file
#' @import httr
#' @import data.table
#' @import AzureStor
#' @return Result of GET request
#' @export
get_earning_announcements <- function(start_date = Sys.Date() - 1,
                                      end_date = Sys.Date(),
                                      local_file = "D:/fundamental_data/earnings_announcement/earning-calendar",
                                      blob_file = "earning-calendar") {

  # define listing dates
  dates_from <- seq.Date(start_date, end_date, by = 3)
  dates_to <- dates_from + 3

  # get data
  ea <- lapply(seq_along(dates_from), function(i) {
    fmpv3_path("earning_calendar", from = dates_from[i], to = dates_to[i], apikey = Sys.getenv("APIKEY-FMPCLOUD"))
  })
  results <- rbindlist(ea)

  # save file localy or to Azure blob or both
  if (!is.na(local_file)) {
    print("Save to local path")
    fwrite(results, paste0(local_file, ".csv"))
    saveRDS(results,paste0(local_file, ".rds") )
  } else if (!is.na(blob_file)) {
    save_blob_files(results, file_name = "earnings-calendar", container = "fundamentals")
    print(paste0("Data saved to blob file ", blob_file))
  }

  return(ea)
}
