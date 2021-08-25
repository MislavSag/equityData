#' Update Earning Announcement Data
#'
#' @description Update earnings announcements data from FMP cloud
#' @param blob_file Blob file to save updated data
#' @import httr
#' @import data.table
#' @import AzureStor
#' @return Result of GET request
#' @export
update_earning_announcements <- function(blob_file = "earnings-calendar.rds") {


  # get old file
  history <- get_blob_file(blob_file,
                          "fundamentals",
                          save_file = "D:/fundamental_data/earnings-calendar.rds",
                          refresh_data_old = 0,
                          overwrite = TRUE)
  history <- history[date < Sys.Date() - 10]

  # get new data
  new <- get_earning_announcements(
    max(history$date) + 1,
    Sys.Date(),
    blob_file = NA
  )

  # check if there are data available for timespan
  if (nrow(new) == 0) {
    print("No data for earning announcements.")
    return(NULL)
  }

  # clean data
  events_new <- rbind(history, new)
  events_new <- unique(events_new)

  # save file to Azure blob if blob_file is not NA
  if (!is.na(blob_file)) {
    save_blob_files(events_new, file_name = blob_file, container = "fundamentals")
    print(paste0("Data saved to blob file ", blob_file))
  }

  return(events_new)
}
