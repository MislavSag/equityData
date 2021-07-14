#' Get Earning Announcement Data
#'
#' @description Get earnings announcements data from FMP cloud
#' @param start_date Start date
#' @param end_date End date
#' @param apikey Fmp cloud API key
#' @param local_file PPth to local file. Use only if you want to save file localy.
#' @import httr
#' @import data.table
#' @return Result of GET request
#' @examples
#' fmpv3_path("earning_calendar", from = Sys.date() - 5, to = Sys.date(), apikey = Sys.getenv("APIKEY-FMPCLOUD"))
get_earning_announcements <- function(start_date = Sys.Date() - 1,
                                      end_date = Sys.Date(),
                                      apikey = Sys.getenv("APIKEY-FMPCLOUD"),
                                      local_file = NA) {


  # define start and end dates
  dates_from <- seq.Date(start_date, end_date, by = 3)
  dates_to <- dates_from + 3

  # get data
  ea <- lapply(seq_along(dates_from), function(i) {
    fmpv3_path("earning_calendar", from = dates_from[i], to = dates_to[i], apikey = apikey)
  })
  results <- rbindlist(earnings_announcements)

  # save file localy or to Azure blob
  if (!is.na(local_file)) {
    priont("Save to path...")
    # fwrite(results, paste0("D:/fundamental_data/earnings_announcement/ea-", Sys.Date(), ".csv"))
  }

  return(ea)
}
