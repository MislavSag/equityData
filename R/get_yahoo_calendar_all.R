#' Get Complete Earnings Calendar from yahoo
#'
#' @description Get complete earnings calendar from yahoo.
#' @param start_date Start date
#' @param end_date End date
#' @param blob_file Blob file
#' @import httr
#' @import data.table
#' @import AzureStor
#' @import rvest
#' @import stringr
#' @return Data frame with yahoo earnings calnedar data
#' @export
get_yahoo_calendar_all <- function(start_date, end_date, blob_file = NA) {

  # solve No visible binding for global variable
  `.` <- NULL

  # get data
  seq_dates <- seq.Date(start_date, end_date, by = 1)
  ec_list <- lapply(seq_dates, function(x) {
    Sys.sleep(1)
    get_yahoo_calendar(x)
  })
  ec <- rbindlist(ec_list)

  # clean table
  setnames(ec, c("symbol", "name", "time", "eps", "eps_estimate", "surprise"))
  ec[, (c("eps", "eps_estimate", "surprise")) := lapply(.SD, as.numeric), .SDcols = c("eps", "eps_estimate", "surprise")]

  # save file to Azure blob if blob_file is not NA
  if (!is.na(blob_file)) {
    save_blob_files(ec, file_name = blob_file, container = "fundamentals")
    print(paste0("Data saved to blob file ", blob_file))
  }

  return(ec)
}


#' Get Earnings Calendar from yahoo
#'
#' @description Get earnings calendar from yahoo for specific date.
#' @param date Date
#' @import httr
#' @import data.table
#' @import AzureStor
#' @import rvest
#' @import stringr
#' @return Data frame with yahoo earnings calnedar data
#' @export
get_yahoo_calendar <- function(date) {

  # solve No visible binding for global variable
  `.` <- NULL

  # get for date
  url <- "https://finance.yahoo.com/calendar/earnings?"
  p <- content(GET(url, query = list(day = date), user_agent("Mozilla/5.0")))

  # number of results
  results_number <- html_element(p, xpath = '//*[@id="fin-cal-table"]/h3/span[2]/span') %>%
    html_text() %>%
    gsub(".*of ", "", .) %>%
    str_extract("\\d+") %>%
    as.numeric()

  # if no data, stop, else extract number of results
  if (is.na(results_number)) {
    return(NULL)
  } else {
    ofssets <- 100 * (results_number %/% 100 + as.logical(results_number %% 100))
    ofssets <- ofssets - 100
    ofssets <- seq(0, ofssets, by = 100)
  }

  # scrap all for date
  if (length(ofssets) > 1) {
    p_list <- list()
    for (i in 1:length(ofssets)) {
      p_list[[i]] <- content(GET(url, query = list(day = date, offset = ofssets[i])))
      Sys.sleep(1)
    }
    ps <- lapply(p_list, function(p_) {
      p_ %>%
        html_element("table") %>%
        html_table()
    })
    tbl <- rbindlist(ps)
  } else {
    tbl <- p %>%
      html_element("table") %>%
      html_table()
  }

  return(tbl)
}
