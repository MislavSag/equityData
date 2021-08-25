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
#' @import parallel
#' @import quarks
#' @import runner
#' @return Data frame with yahoo earnings calnedar data
#' @export
# parameters
#
# library(parallel)
# library(quarks)
# library(runner)
# library(data.table)
# library(httr)
# library(equityData)
# library(AzureStor)
# library(doParallel)
#
# prices <- get_blob_file("prices.rds", container = "fundamentals", save_file = "D:/fundamental_data/prices.rds", refresh_data_old = 30)
#
# # usa and sp500 stocks. This is used later for sabsampling
# apikey_fmp = Sys.getenv("APIKEY-FMPCLOUD")
# fmp_host = "https://financialmodelingprep.com/"
# url <- modify_url(fmp_host, path = "api/v3/available-traded/list",
#                   query = list(apikey = apikey_fmp))
# stocks <- rbindlist(content(GET(url)))
# usa_symbols <- stocks[exchange %in% c("AMEX", "New York Stock Exchange Arca",
#                                       "New York Stock Exchange", "NasdaqGS",
#                                       "Nasdaq", "NASDAQ", "AMEX", "NYSEArca",
#                                       "NASDAQ Global Market", "Nasdaq Global Market",
#                                       "NYSE American", "Nasdaq Capital Market",
#                                       "Nasdaq Global Select")]
# url <- modify_url(fmp_host, path = "api/v3/historical/sp500_constituent",
#                   query = list(apikey = apikey_fmp))
# sp500 <- rbindlist(httr::content(GET(url)))
# sp500_symbols <- unique(c(sp500$removedTicker, sp500$symbol))
#
# symbols = unique(prices$symbol)
# symbols = symbols[symbols %in% unique(usa_symbols$symbol)]
# p = c(0.975)
# win_size = c(200)
# model = c("EWMA")
# method = c("age", "vwhs", "fhs")
# forecast_length = 150
get_risk_quarks <- function(prices, p = c(0.975), win_size = 200, model = c("EWMA", "GARCH"),
                           method = c("plain", "age", "vwhs", "fhs"), forecast_length = 100, container = 'risk-quarks-day') {

  # create container if it doesn not exists
  bl_endp_key <- storage_endpoint(Sys.getenv("BLOB-ENDPOINT"), key=Sys.getenv("BLOB-KEY"))
  existing_containers <- list_blob_containers(bl_endp_key)
  if (container %in% names(existing_containers)) {
    cont <- storage_container(bl_endp_key, container)
  } else {
    stop("Container does not exists.")
  }

  # definr parametres set
  symbols <- unique(prices$symbol)
  params <- expand.grid(p, model, method, win_size, forecast_length, stringsAsFactors = FALSE)
  colnames(params) <- c("p", "model", "method", "win_size", "forecast_length")
  params <- params[!(params$model == 'GARCH' & params$method == "fhs"), ]

  # get all files from the container
  cont_files <- get_all_blob_files(container)

  # get forecasts
  for (sym in symbols) {

    # take sample
    print(sym)
    sample_ <- prices[symbol == sym]
    sample_[, returns := adjClose / shift(adjClose) - 1]
    sample_ <- na.omit(sample_[, .(symbol, date, returns)])

    # create file name
    file_name <- paste0(sym, ".rds")

    # loop over params
    risks_list <- list()
    for (j in 1:nrow(params)) {

      # params
      params_ <- params[j, ]
      print(params_)

      # stop if number of rows is smaller than params_$win_size + params_$forecast_length
      if (nrow(sample_) < params_$win_size + params_$forecast_length ) {
        next()
      }

      # set up clusters
      cl <- makeCluster(8L)
      clusterExport(cl, c("sample_", "params_", "get_series_statistics"), envir = environment())

      # roll estimation
      roll_quarks <- runner(
        x = data.frame(y=sample_$returns),
        f = function(x) {
          library(quarks)
          library(data.table)
          print(x[1])
          if (params_$method == "fhs") {
            y <- rollcast(x,
                          p = params_$p,
                          model = params_$model,
                          method = params_$method,
                          nout = params_$forecast_length,
                          nwin = params_$win_size,
                          nboot = 100
            )
          } else {
            y <- rollcast(x,
                          p = params_$p,
                          model = params_$model,
                          method = params_$method,
                          nout = params_$forecast_length,
                          nwin = params_$win_size
            )
          }
          VaR <- as.data.table(get_series_statistics(y$VaR))
          ES <- as.data.table(get_series_statistics(y$ES))
          colnames(ES) <- gsub("var_", "es_", colnames(ES))
          return(cbind.data.frame(VaR, ES))
        },
        k = params_$win_size + params_$forecast_length,
        na_pad = TRUE
        # cl = cl
      )
      stopCluster(cl)

      # clean table
      risks <- rbindlist(lapply(roll_quarks, as.data.frame), fill = TRUE)[, -1]
      colnames(risks) <- paste(params_$p * 1000, paste0(params_[2:ncol(params_)], collapse = "_"), sep = "_", colnames(risks))
      risks_list[[j]] <- cbind.data.frame(symbol = sample_$symbol, datetime = sample_$date, risks)
    }
  }

  print("Come here")
  # merge all for symbol
  risks_symbol <- do.call(cbind, risks_list)
  risks_symbol <- risks_symbol[, !duplicated(colnames(risks_symbol))]

  # save to blob
  print("Come here")
  save_blob_files(risks_symbol, file_name, container)
}

# get_ris_quarks(prices, p = 0.975, win_size = 200, model = "EWMA", method = c("age", "vwhs", "fhs"))

get_series_statistics <- function(series) {
  var_1 <- series[1]
  var_day <- mean(series[1:8], na.rm = TRUE)
  var_week <- mean(series[1:40], na.rm = TRUE)
  var_month <- mean(series, na.rm = TRUE)
  var_std <- sd(series, na.rm = TRUE)
  return(list(var_1 = var_1, var_day = var_day, var_week = var_week, var_month = var_month, var_std = var_std))
}
