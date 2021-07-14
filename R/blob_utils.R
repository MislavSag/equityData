#' Get File from blob
#'
#' @description Get files from Azure blob storage. Files can be in csv and rds format.
#' @param file File to import (without extensions)
#' @param container Blob container
#' @import httr
#' @import data.table
#' @import AzureStor
#' @return Result of GET request
#' @export
get_blob_file <- function(file, container = "fundamentals") {
    bl_endp_key <- storage_endpoint(Sys.getenv("BLOB-ENDPOINT"), key=Sys.getenv("BLOB-KEY"))
    cont <- storage_container(bl_endp_key, container)
    if (azure_file_exists(cont, paste0(file, ".csv"))) {
      if (grepl("csv", file)) {
        history <- as.data.table(storage_read_csv2(cont, file))
      } else if (grepl("rds", file)) {
        history <- as.data.table(storage_read_csv2(cont, file))
      } else {
        print("File must have csv or rds extension")
      }
    } else {
      print(paste0("File ", file, " doesn't exists on the blob"))
      return(NULL)
    }
    return(history)
}

#' Save files to blob
#'
#' @description Save files bob. It automaticly saves files as csv and rds objects
#' @param object Object to save
#' @param file_name File to import (without extensions)
#' @param container Blob container
#' @import httr
#' @import data.table
#' @import AzureStor
#' @return Result of GET request
#' @export
save_blob_files <- function(object, file_name, container = "fundamentals") {
  bl_endp_key <- storage_endpoint(Sys.getenv("BLOB-ENDPOINT"), key=Sys.getenv("BLOB-KEY"))
  cont <- storage_container(bl_endp_key, container)
  storage_write_csv2(object, cont, file = paste0(file_name, ".csv"))
  storage_save_rds(object, cont, file = paste0(file_name, ".rds"))
}
