#' Get File from blob
#'
#' @description Get files from Azure blob storage. Files can be in csv and rds format.
#' @param blob_file File to import from Azure store container
#' @param container Blob container
#' @param save_file File name to save object localy
#' @param refresh_data_old Number of days after which new, refreshed data would be donwloaded fom container.
#' @param ... Other arguments to storage_download function from AzureStore pakcage
#' @import httr
#' @import data.table
#' @import AzureStor
#' @return Result of GET request
#' @export
get_blob_file <- function(blob_file, container = "fundamentals",
                          save_file = file.path(getwd(), file),
                          refresh_data_old = 3, ...) {


  # define Azure Store objects
  bl_endp_key <- storage_endpoint(Sys.getenv("BLOB-ENDPOINT"), key=Sys.getenv("BLOB-KEY"))
  cont <- storage_container(bl_endp_key, container)

  # check if file should be redownloaed if exists
  if (file.exists(save_file) &&
      refresh_data_old != 0  &&
      (Sys.Date() - as.Date(file.mtime(save_file))) < refresh_data_old) {
      print("Use already dwnloaded file.")
      obj <- readRDS(save_file)
  } else {
    # download and read file
    conn <- storage_download(cont, blob_file, save_file, ...)
    obj <- readRDS(save_file)
  }

  return(obj)
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
  if (grepl("csv", file_name)) {
    storage_write_csv2(object, cont, file = file_name)
  } else if (grepl("rds", file_name)) {
    storage_save_rds(object, cont, file = file_name)
  }
}

#' Get All Blob Files
#'
#' @description Get all blob files from specifed container.
#' @param container Blob container
#' @import httr
#' @import AzureStor
#' @return Result of GET request
#' @export
get_all_blob_files <- function(container = "transcripts") {
  bl_endp_key <- storage_endpoint(Sys.getenv("BLOB-ENDPOINT"), key=Sys.getenv("BLOB-KEY"))
  cont <- storage_container(bl_endp_key, container)
  list_blobs(cont)
}

#' Import Files From Container
#'
#' @description Import all blob files from container
#' @param save_file Path to local file. This is where merged file is saved and path to load data from if it is already saved
#' @param container Blob container
#' @param refresh_data_old Look at refresh_data_old in get_blob_file
#' @param ... other arguments to get_blob_file function.
#' @import httr
#' @import AzureStor
#' @return Result of GET request
#' @export
get_all_blob_files_content <- function(save_file, container = "transcripts", refresh_data_old = 30, ...) {


  # define Azure Store objects
  bl_endp_key <- storage_endpoint(Sys.getenv("BLOB-ENDPOINT"), key=Sys.getenv("BLOB-KEY"))
  cont <- storage_container(bl_endp_key, container)

  # define save path for all files
  save_path <- dirname(save_file)

  # check if file should be redownloaed if exists
  if (file.exists(save_file) &&
      refresh_data_old != 0  &&
      (Sys.Date() - as.Date(file.mtime(save_file))) < refresh_data_old) {
      print("Use already dwnloaded file.")
      obj <- readRDS(save_file)
  } else {
    # import all files and merge to one object
    file_to_import <- list_blobs(cont)
    df_list <- lapply(file_to_import$name, function(x) {
      get_blob_file(x, container, file.path(save_path, container, x), refresh_data_old = refresh_data_old, ...)
    })
    obj <- rbindlist(df_list)

    # save merged file
    saveRDS(obj, save_file)
  }

  return(obj)
}
