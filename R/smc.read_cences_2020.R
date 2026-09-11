#' @title to read files
#' @description \code{smc.read_census_2020}


smc.read_census_2020 <- function(pref_code, dir = NULL){
  smc.collect_census_2020(pref_code, dir = dir)
  sub_dir <- paste0(
    formatC(pref_code, width = 2, flag = "0"),
    "国勢調査2020"
  )
  if (is.null(dir)) {
    download_dir <- sub_dir
  } else {
    download_dir <- file.path(dir, sub_dir)
  }
  df <- smc.read_as_csv_2020(download_dir)
  unlink(download_dir, recursive = T)
  return(df)
}
