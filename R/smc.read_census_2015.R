#' @title to read files
#' @description \code{frh.read_census_2015}



smc.read_census_2015 <- function(pref_code, dir = NULL){
  smc.collect_census_2015(pref_code, dir = dir)
  sub_dir <- paste0(
    formatC(pref_code, width = 2, flag = "0"),
    "国勢調査2015"
  )
  if (is.null(dir)) {
    download_dir <- sub_dir
  } else {
    download_dir <- file.path(dir, sub_dir)
  }
  df <- smc.read_as_csv_2015(download_dir)
  unlink(download_dir, recursive = T)
  return(df)
}
