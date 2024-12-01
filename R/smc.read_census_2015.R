#' @title to read files
#' @description \code{frh.read_census_2015}



smc.read_census_2015 <- function(pref_code){
  smc.collect_census_2015(pref_code)
  download_dir <- paste0(
    formatC(pref_code, width = 2, flag = "0"),
    "国勢調査2015"
  )
  df <- smc.read_as_csv_2015(download_dir)
  unlink(download_dir, recursive = T)
  return(df)
}
