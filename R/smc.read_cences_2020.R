#' @title to read files
#' @description \code{frh.read_census_2020}


smc.read_census_2020 <- function(pref_code){
  simplecensus::smc.collect_census_2020(pref_code)
  download_dir <- paste0(
    formatC(pref_code, width = 2, flag = "0"),
    "国勢調査2020"
  )
  df <- simplecensus::smc.read_as_csv_2020(download_dir)
  unlink(download_dir, recursive = T)
  return(df)
}
