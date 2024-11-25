#' @title to read files
#' @description \code{frh.read_census_2015}
#' @export

krb.read_census_2015 <- function(pref_code){
  krb.collect_census_2015(pref_code)
  download_dir <- paste0(
    formatC(pref_code, width = 2, flag = "0"),
    "国勢調査2015"
  )
  df <- krb.read_as_csv_2015(download_dir)
  unlink(download_dir, recursive = T)
  return(df)
}
