#' @title to read files
#' @description \code{smc.get_census_2015}
#' @export

smc.get_census_2015 <- function(pref_code){
  library(simplecensus)
  # データ取得
  shp <- simplecensus::smc.read_census_shp(pref_code) %>% 
    mutate(KEY_CODE = as.numeric(KEY_CODE))
  census <- simplecensus::smc.read_census_2015(pref_code) %>% 
    mutate(KEY_CODE = as.numeric(KEY_CODE))
  # 結合
  df <- left_join(shp, census)
  # 変数整形
  output <- df %>% 
    simplecensus::smc.create_variables_2015() %>% 
    simplecensus::smc.convert_percentage_vars_2015()
  # 出力
  return(output)
}
