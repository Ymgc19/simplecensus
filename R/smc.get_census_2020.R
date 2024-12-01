#' @title to read files
#' @description \code{smc.get_census_2020}
#' @export

smc.get_census_2020 <- function(pref_code){
  library(simplecensus)
  # データ取得
  shp <- simplecensus::smc.read_census_shp(pref_code)
  census <- simplecensus::smc.read_census_2020(pref_code)
  # 結合
  df <- left_join(shp, census)
  # 変数整形
  output <- df %>% 
    simplecensus::smc.create_variables_2020() %>% 
    simplecensus::smc.convert_percentage_vars_2020()
  # 出力
  return(output)
}
