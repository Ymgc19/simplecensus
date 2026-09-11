#' @title to read files
#' @description \code{smc.process_cenfidentiality}
#' @export



smc.process_cenfidentiality <- function(mesh){
  library(tidyverse)
  # 秘匿処理前のメッシュレベル統計を引数に
  hitoku_ari_df <- mesh %>% 
    filter(!is.na(HTKSAKI.x))
  # 処理する数
  hitoku_ari_df_nrow <- nrow(hitoku_ari_df)
  # 1個ずつデータを処理している
  for (j in 1:hitoku_ari_df_nrow) {
    target <- hitoku_ari_df[j, ]
    hitokusaki <- target$HTKSAKI.x  # 合算先のKEY_CODE
    to_plus_pop <- target$T001102001 # 人口
    to_plus_house <- target$T001102034 # 世帯数
    # データフレームを更新（正しく代入する）
    mesh <- mesh %>%
      mutate(
        T001102001 = if_else(KEY_CODE == hitokusaki, T001102001 + to_plus_pop, T001102001),
        T001102034 = if_else(KEY_CODE == hitokusaki, T001102034 + to_plus_house, T001102034)
      )
    print(j / hitoku_ari_df_nrow)
  }
  return(mesh)
}



