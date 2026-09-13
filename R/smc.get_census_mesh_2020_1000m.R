#' @title to get 1km mesh census data with geometry (2020)
#' @description \code{smc.get_census_mesh_2020_1000m}
#' @details
#' 1kmメッシュ（3次メッシュ）の境界データ（\code{smc.read_census_mesh_shp_1000m}）と
#' 統計データ（\code{smc.read_census_mesh_2020_1000m}）を KEY_CODE で結合して
#' sf オブジェクトとして返す。
#'
#' \code{hitoku = TRUE} にすると \code{smc.process_confidentiality} による
#' 秘匿メッシュの合算処理まで行い、統計値の列は numeric になる。
#' FALSE の場合、統計値は "*"（秘匿）を含むため character のまま返る。
#'
#' ダウンロードした一時ファイルは作業ディレクトリ直下に作成される。
#' @param pref_code 都道府県コード（1〜47の整数）
#' @param hitoku TRUE のとき秘匿メッシュの合算処理を行う
#' @return 1kmメッシュ単位の国勢調査データ（sf オブジェクト）
#' @export

smc.get_census_mesh_2020_1000m <- function(pref_code, hitoku = FALSE){
  library(tidyverse)
  library(sf)

  # 境界データ
  shp <- smc.read_census_mesh_shp_1000m(pref_code) %>%
    dplyr::mutate(KEY_CODE = as.numeric(KEY_CODE))

  # 統計データ
  mesh <- smc.read_census_mesh_2020_1000m(pref_code) %>%
    dplyr::mutate(KEY_CODE = as.numeric(KEY_CODE))

  # 結合
  output <- dplyr::left_join(shp, mesh, by = "KEY_CODE")

  # 秘匿メッシュの合算処理
  if (isTRUE(hitoku)) {
    output <- smc.process_confidentiality(output)
  }

  return(output)
}
