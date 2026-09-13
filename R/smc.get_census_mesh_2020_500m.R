#' @title to get 500m mesh census data with geometry (2020)
#' @description \code{smc.get_census_mesh_2020_500m}
#' @details
#' 500mメッシュの境界データ（\code{smc.read_census_mesh_shp_500m}）と
#' 500mメッシュの統計データ（\code{smc.read_census_mesh_2020_500m}）を
#' KEY_CODE で結合して sf オブジェクトとして返す。
#' 統計値は "*"（秘匿）を含むため character 型のまま返す。
#' 数値として使うときは \code{as.numeric()} を通すこと。
#'
#' \code{hitoku = TRUE} にすると \code{smc.process_confidentiality} による
#' 秘匿メッシュの合算処理まで行い、統計値の列は numeric になる。
#' @param pref_code 都道府県コード（1〜47の整数）
#' @param dir ダウンロード先の親ディレクトリ。NULL の場合は作業ディレクトリ直下
#' @param hitoku TRUE のとき秘匿メッシュの合算処理を行う
#' @return 500mメッシュ単位の国勢調査データ（sf オブジェクト）
#' @export

smc.get_census_mesh_2020_500m <- function(pref_code, dir = NULL, hitoku = FALSE){
  library(tidyverse)
  library(sf)

  # 境界データ
  shp <- smc.read_census_mesh_shp_500m(pref_code, dir = dir) %>%
    dplyr::mutate(KEY_CODE = as.numeric(KEY_CODE))

  # 統計データ
  mesh <- smc.read_census_mesh_2020_500m(pref_code, dir = dir) %>%
    dplyr::mutate(KEY_CODE = as.numeric(KEY_CODE))

  # 結合
  output <- dplyr::left_join(shp, mesh, by = "KEY_CODE")

  # 秘匿メッシュの合算処理
  if (isTRUE(hitoku)) {
    output <- smc.process_confidentiality(output)
  }

  return(output)
}
