# shpのobjを出力する関数
#' @title to read 500m mesh shape files
#' @description \code{smc.read_census_mesh_shp_500m}
#' @details
#' \code{smc.collect_mesh_shp_500m} でダウンロードした500mメッシュの
#' Shapefile をすべて読み込み、EPSG:4326 に変換して1つの sf オブジェクトにまとめる。
#' @param pref_code 都道府県コード（1〜47の整数）
#' @param dir ダウンロード先の親ディレクトリ。NULL の場合は作業ディレクトリ直下
#' @param delete_files TRUE のとき、読み込み後にダウンロードしたフォルダを削除する
#' @return 500mメッシュのポリゴン（sf オブジェクト）
#' @export

smc.read_census_mesh_shp_500m <- function(pref_code, dir = NULL, delete_files = FALSE){
  library(tidyverse)
  library(sf)

  folder_name <- smc.collect_mesh_shp_500m(pref_code, dir = dir)

  # ここでfolder_nameに含まれるshpをまとめて取得
  shp_to_read <- list.files(
    path = folder_name,
    pattern = "\\.shp$",
    full.names = TRUE,
    recursive = TRUE
  )
  if (length(shp_to_read) == 0) {
    stop(paste0("shpファイルが見つかりません: ", folder_name))
  }

  # 1次メッシュごとに平面直角座標系の系が異なるため、読み込むたびに4326へ変換する
  shp <- shp_to_read %>%
    purrr::map(
      \(x) sf::read_sf(x) %>% sf::st_transform(crs = 4326)
    ) %>%
    dplyr::bind_rows() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(KEY_CODE = as.numeric(KEY_CODE)) %>%
    dplyr::filter(!duplicated(KEY_CODE))

  if (isTRUE(delete_files)) {
    unlink(folder_name, recursive = TRUE)
  }

  return(shp)
}
