#' @title to collect 500m mesh shape files from e-Stat
#' @description \code{smc.collect_mesh_shp_500m}
#' @details
#' e-Stat の境界データのうち、4次メッシュ（500mメッシュ）の
#' Shapefile（世界測地系平面直角座標系）をダウンロードして解凍する。
#' 境界データの dlserveyId は
#' S = 3次メッシュ（1km）、H = 4次メッシュ（500m）、Q = 5次メッシュ（250m）。
#' @param pref_code 都道府県コード（1〜47の整数）
#' @param dir ダウンロード先の親ディレクトリ。NULL の場合は作業ディレクトリ直下
#' @return 作成したフォルダのパス
#' @export

smc.collect_mesh_shp_500m <- function(pref_code, dir = NULL) {
  library(utils)
  library(sf)

  # pref_codeの調整
  pref_code_chr <- formatC(as.integer(pref_code), width = 2, flag = "0")

  # 対象となる1次メッシュコード
  mesh_codes <- smc.mesh_code_list(pref_code)

  # ダウンロードするurlのベクトル
  url1 <- "https://www.e-stat.go.jp/gis/statmap-search/data?dlserveyId=H&code="
  url2 <- "&coordSys=2&format=shape&downloadType=5"
  dl_url_vec <- paste0(url1, mesh_codes, url2)

  # フォルダ名の作成（250m・1kmのフォルダ名と衝突しないようにする）
  sub_dir <- paste0(pref_code_chr, "census_mesh_shp_500m")
  if (is.null(dir)) {
    folder_name <- sub_dir
  } else {
    if (!file.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
    folder_name <- file.path(dir, sub_dir)
  }
  dir.create(folder_name, showWarnings = FALSE)

  # ZIPファイルをダウンロードし、解凍
  for (i in seq_along(dl_url_vec)) {
    zip_file <- file.path(folder_name, paste0("MESH0", mesh_codes[i], ".zip"))
    # 'wb'モードでバイナリファイルをダウンロード
    download.file(dl_url_vec[i], destfile = zip_file, mode = "wb")
    unzip(zip_file, exdir = folder_name)
    file.remove(zip_file)
  }

  return(folder_name)
}
