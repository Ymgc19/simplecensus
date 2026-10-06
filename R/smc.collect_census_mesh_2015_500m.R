#' @title to collect 500m mesh census files (2015) from e-Stat
#' @description \code{smc.collect_census_mesh_2015_500m}
#' @details
#' 2015年（平成27年）国勢調査の4次メッシュ（500mメッシュ）統計を
#' e-Stat からダウンロードして解凍する。
#' 取得する統計表（statsId）は以下の7つ。
#' 2015年は2020年と違い、測地系（JGD2000 / JGD2011）ごとの表は無い。
#' \itemize{
#'   \item T000847 : その1 人口等基本集計に関する事項
#'   \item T000879 : その2 人口移動集計及び就業状態等基本集計に関する事項
#'   \item T000880 : その3 従業地・通学地集計及び世帯構造等基本集計に関する事項
#'   \item T001177 : その4 5歳階級別人口
#'   \item T001203 : その5 労働力状態、産業分類及び職業分類別人口（15歳以上）
#'   \item T001204 : その6 住宅の所有及び建て方
#'   \item T001205 : その7 5年前の常住地及び従業地・通学地
#' }
#' 250mメッシュ版の T000876 / T000881 / T000882 / T001178 / T001206 〜 T001208、
#' 1kmメッシュ版の T000846 / T000877 / T000878 / T001176 / T001200 〜 T001202 と
#' 1対1で対応している。
#' @param pref_code 都道府県コード（1〜47の整数）
#' @param dir ダウンロード先の親ディレクトリ。NULL の場合は作業ディレクトリ直下
#' @return 作成した7つのフォルダのパス（character vector）
#' @export

smc.collect_census_mesh_2015_500m <- function(pref_code, dir = NULL){
  library(utils)
  library(tidyverse)

  # pref_codeの調整
  pref_code_chr <- formatC(as.integer(pref_code), width = 2, flag = "0")
  print(pref_code_chr)

  # 対象となる1次メッシュコード
  mesh_codes <- smc.mesh_code_list(pref_code)

  # 500mメッシュ（4次メッシュ）の統計表ID（2015年）
  stats_ids <- c(
    "T000847", # その1 人口等基本集計に関する事項
    "T000879", # その2 人口移動集計及び就業状態等基本集計に関する事項
    "T000880", # その3 従業地・通学地集計及び世帯構造等基本集計に関する事項
    "T001177", # その4 5歳階級別人口
    "T001203", # その5 労働力状態、産業分類及び職業分類別人口（15歳以上）
    "T001204", # その6 住宅の所有及び建て方
    "T001205"  # その7 5年前の常住地及び従業地・通学地
  )

  # urlを調整
  url_head <- "https://www.e-stat.go.jp/gis/statmap-search/data?statsId="
  url_tail <- "&downloadType=2"

  download_dirs <- c()
  for (j in seq_along(stats_ids)) {
    # ディレクトリを作成（250m版「国勢調査メッシュ2015_1」等と衝突しないようにする）
    sub_dir <- paste0(pref_code_chr, "国勢調査メッシュ2015_500m_", j)
    if (is.null(dir)) {
      download_dir <- sub_dir
    } else {
      if (!file.exists(dir)) {
        dir.create(dir, recursive = TRUE)
      }
      download_dir <- file.path(dir, sub_dir)
    }
    if (!file.exists(download_dir)) {
      dir.create(download_dir)
    }

    # 指定された都道府県のデータをfor文でdownload
    for (mesh in mesh_codes) {
      url <- paste0(url_head, stats_ids[j], "&code=", mesh, url_tail)
      zip_file <- file.path(
        download_dir,
        paste0("tbl", stats_ids[j], "H", mesh, ".zip")
      )
      download.file(url, destfile = zip_file, mode = "wb")
      unzip(zip_file, exdir = download_dir)
      file.remove(zip_file)
    }

    download_dirs <- c(download_dirs, download_dir)
    print(paste0("downloaded: ", stats_ids[j], " (", j, "/", length(stats_ids), ")"))
  }

  return(download_dirs)
}
