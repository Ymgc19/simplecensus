#' @title to read 500m mesh census data (2020)
#' @description \code{smc.read_census_mesh_2020_500m}
#' @details
#' \code{smc.collect_census_mesh_2020_500m} でダウンロードした8つの統計表を
#' KEY_CODE で横結合して1つのデータフレームとして返す。
#' 列名は T001101001 〜 / T001108001 〜 / T001141001 〜 / T001144001 〜 /
#' T001192001 〜 / T001193001 〜 / T001194001 〜 / T001195001 〜 となる
#' （250mメッシュ版の T001102001 〜 等に対応）。
#' e-Stat のテキストファイルは1行目が列名、2行目が日本語の項目名なので、
#' 2行目（項目名の行）は読み込み後に除外している。
#' 数値は "-"（該当なし）や "X"（秘匿）を含むため、すべて character 型で返す。
#' @param pref_code 都道府県コード（1〜47の整数）
#' @param dir ダウンロード先の親ディレクトリ。NULL の場合は作業ディレクトリ直下
#' @param delete_files TRUE のとき、読み込み後にダウンロードしたフォルダを削除する
#' @return 500mメッシュ単位の国勢調査データ（tibble）
#' @export

smc.read_census_mesh_2020_500m <- function(pref_code, dir = NULL, delete_files = TRUE){
  library(tidyverse)

  # データのtxtを取得
  download_dirs <- smc.collect_census_mesh_2020_500m(pref_code, dir = dir)

  df_list <- vector("list", length(download_dirs))
  for (j in seq_along(download_dirs)) {
    # 読み込むファイルのベクトル
    txt_files <- list.files(
      download_dirs[j],
      pattern = "\\.txt$",
      full.names = TRUE,
      recursive = TRUE
    )
    if (length(txt_files) == 0) {
      stop(paste0("txtファイルが見つかりません: ", download_dirs[j]))
    }

    # データを読み込んで行結合していく
    df_j <- txt_files %>%
      purrr::map(
        \(x) readr::read_delim(
          x,
          delim = ",",
          locale = readr::locale(encoding = "cp932"),
          col_types = readr::cols(.default = readr::col_character())
        )
      ) %>%
      dplyr::bind_rows() %>%
      # 2行目の日本語項目名の行（KEY_CODEが空）を除外
      dplyr::filter(!is.na(KEY_CODE)) %>%
      dplyr::distinct()

    # 2つ目以降の統計表からは共通の属性列を落としてから結合する
    if (j > 1) {
      df_j <- df_j %>%
        dplyr::select(-dplyr::any_of(c("HTKSYORI", "HTKSAKI", "GASSAN")))
    }

    df_list[[j]] <- df_j
    print(paste0("OK: ", basename(download_dirs[j])))
  }

  # 使用済みのフォルダを削除
  if (isTRUE(delete_files)) {
    for (d in download_dirs) {
      unlink(d, recursive = TRUE)
    }
  }

  # 全部のデータを列結合
  df <- purrr::reduce(
    df_list,
    \(x, y) dplyr::left_join(x, y, by = "KEY_CODE")
  )
  return(df)
}
