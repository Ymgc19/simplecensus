#' @title to read files
#' @description \code{smc.read_census_mesh_2020}

smc.read_census_mesh_2020 <- function(pref_code){
  smc.collect_census_mesh_2020(pref_code) # データのtxtを取得
  
  if (pref_code <= 9){
    pref_code_chr <- as.character(paste("0", pref_code, sep = ""))
  }
  else{
    pref_code_chr <- as.character(pref_code)
  }
  
  # ========== データ1に関して ========== #
  download_dir <- paste0(
    formatC(pref_code_chr, width = 2, flag = "0"),
    "国勢調査メッシュ2020_1"
  ) # 変数1のフォルダ指定
  # 読み込むファイルのベクトル
  dir_vec_1 <- fs::dir_ls(here::here(download_dir),
                         recurse = TRUE,
                         regexp = ".txt$")
  # データを読み込んで行結合していく
  df_1 <- read_delim(dir_vec_1[1], delim = ",",
                     locale = locale(encoding = "cp932"))
  for (i in 2:length(dir_vec_1)){
    # ひとつひとつデータの読み込み
    hoge_1 <- read_delim(dir_vec_1[i], delim = ",",
                       locale = locale(encoding = "cp932"))
    # 行を結合していく
    df_1 <- bind_rows(df_1, hoge_1) %>% distinct()
  }
  unlink(download_dir, recursive = T) # 使用済みのフォルダを削除
  
  # ========== データ2に関して ========== #
  download_dir <- paste0(
    formatC(pref_code_chr, width = 2, flag = "0"),
    "国勢調査メッシュ2020_2"
  ) # 変数1のフォルダ指定
  # 読み込むファイルのベクトル
  dir_vec_2 <- fs::dir_ls(here::here(download_dir),
                          recurse = TRUE,
                          regexp = ".txt$")
  print(download_dir)
  # データを読み込んで行結合していく
  df_2 <- read_delim(dir_vec_2[1], delim = ",",
                     locale = locale(encoding = "cp932"))
  for (i in 2:length(dir_vec_2)){
    # ひとつひとつデータの読み込み
    hoge_2 <- read_delim(dir_vec_2[i], delim = ",",
                       locale = locale(encoding = "cp932"))
    # 行を結合していく
    df_2 <- bind_rows(df_2, hoge_2) %>% distinct()
  }
  unlink(download_dir, recursive = T) # 使用済みのフォルダを削除
  
  # ========== データ3に関して ========== #
  download_dir <- paste0(
    formatC(pref_code_chr, width = 2, flag = "0"),
    "国勢調査メッシュ2020_3"
  ) # 変数1のフォルダ指定
  # 読み込むファイルのベクトル
  dir_vec_3 <- fs::dir_ls(here::here(download_dir),
                          recurse = TRUE,
                          regexp = ".txt$")
  # データを読み込んで行結合していく
  df_3 <- read_delim(dir_vec_3[1], delim = ",",
                     locale = locale(encoding = "cp932"))
  for (i in 2:length(dir_vec_3)){
    # ひとつひとつデータの読み込み
    hoge_3 <- read_delim(dir_vec_3[i], delim = ",",
                       locale = locale(encoding = "cp932"))
    # 行を結合していく
    df_3 <- bind_rows(df_3, hoge_3) %>% distinct()
  }
  unlink(download_dir, recursive = T) # 使用済みのフォルダを削除
  
  # ========== データ4に関して ========== #
  download_dir <- paste0(
    formatC(pref_code_chr, width = 2, flag = "0"),
    "国勢調査メッシュ2020_4"
  ) # 変数1のフォルダ指定
  # 読み込むファイルのベクトル
  dir_vec_4 <- fs::dir_ls(here::here(download_dir),
                          recurse = TRUE,
                          regexp = ".txt$")
  # データを読み込んで行結合していく
  df_4 <- read_delim(dir_vec_4[1], delim = ",",
                     locale = locale(encoding = "cp932"))
  for (i in 2:length(dir_vec_4)){
    # ひとつひとつデータの読み込み
    hoge_4 <- read_delim(dir_vec_4[i], delim = ",",
                         locale = locale(encoding = "cp932"))
    # 行を結合していく
    df_4 <- bind_rows(df_4, hoge_4) %>% distinct()
  }
  unlink(download_dir, recursive = T) # 使用済みのフォルダを削除
  
  # 全部のデータを行結合
  df <- left_join(df_1, df_2, by = "KEY_CODE")
  df <- left_join(df, df_3, by = "KEY_CODE")
  df <- left_join(df, df_4, by = "KEY_CODE")
  return(df)
}
