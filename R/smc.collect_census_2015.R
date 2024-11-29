#' @title to read files
#' @description \code{smc.collect_census_2015}

smc.collect_census_2015 <- function(pref_code){
  # pref_codeの調整
  library(utils)
  if (pref_code <= 9){
    pref_code <- as.character(paste("0", pref_code, sep = ""))
  }
  # urlを調整
  url1 <- "https://www.e-stat.go.jp/gis/statmap-search/data?statsId=T0008"
  url2 <- "&code="
  pref_code <- as.character(pref_code)
  url3 <- "&downloadType=2"

  # ディレクトリを作成
  download_dir <- paste(as.character(pref_code), "国勢調査2015", sep = "")
  if (!file.exists(download_dir)) {
    dir.create(download_dir)
  }
  # 指定された都道府県のデータをfor文でdownload
  zip_url <- c()  # zip_url ベクトルを初期化
  for (i in 1:9){
    num <- c(48, 49, 50, 51, 52, 53, 65, 66, 75)
    url4 <- paste0(url1, as.character(num[i]), url2, pref_code, url3)  # url を正しく生成
    zip_url <- c(zip_url, url4)  # zip_url ベクトルに追加
  }

  # for文でデータを全て読み込む
  for (url in zip_url) {
    filename <- paste0("tbl", substr(basename(url), 14, 20), "C", pref_code, ".zip")
    download.file(url, destfile = file.path(download_dir, filename), mode = "wb")
    unzip(file.path(download_dir, filename), exdir = download_dir)
    txt_files <- list.files(download_dir, pattern = ".txt", full.names = TRUE)
    file.remove(file.path(download_dir, filename))
  }
  return(download_dir)
}



