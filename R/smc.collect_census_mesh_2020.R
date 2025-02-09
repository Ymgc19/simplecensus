#' @title to read files
#' @description \code{smc.collect_census_mesh_2020}


smc.collect_census_mesh_2020 <- function(pref_code){
  # pref_codeの調整
  library(utils)
  if (pref_code <= 9){
    pref_code <- as.character(paste("0", pref_code, sep = ""))
  }
  library(tidyverse)
  
  # メッシュコードを指定
  # 北海道
  p01 <- c(6239, 6240, 6241, 6243, 6339, 6340, 6341, 6342, 6343, 6439,
           6440, 6441, 6442, 6443, 6444, 6445, 6540, 6541, 6542, 6543,
           6544, 6545, 6641, 6642, 6643, 6644, 6645, 6741, 6742, 6840,
           6841, 6842)
  # 青森
  p02 <- c(6240, 6241, 6139, 6140, 6141, 6039, 6040, 6041)
  # 岩手
  p03 <- c(6040, 6041, 5940, 5941, 5942, 5840, 5841)
  # 宮城
  p04 <- c(5840, 5841, 5740, 5741, 5640)
  # 秋田
  p05 <- c(6039, 6040, 5939, 5940, 5839, 5840)
  # 山形
  p06 <- c(5839, 5840, 5739, 5740, 5639, 5640)
  # 福島
  p07 <- c(5539, 5540, 5541, 5639, 5640, 5641)
  # 茨城
  p08 <- c(5540, 5439, 5440, 5339, 5340)
  # 栃木
  p09 <- c(5539, 5540, 5439, 5440)
  # 群馬
  p10 <- c(5538, 5539, 5438, 5439, 5338)
  # 埼玉
  p11 <- c(5438, 5439, 5338, 5339)
  # 千葉
  p12 <- c(5439, 5339, 5340, 5239, 5240)
  # 東京
  p13 <- c(3036, 3641, 3653, 3741, 3841, 3942, 4040, 4042, 4142, 4440,
           4540, 4739, 4740, 4839, 4939, 5038, 5039, 5139, 5239, 5338,
           5339)
  # 神奈川
  p14 <- c(5338, 5339, 5238, 5239)
  # 新潟
  p15 <- c(5537, 5538, 5539, 5638, 5639, 5738, 5739)
  # 富山
  p16 <- c(5536, 5537, 5436, 5437)
  # 石川
  p17 <- c(5636, 5637, 5536, 5537, 5436)
  # 福井
  p18 <- c(5435, 5436, 5335, 5336)
  # 山梨
  p19 <- c(5238, 5338, 5339)
  # 長野
  p20 <- c(5237, 5238, 5337, 5338, 5437, 5438, 5537, 5538)
  # 岐阜
  p21 <- c(5236, 5237, 5336, 5337, 5436, 5437)
  # 静岡
  p22 <- c(5137, 5138, 5237, 5238, 5239, 5338, 5339)
  # 愛知
  p23 <- c(5136, 5137, 5236, 5237, 5336, 5337)
  # 三重
  p24 <- c(5035, 5036, 5135, 5136, 5236)
  # 滋賀
  p25 <- c(5335, 5336, 5235, 5236)
  # 京都
  p26 <- c(5334, 5335, 5234, 5235, 5236)
  # 大阪
  p27 <- c(5235, 5135)
  # 兵庫
  p28 <- c(5134, 5135, 5234, 5235, 5334, 5335)
  # 奈良
  p29 <- c(5035, 5135, 5136, 5235, 5236)
  # 和歌山
  p30 <- c(5035, 5036, 5134, 5135, 5136)
  # 鳥取
  p31 <- c(5233, 5234, 5333, 5334)
  # 島根
  p32 <- c(5131, 5132, 5231, 5232, 5233, 5332, 5333, 5432, 5433, 5531)
  # 岡山
  p33 <- c(5133, 5134, 5233, 5234, 5333, 5334)
  # 広島
  p34 <- c(5132, 5133, 5232, 5233)
  # 山口
  p35 <- c(5030, 5031, 5032, 5130, 5131, 5132, 5231)
  # 徳島
  p36 <- c(5033, 5034, 5133, 5134)
  # 香川
  p37 <- c(5133, 5134)
  # 愛媛
  p38 <- c(4932, 5032, 5033, 5132, 5133)
  # 高知
  p39 <- c(4932, 4933, 4934, 5032, 5033, 5034)
  # 福岡
  p40 <- c(4930, 5030, 5031, 5130)
  # 佐賀
  p41 <- c(4929, 4930, 5029, 5030)
  # 長崎
  p42 <- c(4728, 4828, 4829, 4830, 4928, 4929, 4930, 5029, 5129, 5229)
  # 熊本
  p43 <- c(4829, 4830, 4831, 4930, 4931)
  # 大分
  p44 <- c(4930, 4931, 4932, 5030, 5031)
  # 宮崎
  p45 <- c(4931, 4830, 4831, 4730, 4731)
  # 鹿児島
  p46 <- c(4028, 4128, 4129, 4229, 4230, 4328, 4329, 4429, 4529, 4530,
           4531, 4629, 4630, 4631, 4729, 4730, 4731, 4830)
  # 沖縄
  p47 <- c(3622,3623,3624,3631,3724,3725,3823,3824,3831,3926,
           3927,3928,4027,4028,4128)
  # 便宜的にリスト化
  mesh_code_list <- list(p01, p02, p03, p04, p05, p06, p07, p08, p09, p10,
                         p11, p12, p13, p14, p15, p16, p17, p18, p19, p20,
                         p21, p22, p23, p24, p25, p26, p27, p28, p29, p30,
                         p31, p32, p33, p34, p35, p36, p37, p38, p39, p40,
                         p41, p42, p43, p44, p45, p46, p47)
  
  # ダウンロードするメッシュのコードのリストを作成
  
  # urlを調整
  # 250m　その1
  url1_1 <- "https://www.e-stat.go.jp/gis/statmap-search/data?statsId=T001102&code="
  url1_2 <- "&downloadType=2"
  # 250m その2
  url2_1 <- "https://www.e-stat.go.jp/gis/statmap-search/data?statsId=T001109&code="
  url2_2 <- "&downloadType=2"
  # 250m　その3
  url3_1 <- "https://www.e-stat.go.jp/gis/statmap-search/data?statsId=T001142&code="
  url3_2 <- "&downloadType=2"
  # 250m　その4
  url4_1 <- "https://www.e-stat.go.jp/gis/statmap-search/data?statsId=T001145&code="
  url4_2 <- "&downloadType=2"
  
  # ダウンロードするurlのリストを作成
  zip_url_1 <- c()
  zip_url_2 <- c()
  zip_url_3 <- c()
  zip_url_4 <- c()
  for (i in mesh_code_list[[pref_code]]){
    zip_url_1 <- c(zip_url_1, paste0(url1_1, i, url1_2))
    zip_url_2 <- c(zip_url_2, paste0(url2_1, i, url2_2))
    zip_url_3 <- c(zip_url_3, paste0(url3_1, i, url3_2))
    zip_url_4 <- c(zip_url_4, paste0(url4_1, i, url4_2))
  }
  
  # ========== 変数1 ダウンロード ========== #
  # ディレクトリを作成
  download_dir_1 <- paste(as.character(pref_code), "国勢調査メッシュ2020_1", sep = "")
  if (!file.exists(download_dir_1)) {
    dir.create(download_dir_1)
  }
  # 指定された都道府県のデータをfor文でdownload
  # for文でデータを全て読み込む
  for (url in zip_url_1) {
    filename <- paste0("tbl", substr(basename(url), 14, 20), "C", pref_code, ".zip")
    download.file(url, destfile = file.path(download_dir_1, filename), mode = "wb")
    unzip(file.path(download_dir_1, filename), exdir = download_dir_1)
    txt_files <- list.files(download_dir_1, pattern = ".txt", full.names = TRUE)
    file.remove(file.path(download_dir_1, filename))
  }
  
  # ========== 変数2 ダウンロード ========== #
  # ディレクトリを作成
  download_dir_2 <- paste(as.character(pref_code), "国勢調査メッシュ2020_2", sep = "")
  if (!file.exists(download_dir_2)) {
    dir.create(download_dir_2)
  }
  # 指定された都道府県のデータをfor文でdownload
  # for文でデータを全て読み込む
  for (url in zip_url_2) {
    filename <- paste0("tbl", substr(basename(url), 14, 20), "C", pref_code, ".zip")
    download.file(url, destfile = file.path(download_dir_2, filename), mode = "wb")
    unzip(file.path(download_dir_2, filename), exdir = download_dir_2)
    txt_files <- list.files(download_dir_2, pattern = ".txt", full.names = TRUE)
    file.remove(file.path(download_dir_2, filename))
  }
  
  # ========== 変数3 ダウンロード ========== #
  # ディレクトリを作成
  download_dir_3 <- paste(as.character(pref_code), "国勢調査メッシュ2020_3", sep = "")
  if (!file.exists(download_dir_3)) {
    dir.create(download_dir_3)
  }
  # 指定された都道府県のデータをfor文でdownload
  # for文でデータを全て読み込む
  for (url in zip_url_3) {
    filename <- paste0("tbl", substr(basename(url), 14, 20), "C", pref_code, ".zip")
    download.file(url, destfile = file.path(download_dir_3, filename), mode = "wb")
    unzip(file.path(download_dir_3, filename), exdir = download_dir_3)
    txt_files <- list.files(download_dir_3, pattern = ".txt", full.names = TRUE)
    file.remove(file.path(download_dir_3, filename))
  }
  
  # ========== 変数4 ダウンロード ========== #
  # ディレクトリを作成
  download_dir_4 <- paste(as.character(pref_code), "国勢調査メッシュ2020_4", sep = "")
  if (!file.exists(download_dir_4)) {
    dir.create(download_dir_4)
  }
  # 指定された都道府県のデータをfor文でdownload
  # for文でデータを全て読み込む
  for (url in zip_url_4) {
    filename <- paste0("tbl", substr(basename(url), 14, 20), "C", pref_code, ".zip")
    download.file(url, destfile = file.path(download_dir_3, filename), mode = "wb")
    unzip(file.path(download_dir_3, filename), exdir = download_dir_3)
    txt_files <- list.files(download_dir_3, pattern = ".txt", full.names = TRUE)
    file.remove(file.path(download_dir_3, filename))
  }
}
