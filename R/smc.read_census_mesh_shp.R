# shpのobjを出力する関数
#' @title to look at cute frogs!!!
#' @description \code{smc.read_census_mesh_shp}


smc.read_census_mesh_shp <- function(pref_code){
  library(tidyverse)
  smc.collect_mesh_shp(pref_code)
  if (pref_code <= 9){
    pref_code <- as.character(paste("0", pref_code, sep = ""))
  }
  else{
    pref_code <- pref_code
  }
  # ファイル名を指定
  folder_name <- paste0(pref_code, "census_mesh_shp")
  
  # ここでfolder_nameに含まれるshpをまとめて取得
  shp_to_read <- list.files(path = folder_name, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  # 一個ずつ読み込む
  shp <- read_sf(shp_to_read[1]) %>% 
    st_transform(crs = 4326)
  for (i in 2:length(shp_to_read)){
    hoge <- read_sf(shp_to_read[i]) %>% 
      st_transform(crs = 4326)
    shp <- bind_rows(shp, hoge)
  }
  shp <- shp %>%
    mutate(KEY_CODE = as.numeric(KEY_CODE))
  return(shp)
}
