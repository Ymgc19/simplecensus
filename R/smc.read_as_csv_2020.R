#' @title to read files
#' @description \code{frh.read_as_csv_2020}

smc.read_as_csv_2020 <- function(dir_folder){
  library(tidyverse)
  fs::dir_ls(here::here(dir_folder),
             recurse = TRUE,
             regexp = ".txt$") %>%
    purrr::set_names(
      fs::dir_ls(here::here(dir_folder)) %>%
        basename() %>%
        stringr::str_remove("（.+）")
    ) %>%
    purrr::map(
      \(x) read_delim(x, delim = ",",
                      locale = locale(encoding = "cp932"))
    ) %>%
    reduce(left_join, by = "KEY_CODE") %>%
    dplyr::select(-contains(".")) %>%
    dplyr::mutate(
      across(everything(), \(x) replace(x, x == "-", 0))
    )
}
