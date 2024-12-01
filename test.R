devtools::install_github("Ymgc19/simplecensus")
library(simplecensus)
library(tidyverse)

hoge <- smc.read_census_2020(17)
hoge %>% glimpse()

hoge <- smc.get_census_2020(17)
