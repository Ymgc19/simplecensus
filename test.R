devtools::install_github("Ymgc19/simplecensus")
library(simplecensus)
library(tidyverse)


#hoge <- smc.get_census_2020(17)
hoge <- simplecensus::smc.get_census_2020(17)

hoge %>% glimpse()
