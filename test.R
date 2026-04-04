devtools::install_github("Ymgc19/simplecensus")
library(simplecensus)
library(tidyverse)


#hoge <- smc.get_census_2020(17)
hoge <- simplecensus::smc.get_census_2020(17) 

hoge %>% glimpse()



hoge <- smc.read_census_mesh_2015(1)
smc.collect_census_mesh_2015(1)








m <- simplecensus::smc.read_census_2020(4)
m <- m %>% mutate(KEY_CODE = as.numeric(KEY_CODE))
s <- simplecensus::smc.read_census_shp(4)
s <- s %>% mutate(KEY_CODE = as.numeric(KEY_CODE))

ms <- left_join(s, m)
ms <- ms %>% 
  simplecensus::smc.create_variables_2020() %>% 
  simplecensus::smc.convert_percentage_vars_2020()

ms %>% ggplot() +
  geom_sf(
    aes(fill = smc.per.population0_4), color = NA
  ) +
  scale_fill_gradient(
    low = "turquoise", high = "tomato"
  )

#ms$smc.per.unemployed %>% hist
