# Explore IMD by city 

rm(list = ls())

require(pacman)

pacman::p_load(
  readr, readxl,
  purrr, stringr, tidyr, dplyr,
  broom,
  rgeos,
  ggplot2, cowplot,
  tmap, RColorBrewer
)

# dist to centre and area 
dist_to_centre <- read_csv("data/lsoa_2011_by_dist_to_centres.csv")

# imd  on 2011 
dta <- read_csv("data/imd/imd_id_tidied.csv", col_types = "icdd")

#shapefiles 

lsoa_2011 <- read_shape(file = "shapefiles/Lower_layer_super_output_areas_(E+W)_2011_Boundaries_(Full_Clipped)_V2/LSOA_2011_EW_BFC_V2.shp")



# Start w / Bristol then iterate to others 


dta %>% 
  select(lsoa, year, pop_id, pop_total) %>%
  mutate(prop_id = pop_id / pop_total) %>% 
#  mutate(prop_id = ifelse(prop_id > 0.5, 0.5, prop_id)) %>% 
  select(lsoa, year, prop_id) %>% 
  left_join(dist_to_centre) %>% 
  filter(place == "Bristol") %>% 
  select(lsoa, year, prop_id) %>% 
  spread(year, prop_id) %>% 
  append_data(
    shp = lsoa_2011, data = . ,
    key.shp = "LSOA11CD", key.data = "lsoa", ignore.na = T
  ) -> shp_jn 

shp_jn <- shp_jn[!is.na(shp_jn$`2004`), ]
shp_jn %>% 
  tm_shape(.) + 
  tm_facets(ncol = 2) + 
  tm_fill(
    col = c("2004", "2007", "2010", "2015"), 
    palette = brewer.pal(10, "Spectral"), 
    breaks = seq(0, 0.5, by = 0.05), showNA = F
  ) +
  tm_legend(legend.show = F) -> id_chor_lsoa2011_bristol

id_chor_lsoa2011_bristol

dta %>% 
  select(lsoa, year, pop_id, pop_total) %>%
  mutate(prop_id = pop_id / pop_total) %>% 
  #  mutate(prop_id = ifelse(prop_id > 0.5, 0.5, prop_id)) %>% 
  select(lsoa, year, prop_id) %>% 
  left_join(dist_to_centre) %>% 
  filter(place == "Manchester") %>% 
  select(lsoa, year, prop_id) %>% 
  spread(year, prop_id) %>% 
  append_data(
    shp = lsoa_2011, data = . ,
    key.shp = "LSOA11CD", key.data = "lsoa", ignore.na = T
  ) -> shp_jn 

shp_jn <- shp_jn[!is.na(shp_jn$`2004`), ]
shp_jn %>% 
  tm_shape(.) + 
  tm_facets(ncol = 2) + 
  tm_fill(
    col = c("2004", "2007", "2010", "2015"), 
    palette = brewer.pal(10, "Spectral"), 
    breaks = seq(0, 0.5, by = 0.05), showNA = F
  ) +
  tm_legend(legend.show = F) -> id_chor_lsoa2011_manchester

id_chor_lsoa2011_manchester


dta %>% 
  select(lsoa, year, pop_id, pop_total) %>%
  mutate(prop_id = pop_id / pop_total) %>% 
  #  mutate(prop_id = ifelse(prop_id > 0.5, 0.5, prop_id)) %>% 
  select(lsoa, year, prop_id) %>% 
  left_join(dist_to_centre) %>% 
  filter(place == "London") %>% 
  select(lsoa, year, prop_id) %>% 
  spread(year, prop_id) %>% 
  append_data(
    shp = lsoa_2011, data = . ,
    key.shp = "LSOA11CD", key.data = "lsoa", ignore.na = T
  ) -> shp_jn 

shp_jn <- shp_jn[!is.na(shp_jn$`2004`), ]
shp_jn %>% 
  tm_shape(.) + 
  tm_facets(ncol = 2) + 
  tm_fill(
    col = c("2004", "2007", "2010", "2015"), 
    palette = brewer.pal(10, "Spectral"), 
    breaks = seq(0, 0.5, by = 0.05), showNA = F
  ) +
  tm_legend(legend.show = F) -> id_chor_lsoa2011_london

id_chor_lsoa2011_london


# Thes indicate that a sizeable proportion of lsoas are not matched effectively. 
# These seem to be the places w/ n -> n matching (category X). Though may 
# be something separate. 