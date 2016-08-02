rm(list = ls())

require(pacman)

pacman::p_load(
  readr, readxl,
  purrr, stringr, tidyr, dplyr, 
  rgeos,
  ggplot2, cowplot,
  tmap
)


# Load SIMD scores --------------------------------------------------------


simd_combined <- read_csv("data/simd/simd_combined.csv")


#Tasks 
# Visualise choropleth for 2001
# Visualise choropleth for 2011 
# Visualise cartogram for 2001
# Visualise cartogram for 2011 

# Visualise both boundary layers for 2001 and 2011 


# Load shapefiles  --------------------------------------------------------


# 2001 datazone shapefiles ------------------------------------------------


dz_2001 <- read_shape(file = "shapefiles/scotland_2001_datazones/scotland_dz_2001.shp")
# To visualise this
dz_2001 %>% 
  tm_shape(.) + 
  tm_borders() -> dz_2001_map  
# To see as interactive Leaflet display
#tmap_leaflet(dz_2001_map)
  
# And for 2011 datazones
dz_2011 <- read_shape(file = "shapefiles/SG_DataZoneBdry_2011/SG_DataZone_Bdry_2011.shp")
dz_2011 %>% 
  tm_shape(.) + 
  tm_borders( lty="dashed") -> dz_2011_map
# Interactive
#tmap_leaflet(dz_2011_map)


# Both combined
#tmap_leaflet(dz_2001_map + dz_2011_map)


# To show simd_scores 


simd_combined %>% 
  select(datazone, year, simd_score) %>%
  spread(year, simd_score) %>% 
  append_data(
    shp = dz_2001, data = . ,
    key.shp = "zonecode", key.data = "datazone"
              ) %>% 
  tm_shape(.) + 
  tm_facets(ncol = 4) + 
  tm_fill(col = c("2004", "2006", "2009", "2012")) -> simd_chor_dz2001


#   # Which of course says don't live in Glasgow...
# save_tmap(simd_chor_dz2001, filename = "maps/dz2001_simd_chor.png", width = 40, height = 15,
#           units = "cm", dpi = 300)


# # Create cartograms 

dz_2001_cart <- read_shape(
  file = "shapefiles/scot_2001_dz_2012_pop/dz_cartogram.shp", 
  current.projection = "longlat")

simd_combined %>%
  select(datazone, year, simd_score) %>%
  spread(year, simd_score) %>%
  append_data(
    shp = dz_2001_cart, data = . ,
    key.shp = "zonecode", key.data = "datazone"
  ) %>%
  tm_shape(.) +
  tm_facets(ncol = 4) +
  tm_fill(col = c("2004", "2006", "2009", "2012")) -> simd_cart_dz2001

save_tmap(simd_cart_dz2001, filename = "maps/dz2001_simd_cart.png", width = 40, height = 15,
          units = "cm", dpi = 300)



# As before, but using proportion of working age population income deprived 

simd_combined %>% 
  select(datazone,year, pop_workingage, pop_incomedeprived) %>% 
  mutate(prop_id = pop_incomedeprived/pop_workingage) %>% 
  select(datazone, year, prop_id) %>% 
  spread(key = year, value = prop_id) %>% 
  append_data(
    shp = dz_2001, data = . ,
    key.shp = "zonecode", key.data = "datazone"
  ) %>% 
  tm_shape(.) + 
  tm_facets(ncol = 4) +
  tm_fill(col = c("2004", "2006", "2009", "2012")) -> dz2001_id_chor

save_tmap(dz2001_id_chor, filename = "maps/dz2001_incomedeprived_chor.png", width = 40, height = 15,
          units = "cm", dpi = 300)

# Income deprived, cartogram

simd_combined %>%
  select(datazone,year, pop_workingage, pop_incomedeprived) %>%
  mutate(prop_id = pop_incomedeprived/pop_workingage) %>%
  select(datazone, year, prop_id) %>%
  spread(key = year, value = prop_id) %>%
  append_data(
    shp = dz_2001_cart, data = . ,
    key.shp = "zonecode", key.data = "datazone"
  ) %>%
  tm_shape(.) +
  tm_facets(ncol = 4) +
  tm_fill(col = c("2004", "2006", "2009", "2012")) -> dz2001_id_cart

save_tmap(dz2001_id_cart, filename = "maps/dz2001_incomedeprived_cart.png", width = 40, height = 15,
          units = "cm", dpi = 300)


# Maps of change from 2004 to 2012 , SIMD


simd_combined %>% 
  select(datazone, year, simd_score) %>%
  spread(year, simd_score) %>%
  mutate(dif = `2012` - `2004`) %>% 
  append_data(
    shp = dz_2001_cart, data = . ,
    key.shp = "zonecode", key.data = "datazone"
  ) %>% 
  tm_shape(.) + 
  tm_fill(col = "dif") -> simd_change_cart_dz2001

save_tmap(simd_change_cart_dz2001, filename = "maps/dz2001_simd_change_cart.png", height = 30, width = 15,
          units = "cm", dpi = 300)


# Maps of change from 2004 to 2012 , proportion income deprived


simd_combined %>% 
  select(datazone, year, pop_incomedeprived, pop_workingage) %>%
  mutate(prop_id = pop_incomedeprived/ pop_workingage) %>% 
  select(datazone, year, prop_id) %>% 
  spread(year, prop_id) %>%
  mutate(dif = `2012` - `2004`) %>% 
  append_data(
    shp = dz_2001_cart, data = . ,
    key.shp = "zonecode", key.data = "datazone"
  ) %>% 
  tm_shape(.) + 
  tm_fill(col = "dif") -> id_change_cart_dz2001

save_tmap(id_change_cart_dz2001, filename = "maps/dz2001_id_change_cart.png", height = 30, width = 15,
          units = "cm", dpi = 300)




# Definitions of city centres  --------------------------------------------



# 2001 centroids
ttwa_centroids <- c(
  Aberdeen = "S01000125",
  Glasgow = "S01003358",
  Edinburgh = "S01002131",
  Dundee = "S01001101",
  `Inverness and Dingwall` = "S01003853",
  `Perth and Blairgowrie` = "S01005037",
  `Stirling and Alloa` = "S01006120"
  )

# Add indicators of centres 
tmp <- dz_2001 %>% .@data
tmp$centre <-  NA 
tmp$centre[match(ttwa_centroids, tmp$zonecode)] <- names(ttwa_centroids)
tmp -> dz_2001@data
rm(tmp)


# Map this 

tm_shape(dz_2001) + 
  tm_polygons("grey", border.alpha = 0)  + 
  tm_bubbles( col = "centre",colorNA = NULL, showNA =F) -> centres_2001_bubble


# And for the cartogram

tmp <- dz_2001_cart %>% .@data
tmp$centre <- NA
tmp$centre[match(ttwa_centroids, tmp$zonecode)] <- names(ttwa_centroids)
tmp -> dz_2001_cart@data
rm(tmp)

# Map this 

tm_shape(dz_2001_cart) + 
  tm_polygons("grey", border.alpha = 0)  + 
  tm_bubbles( col = "centre",colorNA = NULL, showNA =F)

# # 2011 centroids
# ttwa_centroids <- c(
#   Aberdeen = "S01006646",
#   Glasgow = "S01010265",
#   Edinburgh = "S01008677",
#   Dundee = "S01007705",
#   Inverness = "S01010620",
#   Perth = "S01011939",
#   `Falkirk and Stirling` = "S01013067"
# )


# Distance from centres  --------------------------------------------------

# using rgeos::gCentroid

calc_distance_to_centres <- function(shp, code_centre){
  gCentroid(shp, byid = T) %>% 
    as(., "data.frame") -> tmp
    
  data_frame(dz = as.character(shp@data$zonecode), x = tmp$x, y = tmp$y) %>% 
  mutate(centre = dz == code_centre) %>% 
  mutate(distance_to_centre = ((x - x[centre])^2 + (y - y[centre])^2)^0.5) -> output
  output
}



# Classify which points are closest to which centres  ---------------------

ttwa_centroids <- c(
  Aberdeen = "S01000125",
  Glasgow = "S01003358",
  Edinburgh = "S01002131",
  Dundee = "S01001101",
  `Inverness and Dingwall` = "S01003853",
  `Perth and Blairgowrie` = "S01005037",
  `Stirling and Alloa` = "S01006120"
)

fn <- function(val, nm){
  dz_2001 %>% 
    calc_distance_to_centres(., val) %>% 
    .[c(1, 5)] -> out 
  names(out) <- c("datazone", nm)
  out
}

# Find nearest centre and distance to nearest centre
map2(ttwa_centroids, names(ttwa_centroids), fn) %>% 
  reduce(., inner_join) %>% 
  gather(place, distance, -datazone) %>%
  arrange(datazone) %>% 
  group_by(datazone) %>% 
  mutate(min_distance = min(distance)) %>%
  filter(distance == min_distance) %>% 
  ungroup() %>% 
  transmute(datazone, nearest_centre = place, distance_to_centre = distance) -> centre_distance

# Check this works - plot distance to nearest centre
dz_2001 %>% 
  append_data(., centre_distance, key.shp = "zonecode", key.data = "datazone", ignore.duplicates =T) %>% 
  tm_shape(.) + 
  tm_polygons(
    col = "distance_to_centre", 
    border.alpha = 0
  )

# plot nearest centre
dz_2001 %>% 
  append_data(., centre_distance, key.shp = "zonecode", key.data = "datazone", ignore.duplicates = T) %>% 
  tm_shape(.) + 
  tm_polygons(col = "nearest_centre", border.alpha = 0) -> dz2001_nearest_centre


save_tmap(dz2001_nearest_centre, filename = "maps/dz2001_nearest_centre.png", height = 30, width = 15,
          units = "cm", dpi = 300)


# Change in proportion income deprived compared with distance from city centre
# for those datazones whose centres are closer to Glasgow than any other centre 
simd_combined  %>% 
  left_join(centre_distance) %>% 
  filter(nearest_centre == "Glasgow") %>%
  distinct() %>% 
  mutate(prop_id = pop_incomedeprived / pop_workingage) %>% 
  select(datazone, distance_to_centre, year, prop_id) %>% 
  spread(year, prop_id) %>% 
  mutate(change = `2012` - `2004`) %>% 
  mutate(distance_to_centre = distance_to_centre / 1000) %>% 
  ggplot(., aes(x = distance_to_centre, y = change)) + 
  geom_point( alpha = 0.1) + 
  scale_x_log10(breaks = c(0.5, 1, 2, 5, 10, 20, 50, 100)) + 
  scale_y_continuous(limits = c(-1.0, 0.5) ) +
  stat_smooth() + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  geom_vline(aes(xintercept = 1e+0)) + 
  geom_vline(aes(xintercept = 1.2e+1)) + 
  labs(x = "Distance to centre (km)", y = "Change in income deprived proportion",
     title = "Glasgow") -> id_change_glasgow_dz2001


# As above, but for Edinburgh
simd_combined  %>% left_join(centre_distance) %>% 
  filter(nearest_centre == "Edinburgh") %>%
  distinct() %>% 
  mutate(prop_id = pop_incomedeprived / pop_workingage) %>% 
  select(datazone, distance_to_centre, year, prop_id) %>% 
  mutate(distance_to_centre = distance_to_centre / 1000) %>% 
  spread(year, prop_id) %>% 
  mutate(change = `2012` - `2004`) %>% 
  ggplot(., aes(x = distance_to_centre, y = change)) + 
  geom_point( alpha = 0.1) + 
  scale_x_log10(breaks = c(0.5, 1, 2, 5, 10, 20, 50, 100)) + 
  scale_y_continuous(limits = c(-1.0, 0.5)) +
  stat_smooth() + 
  geom_vline(aes(xintercept = 1e+0)) + 
  geom_vline(aes(xintercept = 1.2e+1)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  labs(x = "Distance to centre (km)", y = "Change in income deprived proportion",
       title = "Edinburgh") -> id_change_edinburgh_dz2001

# Aberdeen
simd_combined  %>% 
  left_join(centre_distance) %>% 
  filter(nearest_centre == "Aberdeen") %>%
  distinct() %>% 
  mutate(prop_id = pop_incomedeprived / pop_workingage) %>% 
  mutate(distance_to_centre = distance_to_centre / 1000) %>% 
  select(datazone, distance_to_centre, year, prop_id) %>% 
  spread(year, prop_id) %>% 
  mutate(change = `2012` - `2004`) %>% 
  ggplot(., aes(x = distance_to_centre, y = change)) + 
  geom_point( alpha = 0.1) + 
  scale_x_log10(breaks = c(0.5, 1, 2, 5, 10, 20, 50, 100)) + 
  scale_y_continuous(limits = c(-1.0, 0.5)) +
  stat_smooth() + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  geom_vline(aes(xintercept = 1e+0)) + 
  geom_vline(aes(xintercept = 1.2e+1)) + 
  labs(x = "Distance to centre (km)", y = "Change in income deprived proportion",
       title = "Aberdeen") -> id_change_aberdeen_dz2001


# Dundee 
simd_combined  %>% 
  left_join(centre_distance) %>% 
  filter(nearest_centre == "Dundee") %>%
  distinct() %>% 
  mutate(prop_id = pop_incomedeprived / pop_workingage) %>% 
  mutate(distance_to_centre = distance_to_centre / 1000) %>% 
  select(datazone, distance_to_centre, year, prop_id) %>% 
  spread(year, prop_id) %>% 
  mutate(change = `2012` - `2004`) %>% 
  ggplot(., aes(x = distance_to_centre, y = change)) + 
  geom_point( alpha = 0.1) + 
  scale_x_log10(breaks = c(0.5, 1, 2, 5, 10, 20, 50, 100)) + 
  scale_y_continuous(limits = c(-1.0, 0.5)) +
  stat_smooth() + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  geom_vline(aes(xintercept = 1e+0)) + 
  geom_vline(aes(xintercept = 1.2e+1)) + 
  labs(x = "Distance to centre (km)", y = "Change in income deprived proportion",
       title = "Dundee") -> id_change_dundee_dz2001

# Inverness 
simd_combined  %>% 
  left_join(centre_distance) %>% 
  filter(nearest_centre == "Inverness and Dingwall") %>%
  distinct() %>% 
  mutate(prop_id = pop_incomedeprived / pop_workingage) %>% 
  mutate(distance_to_centre = distance_to_centre / 1000) %>% 
  select(datazone, distance_to_centre, year, prop_id) %>% 
  spread(year, prop_id) %>% 
  mutate(change = `2012` - `2004`) %>% 
  ggplot(., aes(x = distance_to_centre, y = change)) + 
  geom_point( alpha = 0.1) + 
  scale_x_log10(breaks = c(0.5, 1, 2, 5, 10, 20, 50, 100)) + 
  scale_y_continuous(limits = c(-1.0, 0.5)) +
  stat_smooth() + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  geom_vline(aes(xintercept = 1e+0)) + 
  geom_vline(aes(xintercept = 1.2e+1)) + 
  labs(x = "Distance to centre (km)", y = "Change in income deprived proportion",
       title = "Inverness") -> id_change_inverness_dz2001

# Create Cowplot figure showing all of these 

plot_grid(id_change_aberdeen_dz2001, id_change_dundee_dz2001, id_change_edinburgh_dz2001, id_change_glasgow_dz2001 )


                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 