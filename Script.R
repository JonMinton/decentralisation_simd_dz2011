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
    border.alpha = 0.1
  )

# plot nearest centre
dz_2001 %>% 
  append_data(., centre_distance, key.shp = "zonecode", key.data = "datazone", ignore.duplicates = T) %>% 
  tm_shape(.) + 
  tm_polygons(col = "nearest_centre", border.alpha = 0)



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
  ggplot(., aes(x = distance_to_centre, y = change)) + 
  geom_point( alpha = 0.1) + 
  scale_x_log10() + 
  scale_y_continuous(limits = c(-1.0, 0.5)) +
  stat_smooth() + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  geom_vline(aes(xintercept = 1e+3)) + 
  geom_vline(aes(xintercept = 1.2e+4)) + 
  labs(x = "Distance to centre (m?)", y = "Change in income deprived proportion",
     title = "Glasgow")


# As above, but for Edinburgh
simd_combined  %>% left_join(centre_distance) %>% 
  filter(nearest_centre == "Edinburgh") %>%
  distinct() %>% 
  mutate(prop_id = pop_incomedeprived / pop_workingage) %>% 
  select(datazone, distance_to_centre, year, prop_id) %>% 
  spread(year, prop_id) %>% 
  mutate(change = `2012` - `2004`) %>% 
  ggplot(., aes(x = distance_to_centre, y = change)) + 
  geom_point( alpha = 0.1) + 
  scale_x_log10() + 
  scale_y_continuous(limits = c(-1.0, 0.5)) +
  stat_smooth() + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  labs(x = "Distance to centre (m?)", y = "Change in income deprived proportion",
       title = "Edinburgh")

# Aberdeen
simd_combined  %>% 
  left_join(centre_distance) %>% 
  filter(nearest_centre == "Aberdeen") %>%
  distinct() %>% 
  mutate(prop_id = pop_incomedeprived / pop_workingage) %>% 
  select(datazone, distance_to_centre, year, prop_id) %>% 
  spread(year, prop_id) %>% 
  mutate(change = `2012` - `2004`) %>% 
  ggplot(., aes(x = distance_to_centre, y = change)) + 
  geom_point( alpha = 0.1) + 
  scale_x_log10() + 
  scale_y_continuous(limits = c(-1.0, 0.5)) +
  stat_smooth() + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  geom_vline(aes(xintercept = 1e+3)) + 
  geom_vline(aes(xintercept = 1.2e+4)) + 
  labs(x = "Distance to centre (m?)", y = "Change in income deprived proportion",
       title = "Aberdeen")


# Dundee 
simd_combined  %>% 
  left_join(centre_distance) %>% 
  filter(nearest_centre == "Dundee") %>%
  distinct() %>% 
  mutate(prop_id = pop_incomedeprived / pop_workingage) %>% 
  select(datazone, distance_to_centre, year, prop_id) %>% 
  spread(year, prop_id) %>% 
  mutate(change = `2012` - `2004`) %>% 
  ggplot(., aes(x = distance_to_centre, y = change)) + 
  geom_point( alpha = 0.1) + 
  scale_x_log10() + 
  scale_y_continuous(limits = c(-1.0, 0.5)) +
  stat_smooth() + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  geom_vline(aes(xintercept = 1e+3)) + 
  geom_vline(aes(xintercept = 1.2e+4)) + 
  labs(x = "Distance to centre (m?)", y = "Change in income deprived proportion",
       title = "Dundee")


# 2011 Datazones ----------------------------------------------------------

lkup <-  read_csv("data/paul_norman_file/paul_norman_dz2011_table.csv")

lkup  %>% 
  select(dz_2001, dz_2011)  %>% 
  arrange(dz_2001, dz_2011)  %>%  
  group_by(dz_2001, dz_2011)  %>% 
  tally  %>% # produce n, giving number of OAs which contain particular groupings of dz_2001 and dz_2011
  arrange(dz_2011, dz_2001)  %>% 
  select(dz_2011, dz_2001, n)  %>% 
  group_by(dz_2011)   %>% 
  arrange(dz_2011, dz_2001) %>% 
  mutate(weight = n / sum(n))   %>% # Weighting by dz_2011
  left_join(simd_combined, by = c("dz_2001" = "datazone"))  %>% 
  select(-simd_rank)  %>%  # Not meaningful to reweight rank
  group_by(dz_2011, year) %>% 
  summarise_each( ~ sum(. * weight), 6:9) -> simd_2011_reweighted



dz_2011 <- read_shape(file = "shapefiles/SG_DataZoneBdry_2011/SG_DataZone_Bdry_2011.shp")

#qtm(dz_2011_shp)

# 2011 centroids
ttwa_centroids <- c(
  Aberdeen = "S01006646",
  Glasgow = "S01010265",
  Edinburgh = "S01008677",
  Dundee = "S01007705",
  Inverness = "S01010620",
  Perth = "S01011939",
  `Falkirk and Stirling` = "S01013067"
)

# using rgeos::gCentroid

calc_distance_to_centres <- function(shp, code_centre){
  gCentroid(shp, byid = T) %>% 
    as(., "data.frame") -> tmp
  
  data_frame(dz = as.character(shp@data$DataZone), x = tmp$x, y = tmp$y) %>% 
    mutate(centre = dz == code_centre) %>% 
    mutate(distance_to_centre = ((x - x[centre])^2 + (y - y[centre])^2)^0.5) -> output
  output
}


fn <- function(val, nm){
  dz_2011 %>% 
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
dz_2011 %>% 
  append_data(., centre_distance, key.shp = "DataZone", key.data = "datazone", ignore.duplicates =T) %>% 
  tm_shape(.) + 
  tm_polygons(
    col = "distance_to_centre", 
    border.alpha = 0.1
  )

# plot nearest centre
dz_2011 %>% 
  append_data(., centre_distance, key.shp = "DataZone", key.data = "datazone", ignore.duplicates = T) %>% 
  tm_shape(.) + 
  tm_polygons(col = "nearest_centre", border.alpha = 0)




# Change in proportion income deprived compared with distance from city centre
# for those datazones whose centres are closer to Glasgow than any other centre 
simd_2011_reweighted  %>% ungroup() %>%  
  left_join(centre_distance, by = c("dz_2011" = "datazone")) %>% 
  filter(nearest_centre == "Glasgow") %>%
  distinct() %>% 
  mutate(prop_id = pop_incomedeprived / pop_workingage) %>% 
  select(dz_2011, distance_to_centre, year, prop_id) %>% 
  spread(year, prop_id) %>% 
  mutate(change = `2012` - `2004`) %>% 
  ggplot(., aes(x = distance_to_centre, y = change)) + 
  geom_point( alpha = 0.1) + 
  scale_x_log10() + 
  scale_y_continuous(limits = c(-1.0, 0.5)) +
  stat_smooth() + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  geom_vline(aes(xintercept = 1e+3)) + 
  geom_vline(aes(xintercept = 1.2e+4)) + 
  labs(x = "Distance to centre (m?)", y = "Change in income deprived proportion",
       title = "Glasgow")


# As above, but for Edinburgh
simd_2011_reweighted  %>% ungroup() %>%  
  left_join(centre_distance, by = c("dz_2011" = "datazone")) %>% 
  filter(nearest_centre == "Edinburgh") %>%
  distinct() %>% 
  mutate(prop_id = pop_incomedeprived / pop_workingage) %>% 
  select(dz_2011, distance_to_centre, year, prop_id) %>% 
  spread(year, prop_id) %>% 
  mutate(change = `2012` - `2004`) %>% 
  ggplot(., aes(x = distance_to_centre, y = change)) + 
  geom_point( alpha = 0.1) + 
  scale_x_log10() + 
  scale_y_continuous(limits = c(-1.0, 0.5)) +
  stat_smooth() + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  geom_vline(aes(xintercept = 1e+3)) + 
  geom_vline(aes(xintercept = 1.2e+4)) + 
  labs(x = "Distance to centre (m?)", y = "Change in income deprived proportion",
       title = "Edinburgh")
# Aberdeen
simd_2011_reweighted  %>% ungroup() %>%  
  left_join(centre_distance, by = c("dz_2011" = "datazone")) %>% 
  filter(nearest_centre == "Aberdeen") %>%
  distinct() %>% 
  mutate(prop_id = pop_incomedeprived / pop_workingage) %>% 
  select(dz_2011, distance_to_centre, year, prop_id) %>% 
  spread(year, prop_id) %>% 
  mutate(change = `2012` - `2004`) %>% 
  ggplot(., aes(x = distance_to_centre, y = change)) + 
  geom_point( alpha = 0.1) + 
  scale_x_log10() + 
  scale_y_continuous(limits = c(-1.0, 0.5)) +
  stat_smooth() + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  geom_vline(aes(xintercept = 1e+3)) + 
  geom_vline(aes(xintercept = 1.2e+4)) + 
  labs(x = "Distance to centre (m?)", y = "Change in income deprived proportion",
       title = "Aberdeen")


# Dundee 
simd_2011_reweighted  %>% ungroup() %>%  
  left_join(centre_distance, by = c("dz_2011" = "datazone")) %>% 
  filter(nearest_centre == "Dundee") %>%
  distinct() %>% 
  mutate(prop_id = pop_incomedeprived / pop_workingage) %>% 
  select(dz_2011, distance_to_centre, year, prop_id) %>% 
  spread(year, prop_id) %>% 
  mutate(change = `2012` - `2004`) %>% 
  ggplot(., aes(x = distance_to_centre, y = change)) + 
  geom_point( alpha = 0.1) + 
  scale_x_log10() + 
  scale_y_continuous(limits = c(-1.0, 0.5)) +
  stat_smooth() + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  geom_vline(aes(xintercept = 1e+3)) + 
  geom_vline(aes(xintercept = 1.2e+4)) + 
  labs(x = "Distance to centre (m?)", y = "Change in income deprived proportion",
       title = "Dundee")

# Inverness 
simd_2011_reweighted  %>% ungroup() %>%  
  left_join(centre_distance, by = c("dz_2011" = "datazone")) %>% 
  filter(nearest_centre == "Inverness") %>%
  distinct() %>% 
  mutate(prop_id = pop_incomedeprived / pop_workingage) %>% 
  select(dz_2011, distance_to_centre, year, prop_id) %>% 
  spread(year, prop_id) %>% 
  mutate(change = `2012` - `2004`) %>% 
  ggplot(., aes(x = distance_to_centre, y = change)) + 
  geom_point( alpha = 0.1) + 
  scale_x_log10() + 
  scale_y_continuous(limits = c(-1.0, 0.5)) +
  stat_smooth() + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  geom_vline(aes(xintercept = 1e+3)) + 
  geom_vline(aes(xintercept = 1.2e+4)) + 
  labs(x = "Distance to centre (m?)", y = "Change in income deprived proportion",
       title = "Inverness")


# 
# # 
# # Gwilym/Jon/Gavin (cc. Mark/Mirjam)
# # 
# # I'd be keen to plan a timetable for this work, and agreeing how we will try to use the data, particularly to 
# look at the question of suburbanisation of poverty. I don't think it is a huge amount of effort and it would be 
# good to have it set up so the full code is available through GitHub so others are able to follow and comment on 
# the process. We'd also try to make the resulting data available through UBDC. Have copied in Mark and Mirjam since 
# they may be interested in contributing or commenting on method. Steps would be as follows:
# # 
# # June/July
# # 1. Get the existing SIMD data for DZ-2001 from 2004 - should have this from the pollution paper work but need to 
#   make sure that, for each year, we have: population, SIMD score and rank, and Income Deprivation %.

# # 
# # 2. Using the old DZ boundaries, run the decentralisation code on the Income Deprived for the four cities for each year, 
# and check whether we see similar pattern to the suburbanisation paper. This also acts as a baseline against which we 
# can check the re-aggregated data.

# # 
# # 3. Agree a procedure for making our 'best' estimate of overall SIMD score/rank, and Income Deprivation, on new DZ-2011 
# boundaries. We can do better than simply looking at the nearest old DZ for each new DZ centroid by using information about 
# variations within each DZ, utilising Census data at OA level. We have: deprivation scores for OAs in 2001 (Mirjam Allik's 
# paper); and a lookup file showing how the population of each OA-2001 is apportioned to each DZ-2011 (Paul Norman's file). 
# The approach would be:
# a. use OA-2001 deprivation decile to apportion DZ-2001 deprivation to the OA-2001;
# b. use Norman's file to re-aggregate to DZ-2011.
# 
# 4. Re-run the decentralisation code using the estimates of Income Deprivation for the new DZ boundaries and compare with 
# 2. To do this, we need to be able to identify the four cities in terms of the DZ-2011 boundaries.
# 
# Early August
# 5. Finally, on the day that the SIMD2016 are released, run the decentralisation code on that data and also produce simple 
# maps showing absolute changes. Put out a short summary.
# 
# Does this sound like a plan?
# 
# Regards
# 
# Nick
#                                 

# 
# From: Paul Norman [mailto:P.D.Norman@leeds.ac.uk] 
# Sent: 18 May 2016 16:29
# To: Mark Livingston
# Cc: Mirjam Allik; Nick Bailey
# Subject: RE: 2001 (and earlier) output areas to 2011 datazones
# 
# Here are the files …
# 
# From: Paul Norman 
# Sent: 18 May 2016 16:25
# To: 'Mark Livingston' <Mark.Livingston@glasgow.ac.uk>
#   Cc: Mirjam Allik <Mirjam.Allik@glasgow.ac.uk>; Nick Bailey <Nick.Bailey@glasgow.ac.uk>
#   Subject: RE: 2001 (and earlier) output areas to 2011 datazones
# 
# Hi Mark & colleagues
# 
# Yep, sure. Here you go. In the correspondence to Alistair I said that I had wanted to delve more into tenure 
# for a revised Townsend over time but haven’t!
#   
#   The conversion tables operate the same way as stepped through in an Excel Northern Ireland spreadsheet version 
#  where this example is 2011 LGDs to the later fewer 2014(?) LGDs. Much of this then is just aggregation but where 
# the weights are less than 1 the source data is apportioned to different LGDs.
# 
# Recipe:
#   Join the original census ‘source’ data to the GCT
# Multiple variables by the weights
# Sum across the ‘target’ zones
# 
# In the Scotland conversions, the older to recent information involves more estimation of locations than the 
# newer stuff. From memory, I think some 71 EDs in the islands struggle because the geocoding and coastline 
# digitisation and differences over time will be somewhat iffy. Obviously if you spot something weird that compromises 
# please shout!
#   
#   Let me know how you get on and if ever you want text to explain for papers etc.
# 
# Best wishes
# 
# Paul
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Dr Paul Norman, School of Geography, University of Leeds
# Programme Manager MSc GIS & Director of Taught Postgraduate Studies
# https://sites.google.com/site/pdqnorman/home
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   ´¯`·.¸¸.·´¯`·.¸ ><((((((º>
#                              `·.¸¸.·´¯`·.¸¸.·´¯`·.¸ ><((((º>
#                                                              
#From: Mark Livingston [mailto:Mark.Livingston@glasgow.ac.uk] 
#Sent: 18 May 2016 16:00
#To: Paul Norman <P.D.Norman@leeds.ac.uk>
#Cc: Mirjam Allik <Mirjam.Allik@glasgow.ac.uk>; Nick Bailey <Nick.Bailey@glasgow.ac.uk>
#Subject: 2001 (and earlier) output areas to 2011 datazones
#                                                            
#Dear Paul,
#
#My colleague Mirjam passed on details of the work you have been doing on deprivation and on comparison over 
# census years which is very interesting.
#                                                            
#Nick Bailey and Myself are currently working on making comparisons of private rental rates and other census 
# data between census years in the UK, but in Scotland in particular, and we are interested in the method 
# that you have used to make your comparisons.
#In the chapters you have described your method for making comparison over years and you indicate that you have 
# “Conversion tables” which  “have been developed for the work” . so we were wondering if it would be possible 
# to get a copy of the conversion tables to allow us to convert data from previous census years to 2011 datazone boundaries.
#                                                            
#Best wishes
#                                                            
#Mark
#                                                            
#Mark Livingston
#Research Fellow, Urban Big Data Centre, Urban Studies
#School of Social and Political Sciences, University of Glasgow
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 