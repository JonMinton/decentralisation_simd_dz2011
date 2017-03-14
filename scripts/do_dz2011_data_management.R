
# SIMD combined 

simd_combined <- read_csv("data/simd/simd_combined.csv")
# Paul Norman Lookup
lkup <-  read_csv("data/paul_norman_file/paul_norman_dz2011_table.csv")

# simd 2016 already dz2011, so exclude from reweighting below

simd_older_combined <- simd_combined %>% filter(year != 2016)
# Produce reweighted SIMD scores
lkup  %>% 
  dplyr::select(dz_2001, dz_2011)  %>% 
  arrange(dz_2001, dz_2011)  %>%  
  group_by(dz_2001, dz_2011)  %>% 
  tally  %>% # produce n, giving number of OAs which contain particular groupings of dz_2001 and dz_2011
  arrange(dz_2011, dz_2001)  %>% 
  dplyr::select(dz_2011, dz_2001, n)  %>% 
  group_by(dz_2011)   %>% 
  arrange(dz_2011, dz_2001) %>% 
  mutate(weight = n / sum(n))   %>% # Weighting by dz_2011
  left_join(simd_older_combined, by = c("dz_2001" = "datazone"))  %>% 
  dplyr::select(-simd_rank)  %>%  # Not meaningful to reweight rank
  group_by(dz_2011, year) %>% 
  summarise(
    pop_total = sum(pop_total * weight),
    pop_workingage = sum(pop_workingage * weight), 
    pop_incomedeprived = sum(pop_incomedeprived * weight)
  ) -> simd_2011_reweighted

            
simd_2016_simplified <- simd_combined %>% 
  filter(year == 2016) %>% 
  dplyr::select(dz_2011 = datazone, year, pop_total, pop_workingage, pop_incomedeprived)

simd_2011_reweighted <- bind_rows(simd_2011_reweighted, simd_2016_simplified)

write_csv(x = simd_2011_reweighted, path = "data/simd/simd_combined_on_2011.csv")





# Shapefile for 2011
dz_2011 <- read_shape(file = "shapefiles/SG_DataZoneBdry_2011/SG_DataZone_Bdry_2011.shp")

# Cartogram for 2011 
dz_2011_cart <- read_shape(file = "shapefiles/dz_2011_cart/dz_2011_cart.shp", current.projection = "longlat")
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
