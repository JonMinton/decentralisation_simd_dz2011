
# SIMD combined 

imd_combined <- read_csv("data/imd/imd_id_tidied.csv")



# Some to dos: 

# 1) find shapefile for lsoas
# 2) produce cartogram for lsoas (or see if I've done this already)
# 3) add city centroids for the cities of interest 
# 4) run analyses using 'greedy' approach below
# 5) run analyses using ttwa approach 
# 6) update 

# Shapefile for lsoa in 2001
lsoa_2001 <- read_shape(file = "shapefiles/SG_DataZoneBdry_2011/SG_DataZone_Bdry_2011.shp")

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
