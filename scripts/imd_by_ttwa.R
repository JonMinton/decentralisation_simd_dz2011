# Explore IMD by city 

rm(list = ls())

require(pacman)

pacman::p_load(
  readr, readxl,
  stringr, tidyr, dplyr, 
  purrr,
  rgeos,
  ggplot2,
  tmap, tmaptools
)


# England and Wales  ------------------------------------------------------


# IMD first, then SIMD 
# # Shapefile for 2011
lsoa_2011 <- read_shape(file = "shapefiles/Lower_layer_super_output_areas_(E+W)_2011_Boundaries_(Full_Clipped)_V2/LSOA_2011_EW_BFC_V2.shp")


# 

# Non Shapefile data ------------------------------------------------------


# imd
dta <- read_csv("data/imd/imd_id_lsoa2011_tidied.csv")


# lookup
lsoa_to_ttwa <- read_csv("data/england_lookup/LSOA11_TTWA11_UK_LU.csv")

names(lsoa_to_ttwa)[1] <- "LSOA11CD"


# Link LSOAs to TTWAs 

dta %>% 
  inner_join(lsoa_to_ttwa, by = c("lsoa" = "LSOA11CD")) %>% 
  select(
    year, lsoa, ttwa = TTWA11CD, ttwa_nm = TTWA11NM, pop_id, pop_total
  ) -> lsoa_id_by_ttwa


# Now to reselect the appropriate TTWAs, reaggregating London w/ Heathrow

ttwas_to_keep <- c(
  London =     "E30000234", Heathrow = "E30000266", # to then combine again
  Manchester =  "E30000239",
  Birmingham = "E30000169",
  Newcastle =  "E30000245",
  Liverpool =  "E30000233",
  Leicester =  "E30000230",
  Bristol   =  "E30000180",
  Sheffield =  "E30000261",
  Leeds     =  "E30000229",
  Nottingham = "E30000249",
  `Warrington and Wigan` = "E30000284",
  `Wolverhampton and Walsall` = "E30000288",
  Luton = "E30000237",
  Cambridge = "E30000186",
  Southampton  = "E30000267",
  `Guildford and Aldershot`  = "E30000212",
  Medway = "E30000242",
  Crawley = "E30000196",
  Coventry = "E30000195",
  Southend = "E30000268",
  Oxford = "E30000250",
  Portsmouth = "E30000254",
  Reading = "E30000256",
  # Wales
  Cardiff = "W22000024"
  # Scotland
#  Glasgow = "S22000065",
#  Edinburgh = "S22000059"
)

lsoa_id_by_ttwa %>% 
  filter(ttwa %in% ttwas_to_keep) %>% 
  #  xtabs(~ ttwa_nm, .)
  mutate(ttwa_nm = ifelse(ttwa_nm == "Slough and Heathrow", "London", ttwa_nm)) %>%  
  #  xtabs(~ ttwa_nm, .)
  mutate(ttwa = ifelse(ttwa_nm == "Slough and Heathrow", "E30000234", ttwa)) -> 
  lsoa_selected_ttwa

# Next tasks is to find distance of each LSOA to the centre 

# Sub-task is then to specify the centres for each ttwa

ttwa_centres <- c(
  London      = "E01004736",
  Manchester  = "E01033677",
  Birmingham  = "E01033620", 
  Newcastle   = "E01008397", 
  Liverpool   = "E01033760",
  Leicester   = "E01032867",
  Bristol     = "E01014540", 
  Sheffield   = "E01033264",
  Leeds       = "E01033010",
  Nottingham  = "E01033406",
  `Warrington and Wigan` = "E01033300",
  `Wolverhampton and Walsall` = "E01010521",
  Luton = "E01015794",
  Cambridge = "E01032797",
  Southampton  = "E01017140",
  `Guildford and Aldershot`  = "E01030452",
  Medway = "E01016130",
  Crawley = "E01031585",
  Coventry = "E01009642",
  Southend = "E01015852",
  Oxford = "E01028549",
  Portsmouth = "E01017028",
  Reading = "E01016358",
  # Scottish Cities
  # Aberdeen = "S01006646",
  # Glasgow = "S01010265",
  # Edinburgh = "S01008677",
  # Dundee = "S01007705",
  # Inverness = "S01010620",
  # Perth = "S01011939",
  # `Falkirk and Stirling` = "S01013067",
  # Cardiff
  Cardiff = "W01001939" #Cardiff 03FF
)            



# area of each lsoa 

lsoa_area <- data_frame(
  lsoa = as.character(lsoa_2011@data$LSOA11CD),
  area = gArea(lsoa_2011, byid = T)
)

# dz_area <- data_frame(
#   lsoa = as.character(dz_2011@data$DataZone),
#   area = gArea(dz_2011, byid = T)
# )


# Create a new function
# This function should 
# Inputs: 
#   - 

# using rgeos::gCentroid

calc_distance_to_centres <- function(shp, code_centre){
  gCentroid(shp, byid = T) %>% 
    as(., "data.frame") -> tmp
  
  data_frame(lsoa = as.character(shp@data$LSOA11CD), x = tmp$x, y = tmp$y) %>% 
    mutate(centre = lsoa == code_centre) %>% 
    mutate(distance_to_centre = ((x - x[centre])^2 + (y - y[centre])^2)^0.5) -> output
  
  num_centres <- output$centre %>% sum()
  if(num_centres == 0) {break("No centroid found")} 
  if(num_centres > 1) {break("Multiple centres found")}
  output
}


fn <- function(val, nm){
  lsoa_2011 %>% 
    calc_distance_to_centres(., val) %>% 
    .[c(1, 5)] -> out 
  names(out) <- c("lsoa", nm)
  out
}


# Find nearest centre and distance to nearest centre
  map2(ttwa_centres, names(ttwa_centres), fn) %>% 
  reduce(., inner_join) %>% 
  gather(place, distance, -lsoa) %>%
  arrange(lsoa) -> 
  lsoa_by_dist_to_centres

lsoas_of_interest  <- unique(lsoa_selected_ttwa$lsoa)

#  Code adjusted to filter by lsoa_to_ttwa
lsoa_by_dist_to_centres %>%
  filter(lsoa %in% lsoas_of_interest) %>%
  left_join(lsoa_to_ttwa, by = c("lsoa" = "LSOA11CD")) %>% 
  filter(TTWA11NM == place) %>% 
  arrange(lsoa) %>%
  inner_join(lsoa_area) %>% 
  select(lsoa, place, distance, area) -> lsoa_by_dist_to_centres

write_csv(lsoa_by_dist_to_centres, "data/lsoa_2011_by_dist_to_centres.csv")




# Scotland (different years, shapefile) -----------------------------------



# 
# 
# 
# # Shapefile for 2011
dz_2011 <- read_shape(file = "shapefiles/SG_DataZoneBdry_2011/SG_DataZone_Bdry_2011.shp")




# simd
#dta <- read_csv("data/imd/imd_id_tidied.csv", col_types = "icdd")
dta <- read_csv("data/simd/simd_combined_on_2011.csv") 

dta %>% 
  select(dz_2011, year, pop_id = pop_incomedeprived, pop_total = pop_total) ->
  dta
# lookup
dz_to_ttwa <- read_csv("data/england_lookup/LSOA11_TTWA11_UK_LU.csv")

names(dz_to_ttwa)[1] <- "LSOA11CD"


# Link LSOAs to TTWAs 

dta %>% 
  inner_join(dz_to_ttwa, by = c("dz_2011" = "LSOA11CD")) %>% 
  select(
    year, dz_2011, ttwa = TTWA11CD, ttwa_nm = TTWA11NM, pop_id, pop_total
  ) -> dz_id_by_ttwa


# Now to reselect the appropriate TTWAs, reaggregating London w/ Heathrow

ttwas_to_keep <- c(
  # London =     "E30000234", Heathrow = "E30000266", # to then combine again
  # Manchester =  "E30000239",
  # Birmingham = "E30000169",
  # Newcastle =  "E30000245",
  # Liverpool =  "E30000233",
  # Leicester =  "E30000230",
  # Bristol   =  "E30000180",
  # Sheffield =  "E30000261",
  # Leeds     =  "E30000229",
  # Nottingham = "E30000249",
  # `Warrington and Wigan` = "E30000284",
  # `Wolverhampton and Walsall` = "E30000288",
  # Luton = "E30000237",
  # Cambridge = "E30000186",
  # Southampton  = "E30000267",
  # `Guildford and Aldershot`  = "E30000212",
  # Medway = "E30000242",
  # Crawley = "E30000196",
  # Coventry = "E30000195",
  # Southend = "E30000268",
  # Oxford = "E30000250",
  # Portsmouth = "E30000254",
  # Reading = "E30000256",
  # # Wales
  # Cardiff = "W22000024"
  # Scotland
   Glasgow = "S22000065",
   Edinburgh = "S22000059"
)

dz_id_by_ttwa %>% 
  filter(ttwa %in% ttwas_to_keep) %>% 
  rename(dz = dz_2011) -> 
  dz_selected_ttwa

# Next tasks is to find distance of each LSOA to the centre 

# Sub-task is then to specify the centres for each ttwa

ttwa_centres <- c(
  # London      = "E01004736",
  # Manchester  = "E01033677",
  # Birmingham  = "E01033620", 
  # Newcastle   = "E01008397", 
  # Liverpool   = "E01033760",
  # Leicester   = "E01032867",
  # Bristol     = "E01014540", 
  # Sheffield   = "E01033264",
  # Leeds       = "E01033010",
  # Nottingham  = "E01033406",
  # `Warrington and Wigan` = "E01033300",
  # `Wolverhampton and Walsall` = "E01010521",
  # Luton = "E01015794",
  # Cambridge = "E01032797",
  # Southampton  = "E01017140",
  # `Guildford and Aldershot`  = "E01030452",
  # Medway = "E01016130",
  # Crawley = "E01031585",
  # Coventry = "E01009642",
  # Southend = "E01015852",
  # Oxford = "E01028549",
  # Portsmouth = "E01017028",
  # Reading = "E01016358",
  # Scottish Cities
  # Aberdeen = "S01006646",
  Glasgow = "S01010265",
  Edinburgh = "S01008677"
  # Dundee = "S01007705",
  # Inverness = "S01010620",
  # Perth = "S01011939",
  # `Falkirk and Stirling` = "S01013067",
  # Cardiff
  # Cardiff = "W0100169" #Cardiff 036B
)



# area of each dz 


dz_area <- data_frame(
  dz = as.character(dz_2011@data$DataZone),
  area = gArea(dz_2011, byid = T)
)


# Create a new function
# This function should 
# Inputs: 
#   - 

# using rgeos::gCentroid

calc_distance_to_centres <- function(shp, code_centre){
  gCentroid(shp, byid = T) %>% 
    as(., "data.frame") -> tmp
  
  data_frame(dz = as.character(shp@data$DataZone), x = tmp$x, y = tmp$y) %>% 
    mutate(centre = dz == code_centre) %>% 
    mutate(distance_to_centre = ((x - x[centre])^2 + (y - y[centre])^2)^0.5) -> output
  
  num_centres <- output$centre %>% sum()
  
  if(num_centres == 0) {break("No centroid found")} 
  if(num_centres > 1) {break("Multiple centres found")}
  output
}


fn <- function(val, nm){
  dz_2011 %>% 
    calc_distance_to_centres(., val) %>% 
    .[c(1, 5)] -> out 
  names(out) <- c("dz", nm)
  out
}


# Find nearest centre and distance to nearest centre
map2(ttwa_centres, names(ttwa_centres), fn) %>% 
  reduce(., inner_join) %>% 
  gather(place, distance, -dz) %>%
  arrange(dz) -> 
  dz_by_dist_to_centres

dzs_of_interest  <- unique(dz_selected_ttwa$dz)
# 
dz_by_dist_to_centres %>%
  filter(dz %in% dzs_of_interest) %>%
  left_join(lsoa_to_ttwa, by = c("dz" = "LSOA11CD")) %>% 
  filter(TTWA11NM == place) %>% 
  arrange(dz) %>%
  inner_join(dz_area) -> dz_by_dist_to_centres

write_csv(dz_by_dist_to_centres, "data/dz_2011_by_dist_to_centres.csv")

# filter(lsoa %in% lsoas_of_interest) %>%
#   left_join(lsoa_to_ttwa, by = c("lsoa" = "LSOA11CD")) %>% 
#   filter(TTWA11NM == place) %>% 

# scotland on 2001 boundaries  --------------------------------------------





# 
# 
# 
# # Shapefile for 2011
dz_2001 <- read_shape(file = "shapefiles/scotland_2001_datazones/scotland_dz_2001.shp")




# simd
#dta <- read_csv("data/imd/imd_id_tidied.csv", col_types = "icdd")
dta <- read_csv("data/simd/simd_combined.csv") 

dta %>% 
  select(dz_2001 = datazone, year, pop_id = pop_incomedeprived, pop_total = pop_total) ->
  dta


# lookup
dz_to_ttwa <- read_csv("data/lsoa_2001_ttwa_2001/LSOA01_TTWA01_UK_LU.csv")




# Link LSOAs to TTWAs 

dta %>% 
  inner_join(dz_to_ttwa, by = c("dz_2001" = "LSOA01CD")) %>% 
  select(
    year, dz_2001, ttwa = TTWA01CD, ttwa_nm = TTWA01NM, pop_id, pop_total
  ) -> dz_id_by_ttwa_2001


# Now to reselect the appropriate TTWAs, reaggregating London w/ Heathrow

ttwas_to_keep <- c(
  # London =     "E30000234", Heathrow = "E30000266", # to then combine again
  # Manchester =  "E30000239",
  # Birmingham = "E30000169",
  # Newcastle =  "E30000245",
  # Liverpool =  "E30000233",
  # Leicester =  "E30000230",
  # Bristol   =  "E30000180",
  # Sheffield =  "E30000261",
  # Leeds     =  "E30000229",
  # Nottingham = "E30000249",
  # `Warrington and Wigan` = "E30000284",
  # `Wolverhampton and Walsall` = "E30000288",
  # Luton = "E30000237",
  # Cambridge = "E30000186",
  # Southampton  = "E30000267",
  # `Guildford and Aldershot`  = "E30000212",
  # Medway = "E30000242",
  # Crawley = "E30000196",
  # Coventry = "E30000195",
  # Southend = "E30000268",
  # Oxford = "E30000250",
  # Portsmouth = "E30000254",
  # Reading = "E30000256",
  # # Wales
  # Cardiff = "W22000024"
  # Scotland
  Glasgow = "S22000018", # 2001 code
  Edinburgh = "S22000012" # 2001 code
)

dz_id_by_ttwa_2001 %>% 
  filter(ttwa %in% ttwas_to_keep) %>% 
  rename(dz = dz_2001) -> 
  dz_selected_ttwa_2001

# Next tasks is to find distance of each LSOA to the centre 

# Sub-task is then to specify the centres for each ttwa

ttwa_centres <- c(
  # London      = "E01004736",
  # Manchester  = "E01033677",
  # Birmingham  = "E01033620", 
  # Newcastle   = "E01008397", 
  # Liverpool   = "E01033760",
  # Leicester   = "E01032867",
  # Bristol     = "E01014540", 
  # Sheffield   = "E01033264",
  # Leeds       = "E01033010",
  # Nottingham  = "E01033406",
  # `Warrington and Wigan` = "E01033300",
  # `Wolverhampton and Walsall` = "E01010521",
  # Luton = "E01015794",
  # Cambridge = "E01032797",
  # Southampton  = "E01017140",
  # `Guildford and Aldershot`  = "E01030452",
  # Medway = "E01016130",
  # Crawley = "E01031585",
  # Coventry = "E01009642",
  # Southend = "E01015852",
  # Oxford = "E01028549",
  # Portsmouth = "E01017028",
  # Reading = "E01016358",
  # Scottish Cities
  # Aberdeen = "S01006646",
  Glasgow = "S01003358", # Closest match to "S01010265",
  Edinburgh = "S01002131" # Closest match to "S01008677"
  # Dundee = "S01007705",
  # Inverness = "S01010620",
  # Perth = "S01011939",
  # `Falkirk and Stirling` = "S01013067",
  # Cardiff
  # Cardiff = "W0100169" #Cardiff 036B
)



# area of each dz 


dz_area <- data_frame(
  dz = as.character(dz_2001@data$zonecode),
  area = gArea(dz_2001, byid = T)
)


# Create a new function
# This function should 
# Inputs: 
#   - 

# using rgeos::gCentroid

calc_distance_to_centres <- function(shp, code_centre){
  gCentroid(shp, byid = T) %>% 
    as(., "data.frame") -> tmp
  
  data_frame(dz = as.character(shp@data$zonecode), x = tmp$x, y = tmp$y) %>% 
    mutate(centre = dz == code_centre) %>% 
    mutate(distance_to_centre = ((x - x[centre])^2 + (y - y[centre])^2)^0.5) -> output
  
  num_centres <- output$centre %>% sum()
  
  if(num_centres == 0) {break("No centroid found")} 
  if(num_centres > 1) {break("Multiple centres found")}
  output
}


fn <- function(val, nm){
  dz_2001 %>% 
    calc_distance_to_centres(., val) %>% 
    .[c(1, 5)] -> out 
  names(out) <- c("dz", nm)
  out
}


# Find nearest centre and distance to nearest centre
map2(ttwa_centres, names(ttwa_centres), fn) %>% 
  reduce(., inner_join) %>% 
  gather(place, distance, -dz) %>%
  arrange(dz) -> 
  dz_by_dist_to_centres

dzs_of_interest  <- unique(dz_selected_ttwa_2001$dz)
# 
dz_by_dist_to_centres %>%
  filter(dz %in% dzs_of_interest) %>% 
  left_join(dz_to_ttwa, by = c("dz" = "LSOA01CD")) %>% 
  filter(TTWA01NM == place) %>% 
  ungroup() %>%
  arrange(dz) %>%
  inner_join(dz_area) -> dz_by_dist_to_centres

write_csv(dz_by_dist_to_centres, "data/dz_2001_by_dist_to_centres.csv")


