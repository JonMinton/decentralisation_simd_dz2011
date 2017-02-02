# Explore IMD by city 

rm(list = ls())

require(pacman)

pacman::p_load(
  readr, readxl,
  stringr, tidyr, dplyr, 
  purrr,
  rgeos,
  ggplot2,
  tmap
)


# imd
dta <- read_csv("data/imd/imd_id_tidied.csv", col_types = "icdd")


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
  Birmingham = "E30000239",
  Newcastle =  "E30000245",
  Liverpool =  "E30000233",
  Leicester =  "E30000230",
  Bristol   =  "E30000180",
  Sheffield =  "E30000261",
  Leeds     =  "E30000229",
  Nottingham = "E30000249"
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
  Nottingham  = "E01033406"
  
)

# Shapefile for 2011
lsoa_2011 <- read_shape(file = "shapefiles/Lower_layer_super_output_areas_(E+W)_2011_Boundaries_(Full_Clipped)_V2/LSOA_2011_EW_BFC_V2.shp")

#plot(lsoa_2011)


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

lsoa_by_dist_to_centres %>% 
  filter(lsoa %in% lsoas_of_interest) %>% 
  group_by(lsoa) %>% 
  filter(distance == min(distance)) %>% 
  ungroup() %>% 
  arrange(lsoa) -> lsoa_by_dist_to_centres

write_csv(lsoa_by_dist_to_centres, "data/lsoa_2011_by_dist_to_centres.csv")

rm(lsoa_2011)
gc()

dta %>% 
  inner_join(lsoa_by_dist_to_centres) %>% 
  mutate(prop_id = pop_id / pop_total) %>% 
  filter(place == "Manchester") %>% 
  ggplot(., aes(x = distance, y = prop_id))  
#  facet_grid(year ~ place)





# xxxxxxxxxxxxxx ----------------------------------------------------------





# Basic descriptive stuff: 

# Mean prop_id by ttwa by year
lsoa_id_by_ttwa %>% 
  group_by(year, ttwa_nm) %>% 
  summarise(pop_id = sum(pop_id), pop_total = sum(pop_total)) %>% 
  mutate(prop_id = pop_id / pop_total) %>% 
  select(year, ttwa_nm, prop_id) %>% 
  ggplot(., aes(x = year, y = prop_id, group = ttwa_nm)) + 
  geom_line(alpha = 0.2) + geom_point( alpha = 0.2)

# General sense of an uneven decline, mainly 2007 to 2010


# Now let's look only at the TTWAs of interest 

sum2 <- function(x) {sum(x, na.rm = T)}
lsoa_id_by_ttwa %>% 
  filter(ttwa_nm %in% ttwas_of_interest) %>% 
  group_by(year, ttwa_nm) %>% 
  summarise(pop_id = sum2(pop_id), pop_total = sum2(pop_total)) %>% 
  mutate(prop_id = pop_id / pop_total) %>% 
  select(year, ttwa_nm, prop_id) %>% 
  ggplot(., aes(x = year, y = prop_id)) + 
  geom_line() + geom_point() + 
  facet_wrap( ~ ttwa_nm)

