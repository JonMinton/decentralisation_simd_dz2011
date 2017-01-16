# Explore IMD by city 

rm(list = ls())

require(pacman)

pacman::p_load(
  readr, readxl,
  stringr, tidyr, dplyr, 
  rgeos,
  ggplot2,
  tmap
)

# TTWAs of interest 

ttwas_of_interest <- c(
  "Birmingham", 
  "Leeds", 
  "Sheffield and Rotherham", 
  "Bradford", 
  "Liverpool",
  "Manchester",  
  "Bristol", 
  "Wakefield and Castleford",
  "Coventry", 
  "Leicester", 
  "Sunderland",     
  "Nottingham", 
  "Newcastle and Durham", 
  "Brighton" 
)


# Load data 

# imd
dta <- read_csv("data/imd/imd_id_tidied.csv", col_types = "icdd")

# lookup
lsoa_to_ttwa <- read_csv("data/england_lookup/LSOA01_TTWA01_UK_LU.csv")

# Link LSOAs to TTWAs 

dta %>% 
  left_join(lsoa_to_ttwa, by = c("lsoa" = "LSOA01CD")) %>% 
  select(
    year, lsoa, ttwa = TTWA01CD, ttwa_nm = TTWA01NM, pop_id, pop_total
  ) -> lsoa_id_by_ttwa


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

