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
dist_to_centre <- read_csv("data/lsoa_2011_by_dist_to_centres.csv")
dta <- read_csv("data/imd/imd_id_tidied.csv", col_types = "icdd")


dta %>% 
  inner_join(dist_to_centre) %>% 
  mutate(prop_id = pop_id / pop_total) %>% 
  ggplot(., aes(x = distance, y = prop_id)) +
  geom_point(alpha = 0.1) + stat_smooth() + 
  facet_grid(year ~ place)

dta %>% 
  inner_join(dist_to_centre) %>% 
  mutate(prop_id = pop_id / pop_total) %>% 
  mutate(year = factor(year)) %>%
  ggplot(., aes(x = distance, y = prop_id, group = year, colour = year)) +
  stat_smooth(, se = F) + 
  facet_wrap( ~ place)



