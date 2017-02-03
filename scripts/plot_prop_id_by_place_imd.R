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
  stat_smooth(se = F) + 
  facet_wrap( ~ place)


# Let's look at density 
dta %>% 
  inner_join(dist_to_centre) %>% 
  mutate(prop_id = pop_id / pop_total) %>% 
  mutate(density = pop_total / area) %>% 
  mutate(year = factor(year)) %>% 
  ggplot(., aes(x  = density, y = prop_id, group = year, colour = year)) + 
  stat_smooth(se = F) + 
  facet_wrap( ~ place)

# Now log density
dta %>% 
  inner_join(dist_to_centre) %>% 
  mutate(prop_id = pop_id / pop_total) %>% 
  mutate(density = pop_total / area) %>% 
  mutate(year = factor(year)) %>% 
  ggplot(., aes(x  = log(density), y = prop_id, group = year, colour = year)) + 
  stat_smooth(se = F) + 
  facet_wrap( ~ place)

# Let's look a correlation between density and distance 

dta %>% 
  inner_join(dist_to_centre) %>% 
  mutate(density = pop_total / area) %>% 
  mutate(year = factor(year)) %>% 
  ggplot(., aes(x = distance, y = log(density), group = year, colour = year)) +
  geom_point(shape = ".", alpha = 0.05) + stat_smooth(se = F) + 
  facet_wrap(~place)

# Correlation between log densiy and log distance 
dta %>% 
  inner_join(dist_to_centre) %>% 
  mutate(density = pop_total / area) %>% 
  mutate(year = factor(year)) %>% 
  ggplot(., aes(x = log(distance), y = log(density), group = year, colour = year)) +
  geom_point(shape = ".", alpha = 0.05) + stat_smooth(se = F) + 
  facet_wrap(~place)

# Associations between distance and log density are more linear
# than those between log distance and log density


# Density, deprivation and distance 

dta %>% 
  inner_join(dist_to_centre) %>% 
  mutate(density = pop_total / area) %>% 
  mutate(year = factor(year)) %>% 
  mutate(prop_id = pop_id / pop_total) %>% 
  ggplot(., aes(x = distance, y = log(density), colour = prop_id)) + 
  geom_point(shape = ".", alpha = 0.2) + 
  facet_grid(year ~ place) + stat_smooth(se = F, method = "lm") + 
  scale_colour_gradientn(colours = c("red", "green", "blue"))


# Now to look at temporal dependence between any two years 

dta %>% 
  mutate(prop_id = pop_id / pop_total) %>%
  left_join(dist_to_centre) %>% 
  filter(year %in% c(2004, 2015)) %>%
  select(year, place, lsoa, prop_id) %>% 
  spread(year, prop_id) -> tmp
tmp %>% 
  filter(!is.na(place)) -> tmp2

tmp %>% 
  ggplot(., aes(y = `2015`, x = `2004`)) + 
  geom_point(shape = ".", alpha = 0.1) + 
  geom_point(data = tmp2) + 
  facet_wrap(~place)


# Not that informativve. There may be a greater fall over some periods than others. 




  