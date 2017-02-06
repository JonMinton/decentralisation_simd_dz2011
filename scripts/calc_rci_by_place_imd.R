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

source("scripts/from_gavin/RCI.R")
# imd
dist_to_centre <- read_csv("data/lsoa_2011_by_dist_to_centres.csv")
dta <- read_csv("data/imd/imd_id_tidied.csv", col_types = "icdd")


dist.order <- order(dz_city@data$distance_to_centre)


D <- function(minority, total){
  majority      <- total - minority
  MINORITY      <- sum(minority)
  MAJORITY      <- sum(majority)
  
  p1 <- minority / MINORITY
  p2 <- majority / MAJORITY 
  
  ad <- abs(p1 - p2)
  
  out <- 0.5 * sum(ad)
  out
}

# Inputs to RCI are 
#povvec
#popvec
#order


# AS the inputs to the RCI function simply include a vector giving order by distance, 
# by changing the order vector to density (highest to lowest) a relative densification 
# index (not quite the same as RCO) can be calculated using the same function. 
# The degree of similarity of dissimilarity between RCI and RDI can be taken as an indicator 
# of polycentricity (maybe)


dta %>% 
  inner_join(dist_to_centre) %>%
  mutate(pdens = pop_total / area) %>% 
  group_by(place, year) %>% 
  nest() %>% 
  mutate(
    pvec = map(data, ~ .[["pop_id"]]),
    tvec = map(data, ~ .[["pop_total"]]),
    ordr = map(data, ~ order(.[["distance"]])),
    ordr_dens = map(data, ~ order(.[["pdens"]], decreasing = T))
         ) %>% 
  mutate(rci_val = pmap_dbl(list(pvec, tvec, ordr), RCI)) %>% 
  mutate(rdi_val = pmap_dbl(list(pvec, tvec, ordr_dens), RCI)) %>% 
  mutate(d_val = pmap_dbl(list(pvec, tvec), D)) %>% 
  select(place, year, rci = rci_val, rdi = rdi_val, d = d_val) %>% 
  ungroup -> rci_by_year_place

rci_by_year_place %>% 
  ggplot(., aes(x = year, y = rci)) + 
  geom_line() + geom_point() + 
  facet_wrap(~place)

rci_by_year_place %>% 
  ggplot(., aes(x = year, y = rdi)) + 
  geom_line() + geom_point() + 
  facet_wrap(~place)



rci_by_year_place %>% 
  ggplot(., aes(x = year, y = rdi, group = place)) + 
  geom_line() + geom_point() + 
  geom_label(aes(label = place, fill = place))


# ratio of rci / rdi 

rci_by_year_place %>% 
  mutate(lrtio = log(rci / rdi)) %>% 
  ggplot(., aes(x = year, y = lrtio)) + 
  geom_line() + geom_point() + 
  facet_wrap(~place)

rci_by_year_place %>% 
  mutate(lrtio = log(rci / rdi)) %>% 
  ggplot(., aes(x = year, y = lrtio, group = place)) + 
  geom_line() + geom_point() + 
  geom_label(aes(label = place, fill = place))

rci_by_year_place %>% 
  ggplot(., aes(x = year, y = rci, group = place)) + 
  geom_line() + geom_point() + 
  geom_label(aes(label = place, fill = place))

rci_by_year_place %>% 
  ggplot(., aes(x = year, y = d, group = place)) + 
  geom_line() + geom_point() + 
  geom_label(aes(label = place))



rci_by_year_place %>% 
    ggplot(., aes(x = year, y = rci)) + 
    geom_line() + geom_point() + 
    facet_wrap(~place)
  

  
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

# Correlation between log density and log distance 
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
  facet_grid(year ~ place) + 
  stat_smooth(se = F, method = "lm") + 
  stat_smooth(se = F, linetype = "dashed") + 
  scale_colour_gradientn(colours = c("red", "green", "blue")) + 
  theme(
    axis.text.x = element_text(angle = 90)
  )


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




  