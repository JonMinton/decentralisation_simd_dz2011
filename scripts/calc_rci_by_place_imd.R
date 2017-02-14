# Explore IMD by city 


# TO DO: Wales  IMD 


rm(list = ls())

require(pacman)

pacman::p_load(
  tidyverse,
  readr, readxl,
  stringr,  
  purrr,
  rgeos,
  ggplot2,
  tmap
)

source("scripts/from_gavin/RCI.R")


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



# imd
dist_to_centre <- read_csv("data/lsoa_2011_by_dist_to_centres.csv")
dta <- read_csv("data/imd/imd_id_tidied.csv", col_types = "icdd")

#SIMD
dist_to_centre_scot <- read_csv("data/dz_2011_by_dist_to_centres.csv")
dta_scot <- read_csv("data/simd/simd_combined_on_2011.csv")

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


# Same but for Scotland
dta_scot %>% 
  inner_join(dist_to_centre_scot, by = c("dz_2011" = "dz")) %>%
  mutate(pdens = pop_total / area) %>%
  select(dz_2011, place, year, pop_id = pop_incomedeprived, pop_total, distance, pdens) %>% 
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
  ungroup -> rci_by_year_place_scot


# Combine England w/ Scotland 

rci_by_year_place <- bind_rows(
  rci_by_year_place, 
  rci_by_year_place_scot
) %>% 
  filter(place != "Cardiff")
  
# Want to arrange TTWA by population size in (say) 2010

dta %>% 
  inner_join(dist_to_centre) %>% 
  filter(year == 2010) %>% 
  group_by(place) %>% 
  summarise(place_pop = sum(pop_total)) -> tmp1


dta_scot %>% 
  inner_join(dist_to_centre_scot, by = c("dz_2011" = "dz")) %>% 
  filter(year == 2009) %>% 
  group_by(place) %>% 
  summarise(place_pop = sum(pop_total)) -> tmp2

tmp1 %>% 
  bind_rows(tmp2) %>% 
  arrange(desc(place_pop)) %>% 
  .[["place"]] -> place_by_size_order

rm(tmp1, tmp2)







rci_by_year_place %>% 
  mutate(place = factor(place, ordered = T, levels = place_by_size_order)) %>% 
  ggplot(., aes(x = year, y = rci)) + 
  geom_line() + geom_point() + 
  geom_hline(aes(yintercept = 0)) +  
  facet_wrap(~place) + 
  labs(title = "RCI by year and TTWA", subtitle = "TTWAs arranged by size", x = "Year", y = "Relative Centralisation Index (RCI)")
ggsave("figures/TTWA/rci_by_year_ttwa.png", height= 25, width = 25, units = "cm", dpi = 300)


rci_by_year_place %>% 
  mutate(place = factor(place, ordered = T, levels = place_by_size_order)) %>% 
  ggplot(., aes(x = year, y = rdi)) + 
  geom_line() + geom_point() + 
  geom_hline(aes(yintercept = 0)) +  
  facet_wrap(~place) + 
  labs(title = "RDI by year and TTWA", subtitle = "TTWAs arranged by size", x = "Year", y = "Relative 'Densification' Index (RDI)")
ggsave("figures/TTWA/rdi_by_year_ttwa.png", height= 25, width = 25, units = "cm", dpi = 300)


# RDI and RCI on same plot 
rci_by_year_place %>% 
  mutate(place = factor(place, ordered = T, levels = place_by_size_order)) %>% 
  select(-d) %>% 
  gather(key = "measure", value = "value", rci, rdi) %>% 
  ggplot(., aes(x  = year, y = value, group = measure)) + 
  geom_line(aes(linetype = measure)) + 
  geom_point(aes(shape = measure)) +
  geom_hline(aes(yintercept = 0)) + 
  facet_wrap(~place) +
  labs(title = "RDI and RCI by year and TTWA", subtitle = "TTWAs arranged by size", x = "Year", y = "Score")
ggsave("figures/TTWA/rdi_rci_by_year_ttwa.png", height= 25, width = 25, units = "cm", dpi = 300)




# D - faceted

rci_by_year_place %>% 
  mutate(TTWA = factor(place, ordered = T, levels = place_by_size_order)) %>% 
  ggplot(., aes(x = year, y = d)) + 
  geom_line() + geom_point()  +
  geom_hline(yintercept = 0) + 
  coord_cartesian(ylim = c(0.25, 0.45)) + 
  labs(x = "Year", y = "Dissimilarity Index") + 
  facet_wrap(~TTWA) + 
  labs(title = "D by year and TTWA", subtitle = "TTWAs arranged by size", x = "Year", y = "Dissimilarity Index")
ggsave("figures/TTWA/d_by_year_ttwa.png", height= 25, width = 25, units = "cm", dpi = 300)



# ratio, faceted
rci_by_year_place %>% 
  mutate(TTWA = factor(place, ordered = T, levels = place_by_size_order)) %>% 
  mutate(rtio = abs(rci) / abs(rdi)) %>% 
  ggplot(., aes(x = year, y = rtio)) + 
  geom_line() + geom_point()  +
  scale_y_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1, 2)) + 
  labs(x = "Year", y = "Ratio of |RCI| / |RDI| (log scale)", title = "Ratio of RCI to RDI", subtitle = "Ratio of absolute values"
       ) + 
  facet_wrap(~TTWA) + 
  geom_hline(aes(yintercept = 1), lty = "dashed") 
ggsave("figures/TTWA/ratio_abs_by_ttwa.png", height = 25, width = 25, units = "cm", dpi = 300)


# Share of poor, by decile of density or decile of distance ---------------

dta_scot %>% 
  inner_join(dist_to_centre_scot, by = c("dz_2011" = "dz")) %>% 
  select(year, lsoa = dz_2011, pop_id = pop_incomedeprived, pop_total, place, distance, area) -> tmp1
  
dta %>% 
  inner_join(dist_to_centre) -> tmp2

ypd <- bind_rows(tmp1, tmp2) %>% filter(place != "Cardiff")
rm(tmp1, tmp2)

ypd %>% 
  group_by(year, place) %>% 
  mutate(
    dist_decile = ntile(distance, 10)
    ) %>% 
  group_by(year, place, dist_decile) %>% 
  summarise(pop_id = sum(pop_id)) %>% 
  group_by(year, place) %>% 
  mutate(share_id = pop_id / sum(pop_id)) %>% 
  ggplot(., aes(x = factor(dist_decile), y = share_id, group = year)) + 
  geom_point(aes(shape = factor(year))) + geom_line(aes(colour = factor(year))) + 
  facet_wrap(~place)


ypd %>% 
  group_by(year, place) %>% 
  mutate(
    density = pop_total / area,
    density_decile = 11 - ntile(density, 10)
  ) %>% 
  group_by(year, place, density_decile) %>% 
  summarise(pop_id = sum(pop_id)) %>% 
  group_by(year, place) %>% 
  mutate(share_id = pop_id / sum(pop_id)) %>% 
  ggplot(., aes(x = factor(density_decile), y = share_id, group = year)) + 
  geom_point(aes(shape = factor(year))) + geom_line(aes(colour = factor(year))) + 
  facet_wrap(~place)


# Density and distance on a single plot 
ypd %>% 
  group_by(year, place) %>% 
  mutate(
    density = pop_total / area,
    decile = 11 - ntile(density, 10)
  ) %>% 
  group_by(year, place, decile) %>% 
  summarise(pop_id = sum(pop_id)) %>% 
  group_by(year, place) %>% 
  mutate(share_id_dens = pop_id / sum(pop_id)) %>% 
  select(year, place, decile, share_id_dens) -> tmp1

ypd %>%  
  group_by(year, place) %>% 
  mutate(
    decile = ntile(distance, 10)
  ) %>% 
  group_by(year, place, decile) %>% 
  summarise(pop_id = sum(pop_id)) %>% 
  group_by(year, place) %>% 
  mutate(share_id_dist = pop_id / sum(pop_id)) %>% 
  select(year, place, decile, share_id_dist) -> tmp2

dd_share <- inner_join(tmp1, tmp2)

rm(tmp1, tmp2)
  
dd_share %>% 
  gather(key = "dd", value = "share", share_id_dens, share_id_dist) %>% 
  ggplot(., aes(x = factor(decile), y = share, group = dd)) + 
  geom_point(aes(shape = dd)) + geom_line(aes(linetype = dd)) + 
  facet_grid(place ~ year) +   theme_minimal() +
  theme(strip.text.y = element_text(angle = 0))  


# Correlation between the two? 

dd_share %>% 
  group_by(year, place) %>% 
  nest() %>% 
  mutate(cr = map_dbl(data, function(x) cor(x[,2:3])[2,1])) %>% 
  select(-data) %>% 
  mutate(place = factor(place, ordered = T, levels = place_by_size_order)) %>% 
  ggplot(., aes(x = year, y = cr)) + 
  geom_point() + geom_line() + 
  facet_wrap(~place) + geom_hline(aes(yintercept = 0)) + 
  labs(x = "Year", y = "Correlation between rdi and rci by deciles within year")



# We should also look at relationship between distance and density 
# (Though I think I've done this already')


dta %>% 
  inner_join(dist_to_centre) %>% 
  mutate(density = pop_total / area) %>% 
  mutate(ldens = log(density)) %>% 
  select(year, place, distance, density) %>% 
  group_by(year, place) %>% 
  nest() %>% 
  mutate(cr = map_dbl(data, ~ cor(.)[2,1])) %>% 
  select(-data) %>% 
  mutate(place = factor(place, ordered = T, levels = place_by_size_order)) %>% 
  ggplot(., aes(x = year, y = cr)) + 
  geom_point() + geom_line() + 
  facet_wrap(~place) + geom_hline(aes(yintercept = 0)) + 
  labs(x = "Year", y = "Correlation between density and log-distance within TTWA")

  











rci_by_year_place %>% 
  mutate(lrtio = log(abs(rci / rdi))) %>% 
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




  