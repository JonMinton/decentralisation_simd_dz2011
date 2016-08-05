rm(list = ls())

require(pacman)

pacman::p_load(
  readr, readxl,
  purrr, Rcpp, MCMCpack,
  stringr, tidyr, dplyr, broom,
  rgeos, shapefiles, sp, spdep, truncdist,
  maptools,   tmap,
  ggplot2, cowplot, RColorBrewer
)

# My script
source("scripts/do_dz2011_data_management.R")

# Gavin's scripts
source('scripts/from_gavin/RCI.R')
source('scripts/from_gavin/binomial.MCARleroux.R')
Rcpp::sourceCpp('scripts/from_gavin/aqmen.cpp')


# Check this works - plot distance to nearest centre
# dz_2011 %>% 
#   append_data(., centre_distance, key.shp = "DataZone", key.data = "datazone", ignore.duplicates =T) %>% 
#   tm_shape(.) + 
#   tm_polygons(
#     col = "distance_to_centre", 
#     border.alpha = 0.1
#   )

# plot nearest centre
dz_2011 %>% 
  append_data(., centre_distance, key.shp = "DataZone", key.data = "datazone", ignore.duplicates = T) %>% 
  tm_shape(.) + 
  tm_polygons(col = "nearest_centre", border.alpha = 0)



# Map of income deprived each year  ---------------------------------------

simd_2011_reweighted %>% 
  select(dz_2011, year, pop_incomedeprived, pop_total) %>%
  mutate(prop_id = pop_incomedeprived / pop_total) %>% 
  select(dz_2011, year, prop_id) %>% 
  spread(year, prop_id) %>% 
  append_data(
    shp = dz_2011, data = . ,
    key.shp = "DataZone", key.data = "dz_2011", ignore.na = T
  ) %>% 
  tm_shape(.) + 
  tm_facets(ncol = 2) + 
  tm_fill(col = c("2004", "2006", "2009", "2012"), breaks = seq(0, 1, by = 0.1), showNA = F) -> id_chor_dz2011

# # As above, but separately for each city 
# 
# facet_for_year <- function(this_year){
#   simd_2011_reweighted %>% 
#     select(dz_2011, year, pop_incomedeprived, pop_total) %>%
#     mutate(prop_id = pop_incomedeprived / pop_total) %>% 
#     select(dz_2011, year, prop_id) %>%
#     left_join(centre_distance, by = c("dz_2011" = "datazone")) %>% 
#     spread(year, prop_id) %>% 
#     append_data(
#       shp = dz_2011, data = . ,
#       key.shp = "DataZone", key.data = "dz_2011", ignore.na = T
#     ) %>% 
#     tm_shape(., title = this_year) + 
#     tm_facets("nearest_centre" , ncol = 1, showNA = FALSE, free.coords = TRUE, drop.units = TRUE) + 
#     tm_fill(col = this_year, breaks = seq(0, 1, by = 0.1), showNA = F, legend.show = F) 
# }
# 
# 
# years <- c("2004", "2006", "2009", "2012")


# Change in proportion income deprived compared with distance from city centre
# for those datazones whose centres are closer to Glasgow than any other centre 
decent_plot_for_city <- function(this_city){
  simd_2011_reweighted  %>% 
    ungroup() %>%  
    left_join(centre_distance, by = c("dz_2011" = "datazone")) %>% 
    filter(nearest_centre == this_city) %>%
    distinct() %>% 
    mutate(prop_id = pop_incomedeprived / pop_total) %>% 
    select(dz_2011, distance_to_centre, year, prop_id) %>% 
    mutate(distance_to_centre = distance_to_centre / 1000) %>% 
    spread(year, prop_id) %>% 
    mutate(change = `2012` - `2004`) %>% 
    ggplot(., aes(x = distance_to_centre, y = change)) + 
    geom_point( alpha = 0.01) + 
    scale_x_log10(limits = c(0.5, 50), breaks = c(0.5, 1, 2, 5, 10, 20, 50)) + 
    scale_y_continuous(limits = c(-0.25, 0.25) ) +
    stat_smooth() + 
    geom_hline(aes(yintercept = 0), linetype = "dashed") + 
    geom_vline(aes(xintercept = 1e+0)) + 
    geom_vline(aes(xintercept = 1.2e+1)) + 
    labs(x = "Distance to centre (km)", y = "Change in ID",
         title = this_city)
}


plot_grid(
  decent_plot_for_city("Aberdeen"),
  decent_plot_for_city("Dundee"),
  decent_plot_for_city("Edinburgh"),
  decent_plot_for_city("Glasgow"),
  nrow = 2
)
# Change in population size 

popchange_plot_for_city <- function(this_city){
  simd_2011_reweighted  %>% 
    ungroup() %>%  
    left_join(centre_distance, by = c("dz_2011" = "datazone")) %>% 
    filter(nearest_centre == this_city) %>%
    distinct() %>% 
    select(dz_2011, distance_to_centre, year, pop_total) %>% 
    mutate(distance_to_centre = distance_to_centre / 1000) %>% 
    spread(year, pop_total) %>% 
    mutate(change = 100 * (`2012` / `2004`) - 1) %>% 
    ggplot(., aes(x = distance_to_centre, y = change)) + 
    geom_point( alpha = 0.1) + 
    scale_x_log10(breaks = c(0.5, 1, 2, 5, 10, 20, 50, 100)) + 
    scale_y_continuous(limits = c(0, 300)) +    
    stat_smooth() + 
    geom_vline(aes(xintercept = 1e+0)) + 
    geom_vline(aes(xintercept = 1.2e+1)) +
    geom_hline(aes(yintercept = 100), linetype = "dashed") + 
    labs(x = "Distance to centre (km)", y = "% Change in population size",
         title = this_city)
}

popchange_plot_for_city("Aberdeen")
popchange_plot_for_city("Dundee")
popchange_plot_for_city("Edinburgh")
popchange_plot_for_city("Glasgow")

# Change in income deprivation population 

idpopchange_plot_for_city <- function(this_city){
  simd_2011_reweighted  %>% 
    ungroup() %>%  
    left_join(centre_distance, by = c("dz_2011" = "datazone")) %>% 
    filter(nearest_centre == this_city) %>%
    distinct() %>% 
    select(dz_2011, distance_to_centre, year, pop_incomedeprived) %>% 
    mutate(distance_to_centre = distance_to_centre / 1000) %>% 
    spread(year, pop_incomedeprived) %>% 
    mutate(change = 100 * (`2012` / `2004`) - 1) %>% 
    ggplot(., aes(x = distance_to_centre, y = change)) + 
    geom_point( alpha = 0.1) + 
    scale_x_log10(breaks = c(0.5, 1, 2, 5, 10, 20, 50, 100)) + 
    scale_y_continuous(limits = c(0, 300)) +    
    stat_smooth() + 
    geom_vline(aes(xintercept = 1e+0)) + 
    geom_vline(aes(xintercept = 1.2e+1)) +
    geom_hline(aes(yintercept = 100), linetype = "dashed") + 
    labs(x = "Distance to centre (km)", y = "% Change in income deprived population",
         title = this_city)
}

idpopchange_plot_for_city("Aberdeen")
idpopchange_plot_for_city("Dundee")
idpopchange_plot_for_city("Edinburgh")
idpopchange_plot_for_city("Glasgow")


source("scripts/produce_dz2011_regression_bubble.R")


# As a scatterplot?

simple_results %>% 
  mutate(place2 = str_sub(place, 1, 1)) %>% 
  select(type, place2, estimate) %>% 
  spread(type, estimate) %>% 
  ggplot(., aes(x = denominator, y = numerator)) + 
  geom_point(size = 10, colour = "lightblue") +
  geom_text(aes(label = place2, alpha =  propid), show.legend = F) +
  coord_cartesian(xlim = c(-0.1, 0.1), ylim = c(0.0, 0.1)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  labs(x = "change in total population with distance", y = "change in income deprived population with distance")





# Do RCI  -----------------------------------------------------------------

# Tasks 

# 1) Split Scotland up into four cities, dzs within 15km, for 2004 and 2012 
# 2) produce spatial structure objects 


# #### Compute the spatial autocorrelation using Moran's I
# moran.mc(x=sheffield.map.final@data$prop.eu15.2001, listw=W.list.city, nsim=10000)
# moran.mc(x=sheffield.map.final@data$prop.eu15.2011, listw=W.list.city, nsim=10000)
# moran.mc(x=sheffield.map.final@data$prop.eu12.2011, listw=W.list.city, nsim=10000)

#### The temporal dependence
# plot(prop.eu15.2001, prop.eu15.2011, col="red", pch=19, xlab="2001", ylab="2011")
# abline(0,1, col="blue")
# cor.test(prop.eu15.2001,prop.eu15.2011)
# 
# simd_2011_reweighted %>% 
#   filter(year %in% c(2004, 2012)) %>% 
#   mutate(prop_id = pop_incomedeprived / pop_total) %>% 
#   select(dz_2011, year, prop_id) %>% 
#   spread(year, prop_id) %>% 
#   rename(prop_id_2004 = `2004`, prop_id_2012 = `2012`) %>% 
#   ggplot(., aes(x = prop_id_2004, y = prop_id_2012)) + 
#   geom_point(alpha = 0.1) + 
#   geom_abline(colour = "blue", slope = 1, intercept = 0) + 
#   stat_smooth(colour = "red", linetype = "dashed") + 
#   labs(
#     x = "ID prop, 2004", y = "ID prop, 2012",
#     title = "2012 against 2004"
#        ) -> s_04_12
# 
# simd_2011_reweighted %>% 
#   filter(year %in% c(2004, 2009)) %>% 
#   mutate(prop_id = pop_incomedeprived / pop_total) %>% 
#   select(dz_2011, year, prop_id) %>% 
#   spread(year, prop_id) %>% 
#   rename(prop_id_2004 = `2004`, prop_id_2009 = `2009`) %>% 
#   ggplot(., aes(x = prop_id_2004, y = prop_id_2009)) + 
#   geom_point(alpha = 0.1) + 
#   geom_abline(colour = "blue", slope = 1, intercept = 0) + 
#   stat_smooth(colour = "red", linetype = "dashed") + 
#   labs(
#     x = "ID prop, 2004", y = "ID prop, 2009",
#     title = "2009 against 2004"
#     ) -> s_04_09
# 
# 
# simd_2011_reweighted %>% 
#   filter(year %in% c(2009, 2012)) %>% 
#   mutate(prop_id = pop_incomedeprived / pop_total) %>% 
#   select(dz_2011, year, prop_id) %>% 
#   spread(year, prop_id) %>% 
#   rename(prop_id_2009 = `2009`, prop_id_2012 = `2012`) %>% 
#   ggplot(., aes(x = prop_id_2009, y = prop_id_2012)) + 
#   geom_point(alpha = 0.1) + 
#   geom_abline(colour = "blue", slope = 1, intercept = 0) + 
#   stat_smooth(colour = "red", linetype = "dashed") + 
#   labs(
#     x = "ID prop, 2009", y = "ID prop, 2012",
#     title = "2012 against 2009"
#   ) -> s_09_12
# 
# plot_grid(s_04_12, s_04_09, s_09_12, nrow = 1)
# 

all_posteriors <- read_csv("data/all_posterior_draws.csv") # Results from running the many models

all_posteriors  %>% 
  group_by(place, measure, period)  %>% 
  summarise(
    lower = quantile(value, 0.025), 
    median = quantile(value, 0.500), 
    upper = quantile(value, 0.975)
            ) -> posterior_summaries

# Do RCI graph
posterior_summaries %>% 
  filter(measure == "RCI") %>% 
   mutate(period = as.factor(period)) %>% 
   ggplot(., 
          aes(
            x = period, group = place, 
            y = median, colour = place,
            shape = place)) + 
   geom_line() + geom_point() + 
   scale_y_continuous(limits = c(0, 0.4) ) +
   geom_line(aes(y = upper), linetype = "dashed") + 
   geom_line(aes(y = lower), linetype = "dashed") + 
   labs(
     y = "RCI of income deprived", x = "Year", 
     title = "Change in RCI", 
     colour = "City", shape = "City") +
   theme(legend.position = c(0.8, 0.8))

# Do D graph

posterior_summaries %>% 
  filter(measure == "D") %>% 
  mutate(period = as.factor(period)) %>% 
  ggplot(., 
         aes(
           x = period, group = place, 
           y = median, colour = place,
           shape = place)) + 
  geom_line() + geom_point() + 
  scale_y_continuous(limits = c(0, 0.5) ) +
  geom_line(aes(y = upper), linetype = "dashed") + 
  geom_line(aes(y = lower), linetype = "dashed") + 
  labs(
    y = "D of income deprived", x = "Year", 
    title = "Change in D", 
    colour = "City", shape = "City") +
  theme(legend.position = c(0.8, 0.8))
