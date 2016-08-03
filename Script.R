rm(list = ls())

require(pacman)

pacman::p_load(
  readr, readxl,
  purrr, stringr, tidyr, dplyr, 
  broom,
  rgeos,
  ggplot2, cowplot,
  tmap
)

source("scripts/do_dz2011_data_management.R")

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



# 
# # 
# # Gwilym/Jon/Gavin (cc. Mark/Mirjam)
# # 
# # I'd be keen to plan a timetable for this work, and agreeing how we will try to use the data, particularly to 
# look at the question of suburbanisation of poverty. I don't think it is a huge amount of effort and it would be 
# good to have it set up so the full code is available through GitHub so others are able to follow and comment on 
# the process. We'd also try to make the resulting data available through UBDC. Have copied in Mark and Mirjam since 
# they may be interested in contributing or commenting on method. Steps would be as follows:
# # 
# # June/July
# # 1. Get the existing SIMD data for DZ-2001 from 2004 - should have this from the pollution paper work but need to 
#   make sure that, for each year, we have: population, SIMD score and rank, and Income Deprivation %.

# # 
# # 2. Using the old DZ boundaries, run the decentralisation code on the Income Deprived for the four cities for each year, 
# and check whether we see similar pattern to the suburbanisation paper. This also acts as a baseline against which we 
# can check the re-aggregated data.

# # 
# # 3. Agree a procedure for making our 'best' estimate of overall SIMD score/rank, and Income Deprivation, on new DZ-2011 
# boundaries. We can do better than simply looking at the nearest old DZ for each new DZ centroid by using information about 
# variations within each DZ, utilising Census data at OA level. We have: deprivation scores for OAs in 2001 (Mirjam Allik's 
# paper); and a lookup file showing how the population of each OA-2001 is apportioned to each DZ-2011 (Paul Norman's file). 
# The approach would be:
# a. use OA-2001 deprivation decile to apportion DZ-2001 deprivation to the OA-2001;
# b. use Norman's file to re-aggregate to DZ-2011.
# 
# 4. Re-run the decentralisation code using the estimates of Income Deprivation for the new DZ boundaries and compare with 
# 2. To do this, we need to be able to identify the four cities in terms of the DZ-2011 boundaries.
# 
# Early August
# 5. Finally, on the day that the SIMD2016 are released, run the decentralisation code on that data and also produce simple 
# maps showing absolute changes. Put out a short summary.
# 
# Does this sound like a plan?
# 
# Regards
# 
# Nick
#                                 

# 
# From: Paul Norman [mailto:P.D.Norman@leeds.ac.uk] 
# Sent: 18 May 2016 16:29
# To: Mark Livingston
# Cc: Mirjam Allik; Nick Bailey
# Subject: RE: 2001 (and earlier) output areas to 2011 datazones
# 
# Here are the files …
# 
# From: Paul Norman 
# Sent: 18 May 2016 16:25
# To: 'Mark Livingston' <Mark.Livingston@glasgow.ac.uk>
#   Cc: Mirjam Allik <Mirjam.Allik@glasgow.ac.uk>; Nick Bailey <Nick.Bailey@glasgow.ac.uk>
#   Subject: RE: 2001 (and earlier) output areas to 2011 datazones
# 
# Hi Mark & colleagues
# 
# Yep, sure. Here you go. In the correspondence to Alistair I said that I had wanted to delve more into tenure 
# for a revised Townsend over time but haven’t!
#   
#   The conversion tables operate the same way as stepped through in an Excel Northern Ireland spreadsheet version 
#  where this example is 2011 LGDs to the later fewer 2014(?) LGDs. Much of this then is just aggregation but where 
# the weights are less than 1 the source data is apportioned to different LGDs.
# 
# Recipe:
#   Join the original census ‘source’ data to the GCT
# Multiple variables by the weights
# Sum across the ‘target’ zones
# 
# In the Scotland conversions, the older to recent information involves more estimation of locations than the 
# newer stuff. From memory, I think some 71 EDs in the islands struggle because the geocoding and coastline 
# digitisation and differences over time will be somewhat iffy. Obviously if you spot something weird that compromises 
# please shout!
#   
#   Let me know how you get on and if ever you want text to explain for papers etc.
# 
# Best wishes
# 
# Paul
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Dr Paul Norman, School of Geography, University of Leeds
# Programme Manager MSc GIS & Director of Taught Postgraduate Studies
# https://sites.google.com/site/pdqnorman/home
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   ´¯`·.¸¸.·´¯`·.¸ ><((((((º>
#                              `·.¸¸.·´¯`·.¸¸.·´¯`·.¸ ><((((º>
#                                                              
#From: Mark Livingston [mailto:Mark.Livingston@glasgow.ac.uk] 
#Sent: 18 May 2016 16:00
#To: Paul Norman <P.D.Norman@leeds.ac.uk>
#Cc: Mirjam Allik <Mirjam.Allik@glasgow.ac.uk>; Nick Bailey <Nick.Bailey@glasgow.ac.uk>
#Subject: 2001 (and earlier) output areas to 2011 datazones
#                                                            
#Dear Paul,
#
#My colleague Mirjam passed on details of the work you have been doing on deprivation and on comparison over 
# census years which is very interesting.
#                                                            
#Nick Bailey and Myself are currently working on making comparisons of private rental rates and other census 
# data between census years in the UK, but in Scotland in particular, and we are interested in the method 
# that you have used to make your comparisons.
#In the chapters you have described your method for making comparison over years and you indicate that you have 
# “Conversion tables” which  “have been developed for the work” . so we were wondering if it would be possible 
# to get a copy of the conversion tables to allow us to convert data from previous census years to 2011 datazone boundaries.
#                                                            
#Best wishes
#                                                            
#Mark
#                                                            
#Mark Livingston
#Research Fellow, Urban Big Data Centre, Urban Studies
#School of Social and Political Sciences, University of Glasgow
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 