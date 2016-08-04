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



################################
#### Fit the model
################################
#### MCMC quantities
burnin <- 10000
n.sample <- 20000
thin <- 10
n.keep <- (n.sample - burnin)/thin

# Fit the model for Glasgow first 

do_model <- function(place, initial_dist = 18000){
  
  this_dist <- initial_dist
  has_islands <- T
  
  while(has_islands){
    dz_2011  %>% 
      append_data(
        shp = . , data = centre_distance, 
        key.shp = "DataZone", key.data = "datazone"
      ) %>% 
      .[.$nearest_centre == "Glasgow",] %>% 
      .[.$distance_to_centre <= this_dist,] -> dz_city

    w <-  poly2nb(dz_city)   
    if (any(card(w) == 0)){
        this_dist <- this_dist + 1000
    } else {
      has_islands <- F
    }
  }
  
  simd_2011_reweighted %>% 
    filter(year %in% c(2004, 2012)) %>% 
    select(dz_2011, year, pop_total, pop_incomedeprived) -> tmp
  
  tmp %>% 
    select(-pop_incomedeprived) %>% 
    mutate(year = paste0("pop_total_", year)) %>% 
    spread(year, pop_total) -> pops
  
  tmp %>% 
    select(-pop_total) %>% 
    mutate(year = paste0("pop_id_", year)) %>% 
    spread(year, pop_incomedeprived) -> incdeps
  
  popinc <- pops %>% inner_join(incdeps)
  rm(tmp, pops, incdeps)
  
  
  dz_city %>% 
    append_data(
      shp = ., data = popinc,
      key.shp = "DataZone", key.data = "dz_2011",
      ignore.na = T
    ) %>% 
    .[!is.na(.$pop_total_2004),] -> dz_city
  
  
  w_nb <- poly2nb(dz_city)
  # distance to allow the 'islands' to be included 
  W_list <- nb2listw(W_nb, style = "B")
  W <- nb2mat(W_nb, style = "B")
  n <- nrow(W)
  
  # Need to remove unconnected datazones ('islands')
  
  
  
  #### Format the data
  Y.mat <- cbind(
    dz_city@data$pop_id_2004, 
    dz_city@data$pop_id_2012 
  )
  Y <- as.integer(t(Y.mat)) # Noninteger values as reweighted
  
  N.mat <- cbind(
    dz_city@data$pop_total_2004, 
    dz_city@data$pop_total_2012 
  )
  N <- as.integer(t(N.mat)) # Noninteger values as reweighted
  
  #### Run the model
  model <- binomial.MCARleroux(
    formula=Y~1, trials=N, W=W, 
    burnin=burnin, n.sample=n.sample, thin=thin
  )
  model$summary.results
  
  #### Compute the coordinates and ordering from the city centre
  dist.order <- order(dz_city@data$distance_to_centre)
  
  #### Compute the global RCI and D
  indicators.post <- array(NA, c(n.keep,4))
  colnames(indicators.post) <- c("RCI2004", "RCI2012", "D2004", "D2012")
  
  all_2004 <- as.integer(N.mat[,1])
  all_2012 <- as.integer(N.mat[,2])
  for(i in 1:n.keep)
  {
    ## Compute the probability and fitted values for the ith posterior sample
    logit <- model$samples$beta[i, ] + model$samples$phi[i, ]
    prob <- exp(logit) / (1 + exp(logit))
    prob.mat <- matrix(prob, nrow=n, byrow=TRUE)
    fitted.mat <- N.mat * prob.mat
    
    ## Compute the RCI for both years
    indicators.post[i, 1] <-RCI(fitted.mat[ ,1], as.integer(N.mat[,1]), dist.order)
    indicators.post[i, 2] <-RCI(fitted.mat[ ,2], as.integer(N.mat[,2]), dist.order)
    
    ## Compute D for both years
    p_2004 <- prob.mat[ ,1]
    p_2004_av <- sum(p_2004 * all_2004) / sum(all_2004)
    indicators.post[i, 3] <- sum(all_2004 * abs(p_2004 - p_2004_av)) / (2 * sum(all_2004) * p_2004_av * (1-p_2004_av))   
    
    p_2012 <- prob.mat[ ,2]
    p_2012_av <- sum(p_2012 * all_2012) / sum(all_2012)
    indicators.post[i, 4] <- sum(all_2012 * abs(p_2012 - p_2012_av)) / (2 * sum(all_2012) * p_2012_av * (1-p_2012_av))   
  }
  
  indicators.post  
}


indicators_dundee <- do_model("Dundee")
indicators_aberdeen <- do_model("Aberdeen")
indicators_edinburgh <- do_model("Edinburgh")



## Summarise the results
## RCI and D in 2001 and 2011 - estimate and 95% Credible Interval
round(apply(indicators.post, 2, quantile, c(0.5, 0.025, 0.975)),3)

## Differences in RCI and D in 2011 - 2001
round(quantile(indicators.post[ ,2] - indicators.post[ ,1], c(0.5, 0.025, 0.975)),3)
round(quantile(indicators.post[ ,4] - indicators.post[ ,3], c(0.5, 0.025, 0.975)),3)





# ##################################################
# #### Compute the local RCI for each area and plot
# ##################################################
# fitted <- matrix(model$fitted.values, nrow=n.city, byrow=TRUE)
# 
# #use RCI path to calculate Local RCI for each k
# K.range <- seq(10,n.city,10)
# K.length <- length(K.range)
# RCI.local.K <- array(NA, c(n.city,2,K.length))
# rownames(RCI.local.K) <- sheffield.map$lsoa
# 
# for(k in 1:K.length) {
#   for(i in 1:n.city)
#   {
#     ## Compute the ordering from the current DZ
#     centre <- coords[i, ]
#     dist.cc <- sqrt((coords[ ,1] - centre[1])^2 + (coords[ ,2] - centre[2])^2)
#     dist.order <- order(dist.cc) 
#     
#     ## Compute the RCI
#     RCI.local.K[i,1,k] <- RCI.path(fitted[ ,1], total2001, dist.order,K=K.range[k])
#     RCI.local.K[i,2,k] <- RCI.path(fitted[ ,2], total2011, dist.order,K=K.range[k])
#   }
#   print(k)
# }
# 
# ## summarise the median and 95% interval of local RCI for each area at each year
# dim(RCI.local.K)
# RCI.local.2001 <- apply(RCI.local.K[,1,],1,quantile,c(0.5,0.025,0.975))
# RCI.local.2011 <- apply(RCI.local.K[,2,],1,quantile,c(0.5,0.025,0.975))
# 
# RCI.local.2001 <- t(RCI.local.2001)
# RCI.local.2011 <- t(RCI.local.2011)
# # the number of significant ones
# sum(RCI.local.2001[,2]*RCI.local.2001[,3] > 0)
# sum(RCI.local.2011[,2]*RCI.local.2011[,3] > 0)
# 
# sum(rownames(RCI.local.2001)==rownames(RCI.local.2011))
# 
# RCI.local <- cbind(RCI.local.2001,RCI.local.2011)
# colnames(RCI.local) <- c("RCI.2001","lower.2001","upper.2001","RCI.2011","lower.2011","upper.2011")
# RCI.local <- data.frame(RCI.local)
# RCI.local$centralised2001 <- RCI.local$RCI.2001 > 0 & RCI.local$lower.2001*RCI.local$upper.2001 > 0
# RCI.local$decetra2001 <- RCI.local$RCI.2001 < 0 & RCI.local$lower.2001*RCI.local$upper.2001 > 0
# 
# RCI.local$centralised2011 <- RCI.local$RCI.2011 > 0 & RCI.local$lower.2011*RCI.local$upper.2011 > 0
# RCI.local$decetra2011 <- RCI.local$RCI.2011 < 0 & RCI.local$lower.2011*RCI.local$upper.2011 > 0
# 
# ## map the local RCI
# sheffield.map@data <- data.frame(sheffield.map@data,RCI.local)
# 
# #year 2001
# plot(sheffield.map)
# plot(sheffield.map[sheffield.map$centralised2001==1,],col="red",add=TRUE,lwd=1.5)
# plot(sheffield.map[sheffield.map$decetra2001==1,],col="green",add=TRUE,lwd=1.5)
# #year 2011
# plot(sheffield.map)
# plot(sheffield.map[sheffield.map$centralised2011==1,],col="red",add=TRUE,lwd=1.5)
# plot(sheffield.map[sheffield.map$decetra2011==1,],col="green",add=TRUE,lwd=1.5)
# 
# ## Map the local RCI
# range <- quantile(c(min(sheffield.map$RCI.2001-0.01),max(sheffield.map$RCI.2011+0.01)), seq(0, 1, 0.1))
# n.range <- length(range) - 1
# spplot(sheffield.map,c("RCI.2001","RCI.2011"), 
#        at=range, scales=list(draw=TRUE), xlab="Easting", names.attr=c("Local RCI 2001", "Local RCI 2011"),
#        ylab="Northing", col.regions=hsv(0.7, seq(0.2,1,length.out=n.range),1), col="transparent")
# 
# 
# writeOGR(sheffield.map,dsn=".",layer="sheffield.map",,driver="ESRI Shapefile")


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
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 