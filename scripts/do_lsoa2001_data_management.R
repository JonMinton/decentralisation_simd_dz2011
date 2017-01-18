

rm(list = ls())

require(pacman)

pacman::p_load(
  Rcpp,
  shapefiles,
  sp,
  spdep,
  CARBayes,
  MCMCpack,
  truncdist,
  
  rgdal,
  maptools,  
  
  readr, readxl,
  stringr, tidyr, dplyr,
  purrr,
  rgeos,
  ggplot2,
  RColorBrewer,
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





# Some to dos: 

# 1) find shapefile for lsoas
# 2) produce cartogram for lsoas (or see if I've done this already)
# 3) add city centroids for the cities of interest 
# 4) run analyses using 'greedy' approach below
# 5) run analyses using ttwa approach 
# 6) update 

# Shapefile for lsoa in 2001
lsoa_2001 <- read_shape(file = "shapefiles/Lower_layer_super_output_areas_(E+W)_2001_Boundaries_(Full_Clipped)_V2/LSOA_2001_EW_BFC_V2.shp")
names(lsoa_2001)



churn_chor <- function(TTWA){
  lsoa_id_by_ttwa %>% 
    filter(ttwa_nm == TTWA) %>% 
    select(lsoa, year, pop_id, pop_total) %>%
    mutate(prop_id = pop_id / pop_total) %>% 
    mutate(prop_id = ifelse(prop_id > 0.5, 0.5, prop_id)) %>% 
    select(lsoa, year, prop_id) %>% 
    spread(year, prop_id) %>% 
  append_data(
    shp = lsoa_2001, data = . ,
    key.shp = "LSOA01CD", key.data = "lsoa", ignore.na = T
  ) -> shp_jn 
  
  shp_jn <- shp_jn[!is.na(shp_jn$`2007`), ]
  shp_jn %>% 
    tm_shape(.) + 
    tm_facets(ncol = 3) + 
    tm_fill(
      col = c("2004", "2007", "2010", "2015"), 
      palette = brewer.pal(10, "Spectral"), 
      breaks = seq(0, 0.5, by = 0.05), showNA = F
    ) +
    tm_legend(legend.show = F) 
}

churn_chor("Birmingham")
churn_chor("Leeds")




# 2001 centroids
ttwa_centroids <- c(
  Birmingham = "E01009150",
  Leeds = "E01011365",
  `Sheffield and Rotherham` = "E01008070",
  Bradford = "E01010844",
  Liverpool = "E01006650",
  Manchester = "E01005131",
  Bristol = "E01014656",
  `Wakefield and Castleford` = "E01011920",
  Coventry = "E01009642",
  Leicester = "E01013644",
  Nottingham = "E01013869",
  `Newcastle and Durham` = "E01008397",
  Kingston = "E01012853",
  Brighton = "E01016952"
)



# using rgeos::gCentroid

calc_distance_to_centres <- function(shp, code_centre){
  gCentroid(shp, byid = T) %>% 
    as(., "data.frame") -> tmp
  
  data_frame(dz = as.character(shp@data$LSOA01CD), x = tmp$x, y = tmp$y) %>% 
    mutate(centre = dz == code_centre) %>% 
    mutate(distance_to_centre = ((x - x[centre])^2 + (y - y[centre])^2)^0.5) -> output
  output
}


fn <- function(val, nm){
  lsoa_2001 %>% 
    calc_distance_to_centres(., val) %>% 
    .[c(1, 5)] -> out 
  names(out) <- c("lsoa", nm)
  out
}

# Find nearest centre and distance to nearest centre
map2(ttwa_centroids, names(ttwa_centroids), fn) %>% 
  reduce(., inner_join) %>% 
  gather(place, distance, -lsoa) %>%
  arrange(lsoa) %>% 
  group_by(lsoa) %>% 
  mutate(min_distance = min(distance)) %>%
  filter(distance == min_distance) %>% 
  ungroup() %>% 
  transmute(lsoa, nearest_centre = place, distance_to_centre = distance) -> centre_distance


################################
#### Fit the model
################################
#### MCMC quantities
burnin <- 10000
n.sample <- 20000
thin <- 10
n.keep <- (n.sample - burnin)/thin


# Now to revise do_model to run all available years 

do_model <- function(place){

  lsoa_id_by_ttwa %>% 
    filter(ttwa_nm == place) %>% 
    select(year, lsoa, pop_id, pop_total) -> tmp
  
  tmp %>% 
    select(-pop_id) %>% 
    mutate(year = paste0("pop_total_", year)) %>% 
    spread(year, pop_total) -> pops
  
  tmp %>% 
    select(-pop_total) %>% 
    mutate(year = paste0("pop_id_", year)) %>% 
    spread(year, pop_id) -> incdeps
  
  popinc <- pops %>% inner_join(incdeps)
  rm(tmp, pops, incdeps)
  
  
  lsoa_2001 %>% 
    append_data(
      shp = ., data = popinc,
      key.shp = "LSOA01CD", key.data = "lsoa",
      ignore.na = T
    ) -> lsoa_city
  
  
  qtm(lsoa_city)
  browser()
  
  
  W_nb <- poly2nb(lsoa_city)
  # distance to allow the 'islands' to be included 
  W_list <- nb2listw(W_nb, style = "B")
  W <- nb2mat(W_nb, style = "B")
  n <- nrow(W)
  browser()
  

  #### Format the data
  Y.mat <- cbind(
    as.integer(lsoa_city@data$pop_id_2004),
    as.integer(lsoa_city@data$pop_id_2007), 
    as.integer(lsoa_city@data$pop_id_2010), 
    as.integer(lsoa_city@data$pop_id_2015)
  )
  
  N.mat <- cbind(
    as.integer(lsoa_city@data$pop_total_2004),
    as.integer(lsoa_city@data$pop_total_2007), 
    as.integer(lsoa_city@data$pop_total_2010), 
    as.integer(lsoa_city@data$pop_total_2015)
  )
  
  # denom_too_small <- N.mat[,5] < 1
  # N.mat[denom_too_small, 5] <- 1
  # 
  # Y.mat[denom_too_small, 5] <- 0
  
  
  
  #### Run the model
  Y <- Y.mat[,1] ; N <- N.mat[,1]
  model_01 <- S.CARleroux(Y ~ 1, trials = N, W = W, family = "binomial", burnin = burnin, n.sample = n.sample, thin = thin )  
  
  Y <- Y.mat[,2] ; N <- N.mat[,2]
  model_02 <- S.CARleroux(Y ~ 1, trials = N, W = W, family = "binomial", burnin = burnin, n.sample = n.sample, thin = thin )  
  
  Y <- Y.mat[,3] ; N <- N.mat[,3]
  model_03 <- S.CARleroux(Y ~ 1, trials = N, W = W, family = "binomial", burnin = burnin, n.sample = n.sample, thin = thin )  
  
  Y <- Y.mat[,4] ; N <- N.mat[,4]
  model_04 <- S.CARleroux(Y ~ 1, trials = N, W = W, family = "binomial", burnin = burnin, n.sample = n.sample, thin = thin )  
  

  #  model$summary.results
  
  #### Compute the coordinates and ordering from the city centre
  dist.order <- order(lsoa_city@data$distance_to_centre)
  
  #### Compute the global RCI and D
  indicators.post <- array(NA, c(n.keep,8))
  colnames(indicators.post) <- c(
    "RCI_2004", "RCI_2007","RCI_2010","RCI_2015",
    "D_2004", "D_2007", "D_2010", "D_2015"
  )
  
  all_2004 <- as.integer(N.mat[,1])
  all_2007 <- as.integer(N.mat[,2])
  all_2010 <- as.integer(N.mat[,3])
  all_2015 <- as.integer(N.mat[,4])

  
  for(i in 1:n.keep)
  {
    ## Compute the probability and fitted values for the ith posterior sample
    logit_01 <- model_01$samples$beta[i, ] + model_01$samples$phi[i, ]
    prob_01 <- exp(logit_01) / (1 + exp(logit_01))
    prob.mat_01 <- matrix(prob_01, nrow=n, byrow=TRUE)
    fitted.mat_01 <- N.mat[,1] * prob.mat_01
    # Explore from here 
    
    logit_02 <- model_02$samples$beta[i, ] + model_02$samples$phi[i, ]
    prob_02 <- exp(logit_02) / (1 + exp(logit_02))
    prob.mat_02 <- matrix(prob_02, nrow=n, byrow=TRUE)
    fitted.mat_02 <- N.mat[,2] * prob.mat_02
    
    
    logit_03 <- model_03$samples$beta[i, ] + model_03$samples$phi[i, ]
    prob_03 <- exp(logit_03) / (1 + exp(logit_03))
    prob.mat_03 <- matrix(prob_03, nrow=n, byrow=TRUE)
    fitted.mat_03 <- N.mat[,3] * prob.mat_03
    
    logit_04 <- model_04$samples$beta[i, ] + model_04$samples$phi[i, ]
    prob_04 <- exp(logit_04) / (1 + exp(logit_04))
    prob.mat_04 <- matrix(prob_04, nrow=n, byrow=TRUE)
    fitted.mat_04 <- N.mat[,4] * prob.mat_04
    

    ## Compute the RCI for both years
    indicators.post[i, 1] <- RCI(fitted.mat_01, as.integer(N.mat[,1]), dist.order)
    indicators.post[i, 2] <- RCI(fitted.mat_02, as.integer(N.mat[,2]), dist.order)
    indicators.post[i, 3] <- RCI(fitted.mat_03, as.integer(N.mat[,3]), dist.order)
    indicators.post[i, 4] <- RCI(fitted.mat_04, as.integer(N.mat[,4]), dist.order)

    ## Compute D for both years
    p_2004 <- prob.mat_01
    p_2004_av <- sum(p_2004 * all_2004) / sum(all_2004)
    indicators.post[i, 6] <- sum(all_2004 * abs(p_2004 - p_2004_av)) / (2 * sum(all_2004) * p_2004_av * (1-p_2004_av))   
    
    p_2007 <- prob.mat_02
    p_2007_av <- sum(p_2007 * all_2007) / sum(all_2007)
    indicators.post[i, 7] <- sum(all_2007 * abs(p_2007 - p_2007_av)) / (2 * sum(all_2007) * p_2007_av * (1-p_2007_av))   
    
    p_2010 <- prob.mat_03
    p_2010_av <- sum(p_2010 * all_2010) / sum(all_2010)
    indicators.post[i, 8] <- sum(all_2010 * abs(p_2010 - p_2010_av)) / (2 * sum(all_2010) * p_2010_av * (1-p_2010_av))   
    
    p_2015 <- prob.mat_04
    p_2015_av <- sum(p_2015 * all_2015) / sum(all_2015)
    indicators.post[i, 9] <- sum(all_2015 * abs(p_2015 - p_2015_av)) / (2 * sum(all_2015) * p_2015_av * (1-p_2015_av))   
    

  }
  
  indicators.post  
}




indicators_birmingham <- do_model("Birmingham") # Empty neighbour sets found
indicators_leeds <- do_model("Leeds") 


# Tidy all indicator draws 

tidy_indicators <- function(mtrx, place){
  mtrx %>% 
    data.frame() %>% 
    tbl_df %>% 
    mutate(draw = 1:dim(mtrx)[1]) %>% 
    gather(mp, value, -draw) %>% 
    mutate(place = place) %>% 
    separate(mp, into = c("measure", "period")) %>% 
    dplyr::select(place, measure, period, draw, value)
}

tind_aberdeen <- tidy_indicators(indicators_aberdeen, "Aberdeen")
tind_dundee <- tidy_indicators(indicators_dundee, "Dundee")
tind_edinburgh <- tidy_indicators(indicators_edinburgh, "Edinburgh")
tind_glasgow <- tidy_indicators(indicators_glasgow, "Glasgow")

tidy_indicators_all <- reduce(
  list(tind_aberdeen, tind_dundee, tind_edinburgh, tind_glasgow),
  bind_rows
)









# # Cartogram for 2011 
# lsoa_cart <- read_shape(file = "shapefiles/lsoa_cart/lsoa_cart.shp", current.projection = "longlat")
# #qtm(lsoa_shp)




