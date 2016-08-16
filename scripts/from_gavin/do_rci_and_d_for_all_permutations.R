

################################
#### Fit the model
################################
#### MCMC quantities
burnin <- 10000
n.sample <- 20000
thin <- 10
n.keep <- (n.sample - burnin)/thin


# Now to revise do_model to run all available years 

do_model <- function(place, initial_dist = 18000){
  
  this_dist <- initial_dist
  has_islands <- T
  
  while(has_islands){
    dz_2011  %>% 
      append_data(
        shp = . , data = centre_distance, 
        key.shp = "DataZone", key.data = "datazone"
      ) %>% 
      .[.$nearest_centre == place,] %>% 
      .[.$distance_to_centre <= this_dist,] -> dz_city
    # plot(dz_city, main = this_dist)
    # browser()
    
    w <-  poly2nb(dz_city)   
    if (any(card(w) == 0)){
      this_dist <- this_dist + 1000
    } else {
      has_islands <- F
    }
  }
  
  simd_2011_reweighted %>% 
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
  
  
  W_nb <- poly2nb(dz_city)
  # distance to allow the 'islands' to be included 
  W_list <- nb2listw(W_nb, style = "B")
  W <- nb2mat(W_nb, style = "B")
  n <- nrow(W)
  
  # Need to remove unconnected datazones ('islands')
  
  
  
  #### Format the data
  Y.mat <- cbind(
    as.integer(dz_city@data$pop_id_2004),
    as.integer(dz_city@data$pop_id_2006), 
    as.integer(dz_city@data$pop_id_2009), 
    as.integer(dz_city@data$pop_id_2012) 
  )
  
  N.mat <- cbind(
    as.integer(dz_city@data$pop_total_2004),
    as.integer(dz_city@data$pop_total_2006), 
    as.integer(dz_city@data$pop_total_2009), 
    as.integer(dz_city@data$pop_total_2012) 
  )


  
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
  dist.order <- order(dz_city@data$distance_to_centre)
  
  #### Compute the global RCI and D
  indicators.post <- array(NA, c(n.keep,8))
  colnames(indicators.post) <- c(
    "RCI_2004", "RCI_2006","RCI_2009","RCI_2012", 
    "D_2004", "D_2006", "D_2009", "D_2012"
  )
  
  all_2004 <- as.integer(N.mat[,1])
  all_2006 <- as.integer(N.mat[,2])
  all_2009 <- as.integer(N.mat[,3])
  all_2012 <- as.integer(N.mat[,4])
  
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
    indicators.post[i, 5] <- sum(all_2004 * abs(p_2004 - p_2004_av)) / (2 * sum(all_2004) * p_2004_av * (1-p_2004_av))   
    
    p_2006 <- prob.mat_02
    p_2006_av <- sum(p_2006 * all_2006) / sum(all_2006)
    indicators.post[i, 6] <- sum(all_2006 * abs(p_2006 - p_2006_av)) / (2 * sum(all_2006) * p_2006_av * (1-p_2006_av))   
    
    p_2009 <- prob.mat_03
    p_2009_av <- sum(p_2009 * all_2009) / sum(all_2009)
    indicators.post[i, 7] <- sum(all_2009 * abs(p_2009 - p_2009_av)) / (2 * sum(all_2009) * p_2009_av * (1-p_2009_av))   
    
    p_2012 <- prob.mat_04
    p_2012_av <- sum(p_2012 * all_2012) / sum(all_2012)
    indicators.post[i, 8] <- sum(all_2012 * abs(p_2012 - p_2012_av)) / (2 * sum(all_2012) * p_2012_av * (1-p_2012_av))   
    
    
  }
  
  indicators.post  
}




indicators_aberdeen <- do_model("Aberdeen")
indicators_dundee <- do_model("Dundee")
indicators_edinburgh <- do_model("Edinburgh")
indicators_glasgow <- do_model("Glasgow")

# Tidy all indicator draws 

tidy_indicators <- function(mtrx, place){
  mtrx %>% 
    data.frame() %>% 
    tbl_df %>% 
    mutate(draw = 1:dim(mtrx)[1]) %>% 
    gather(mp, value, -draw) %>% 
    mutate(place = place) %>% 
    separate(mp, into = c("measure", "period")) %>% 
    select(place, measure, period, draw, value)
}

tind_aberdeen <- tidy_indicators(indicators_aberdeen, "Aberdeen")
tind_dundee <- tidy_indicators(indicators_dundee, "Dundee")
tind_edinburgh <- tidy_indicators(indicators_edinburgh, "Edinburgh")
tind_glasgow <- tidy_indicators(indicators_glasgow, "Glasgow")

tidy_indicators_all <- reduce(
  list(tind_aberdeen, tind_dundee, tind_edinburgh, tind_glasgow),
  bind_rows
)


write_csv(tidy_indicators_all, path = "data/all_posterior_draws.csv")
