# Regressions -------------------------------------------------------------


# The purpose of this section is 
# 1) produce a comparable estimate of the gradient of 
# change in poverty against distance for each place 
# 2) produce gradients of change in total population 
# 3) produce gradients of change in income deprived population 

manage_propid_regression <- function(this_city){
  simd_2011_reweighted  %>% 
    ungroup() %>%  
    left_join(centre_distance, by = c("dz_2011" = "datazone")) %>% 
    filter(nearest_centre == this_city) %>%
    distinct() %>% 
    select(dz_2011, distance_to_centre, year, pop_incomedeprived, pop_total) %>% 
    mutate(distance_to_centre = distance_to_centre / 1000) %>% 
    mutate(prop_id = pop_incomedeprived / pop_total) %>% 
    filter(distance_to_centre >= 1.0, distance_to_centre < 12.0) %>% 
    select(dz_2011, year, distance_to_centre, prop_id) %>% 
    spread(year, prop_id) %>% 
    mutate(change = `2012` - `2004`) %>% 
    lm(change ~ distance_to_centre, .) %>% 
    tidy() %>% 
    mutate(place = this_city) %>% 
    select(place, everything())
}

c("Aberdeen", "Dundee", "Edinburgh", "Glasgow") %>% 
  map_df(.f = manage_propid_regression) -> propid_regression

manage_numerator_regression <- function(this_city){
  simd_2011_reweighted  %>% 
    ungroup() %>%  
    left_join(centre_distance, by = c("dz_2011" = "datazone")) %>% 
    filter(nearest_centre == this_city) %>%
    distinct() %>% 
    select(dz_2011, distance_to_centre, year, pop_incomedeprived) %>% 
    mutate(distance_to_centre = distance_to_centre / 1000) %>% 
    filter(distance_to_centre >= 1.0, distance_to_centre < 12.0) %>% 
    select(dz_2011, year, distance_to_centre, pop_incomedeprived) %>% 
    spread(year, pop_incomedeprived) %>% 
    mutate(`2012` = `2012` + 0.5, `2004` = `2004` + 0.5) %>% # Continuity correction
    mutate(change = (`2012` - `2004`)/`2004`) %>% 
    lm(change ~ distance_to_centre, .) %>% 
    tidy() %>% 
    mutate(place = this_city) %>% 
    select(place, everything())
}

c("Aberdeen", "Dundee", "Edinburgh", "Glasgow") %>% 
  map_df(.f = manage_numerator_regression) -> numerator_regression

manage_denominator_regression <- function(this_city){
  simd_2011_reweighted  %>% 
    ungroup() %>%  
    left_join(centre_distance, by = c("dz_2011" = "datazone")) %>% 
    filter(nearest_centre == this_city) %>%
    distinct() %>% 
    select(dz_2011, distance_to_centre, year, pop_total) %>% 
    mutate(distance_to_centre = distance_to_centre / 1000) %>% 
    filter(distance_to_centre >= 1.0, distance_to_centre < 12.0) %>% 
    select(dz_2011, year, distance_to_centre, pop_total) %>% 
    spread(year, pop_total) %>% 
    mutate(`2012` = `2012` + 0.5, `2004` = `2004` + 0.5) %>% # Continuity correction
    mutate(change = (`2012` - `2004`)/`2004`) %>% 
    lm(change ~ distance_to_centre, .) %>% 
    tidy() %>% 
    mutate(place = this_city) %>% 
    select(place, everything())
}

c("Aberdeen", "Dundee", "Edinburgh", "Glasgow") %>% 
  map_df(.f = manage_denominator_regression) -> denominator_regression



# Plot these 

numerator_regression %>% 
  filter(term == "distance_to_centre") %>% 
  select(place, estimate) %>% 
  ggplot(.) + 
  geom_bar(aes(x = place, y = estimate), stat = "identity")


denominator_regression %>% 
  filter(term == "distance_to_centre") %>% 
  select(place, estimate) %>% 
  ggplot(.) + 
  geom_bar(aes(x = place, y = estimate), stat = "identity")


# Combine into a single dataset 

tmp1 <- numerator_regression %>% 
  mutate(type = "numerator") %>% 
  mutate(stat_sig = p.value < 0.05) %>% 
  filter(term == "distance_to_centre") %>% 
  select(type, place, estimate, stat_sig)


tmp2 <- denominator_regression %>% 
  mutate(type = "denominator") %>% 
  mutate(stat_sig = p.value < 0.05) %>% 
  filter(term == "distance_to_centre") %>% 
  select(type, place, estimate, stat_sig)

tmp3 <- propid_regression %>% 
  mutate(type = "propid") %>% 
  mutate(stat_sig = p.value < 0.05) %>% 
  filter(term == "distance_to_centre") %>% 
  select(type, place, estimate, stat_sig)

simple_results <- bind_rows(tmp1, tmp2, tmp3)
rm(tmp1, tmp2, tmp3)
