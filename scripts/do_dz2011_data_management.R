
# SIMD combined 

simd_combined <- read_csv("data/simd/simd_combined.csv")
# Paul Norman Lookup
lkup <-  read_csv("data/paul_norman_file/paul_norman_dz2011_table.csv")

# simd 2016 already dz2011, so exclude from reweighting below

simd_older_combined <- simd_combined %>% filter(year != 2016)
# Produce reweighted SIMD scores
lkup  %>% 
  dplyr::select(dz_2001, dz_2011)  %>% 
  arrange(dz_2001, dz_2011)  %>%  
  group_by(dz_2001, dz_2011)  %>% 
  tally  %>% # produce n, giving number of OAs which contain particular groupings of dz_2001 and dz_2011
  arrange(dz_2011, dz_2001)  %>% 
  dplyr::select(dz_2011, dz_2001, n)  %>% 
  group_by(dz_2011)   %>% 
  arrange(dz_2011, dz_2001) %>% 
  mutate(weight = n / sum(n))   %>% # Weighting by dz_2011
  left_join(simd_older_combined, by = c("dz_2001" = "datazone"))  %>% 
  dplyr::select(-simd_rank)  %>%  # Not meaningful to reweight rank
  group_by(dz_2011, year) %>% 
  summarise(
    pop_total = sum(pop_total * weight),
    pop_workingage = sum(pop_workingage * weight), 
    pop_incomedeprived = sum(pop_incomedeprived * weight)
  ) -> simd_2011_reweighted

            
simd_2016_simplified <- simd_combined %>% 
  filter(year == 2016) %>% 
  dplyr::select(dz_2011 = datazone, year, pop_total, pop_workingage, pop_incomedeprived)

simd_2011_reweighted <- bind_rows(simd_2011_reweighted, simd_2016_simplified)

write_csv(x = simd_2011_reweighted, path = "data/simd/simd_combined_on_2011.csv")




