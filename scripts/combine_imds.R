rm(list = ls())

require(pacman)

pacman::p_load(
  readr, readxl,
  stringr, tidyr, dplyr, 
  rgeos,
  ggplot2,
  tmap
)



# 2015 Data  --------------------------------------------------------------

#The aim of the data coding is to estimate the number of people income deprived in the census. This 
# appears to require combining data from file 5 and file 6. 

# Let's explore further... 

imd_num <- read_excel("data/imd/2015/File_5_ID_2015_Scores_for_the_Indices_of_Deprivation.xlsx", "ID2015 Scores")
imd_num %>% 
 .[,c(1, 6)] -> imd_num

names(imd_num) <- c("lsoa", "prop_id")

# Now the denominator

imd_denom <- read_excel("data/imd/2015/File_6_ID_2015_Population_denominators.xlsx", 
  sheet = 2, col_types = rep("text", 9), skip = 1,
  col_names = c(
    "lsoa", "lsoa_nm", "lad", "lad_nm", "pop_total", "pop_children", "pop_wage", "pop_older", "pop_wage2"
  )
)

imd_denom %>% 
  select(lsoa, pop_total) -> imd_denom

imd_denom %>% 
  mutate(pop_total = as.numeric(str_replace_all(pop_total, ",", ""))
         ) -> imd_denom


imd_2015 <- imd_num %>% inner_join(imd_denom)

imd_2015 %>% 
  mutate(pop_id = prop_id * pop_total) %>% 
  mutate(year = 2015) %>% 
  select(lsoa, year, pop_id, pop_total) -> imd_2015


# 

# 2010 income deprived ----------------------------------------------------


# The two fields to use are 
# 1) ID2010 - underlying indicators - income domain tabls 
# 2) population denominators 

imd_denom <- read_excel("data/imd/2010/1981232 - population denominators .xls", "Mid_2008")

imd_denom %>% 
  .[,c(1, 2)] -> imd_denom

names(imd_denom) <- c("lsoa", "pop_total")

imd_num <- read_excel("data/imd/2010/ID2010 - Underlying Indicators.xls", "ID 2010 Income Domain")

imd_num %>% 
  .[,c(1, 9)] -> imd_num
names(imd_num) <- c("lsoa", "pop_id")

imd_num
imd_num %>% 
  inner_join(imd_denom) %>% 
  .[,c(1,2,3)] %>% 
  mutate(prop_id = pop_id / pop_total) %>% 
  mutate(year = 2010) %>% 
  select(lsoa, year, pop_id, pop_total) -> imd_2010


# 2007 data 

imd_denom <- read_excel(
  "data/imd/2007/708341 - Population Counts.xls", "Mid-2005 Population at Risk",
  col_types = rep("text", 12), col_names = T
  )
imd_denom %>% .[,c(1, 8)] -> imd_denom
names(imd_denom) <- c("lsoa", "pop_total")

imd_denom %>% 
  mutate(pop_total = as.numeric(str_replace_all(pop_total, ",", ""))
         ) -> imd_denom


imd_num <- read_excel(
  "data/imd/2007/All data  ID 2007 29 May 2008 - Income.xls", "Income",
  col_types = rep("text", 6), col_names = T                    
                      )
imd_num %>% .[,c(1, 6)] -> imd_num
names(imd_num) <- c("lsoa", "pop_id")
imd_num %>% 
  mutate(pop_id2 = str_replace_all(pop_id, ",", ""),
         pop_id3 = str_replace_all(pop_id, "under 10", "5"),
         pop_id = as.numeric(pop_id3)
         ) %>% 
  select(lsoa, pop_id) -> imd_num

imd_num %>% 
  inner_join(imd_denom) %>% 
  mutate(year = 2007) %>% 
  select(lsoa, year, pop_id, pop_total) -> imd_2007


# 2004 data 

imd_num <- read_excel(
  "data/imd/2004/SOA levelid2004 - all imd_scores.xls", "Income"
)
imd_num %>% 
  select(lsoa = SOA, prop_id = `INCOME SCORE`) -> imd_num

imd_denom <- read_excel(
  "data/imd/2004/soalevel2001 - Population Counts.xls",
  "SOA estimates - rounded",
  skip = 3
)
imd_denom %>% 
  select(lsoa = `Lower SOA code`, pop_total = `Total population`) -> imd_denom


imd_num %>% 
  inner_join(imd_denom) %>% 
  mutate(pop_id = prop_id * pop_total) %>% 
  mutate(year = 2004) %>% 
  select(lsoa, year, pop_id, pop_total) -> imd_2004


# 2000 data 

# No comparable data 



# Approach to mapping onto 2011 lsoas using Paul Norman file --------------



postcode_links <- read_csv("data/paul_norman_file/postcode_lookups/postcode01_to_lsoa11.csv")

postcode_links %>% 
  select(
    persons = PERSONS, 
    lsoa_2001 = LSOA_CODE, 
    lsoa_2011 = LSOA11CD
    ) %>% 
  group_by(lsoa_2011) %>% 
  mutate(lsoa_total_2011 = sum(persons)) %>% 
  ungroup() %>% group_by(lsoa_2001) %>% 
  mutate(lsoa_total_2001 = sum(persons)) %>% 
  ungroup() %>% group_by(lsoa_2001, lsoa_2011) %>% 
  mutate(share_2001_pop_in_2011 = sum(lsoa_total_2011) / sum(lsoa_total_2001)) %>% 
  # ggplot(., aes(x = share_2001_pop_in_2011)) +
  # scale_x_log10() + 
  # geom_histogram()  
  ungroup() %>% 
  mutate(match_status = ifelse(
    share_2001_pop_in_2011 == 1, "1_to_1",
    ifelse(
      share_2001_pop_in_2011 < 1, "n_to_1",
      ifelse(
        share_2001_pop_in_2011 > 1, "1_to_n",
          "something is wrong"
        )
      )
    )
  ) -> postcodes_categorised
  # xtabs(~ match_status, .)

# For the 97% of values where there is a match,
# Filter these in
# Then establish lookup

postcodes_categorised %>% 
  filter(match_status == "1_to_1") %>% 
  mutate(lsoas_match = lsoa_2001 == lsoa_2011) %>% 
#  xtabs(~lsoas_match, .)
# NOT ALL LSOAS match
  # 99.8% of areas DO match
# Let's explore those areas that do not 
  # filter(lsoas_match == F) %>% 
  # View()
# For these non-matches, want to know the number of 
# unique 2011 codes for each 2001 code 
  # group_by(lsoa_2001) %>% 
  # summarise(n_unique_2011 = length(unique(lsoa_2011))) %>% 
  # xtabs(~ n_unique_2011, .)
# All 2001 have a unique 2011 code 
# We can then use this to form a 1_to_1 lookup, which will 
# mostly have identical columns in from and to column
  group_by(lsoa_2001) %>% 
  summarise(lsoa_2011 = lsoa_2011[1]) -> simple_match_lsoa_lookup


# The next simplest case will be where multiple lsoas in 2001 
# are completely represented by a single lsoa in 2011

postcodes_categorised %>% 
  filter(match_status != "1_to_1") %>% 
  group_by(lsoa_2001, lsoa_2011) %>% 
  summarise(n = sum(persons)) %>% 
  ungroup() %>% group_by(lsoa_2011) %>% 
  mutate(n_t2 = sum(n)) %>% 
  ungroup() %>% group_by(lsoa_2001) %>% 
  mutate(n_t1 = sum(n)) %>% 
  ungroup() %>% 
  mutate(t2_bigger = n_t2 > n_t1) %>% 
  # xtabs(~ t2_bigger, .)
# Around 30% have t2 > t1
  filter(t2_bigger) %>% 
  group_by(lsoa_2011) %>% 
  mutate(t1_to_t2_reweight = n_t1 / sum(n_t1)) %>% 
  # mutate(num_t1s_in_t2 = length(unique(lsoa_2001))) %>% 
  # mutate(sum_t1_to_t2_reweight = sum(t1_to_t2_reweight)) %>% 
  # select(lsoa_2001, lsoa_2011, num_t1s_in_t2, t1_to_t2_reweight, sum_t1_to_t2_reweight) %>% 
  # View()
  filter(!is.na(lsoa_2001), !is.na(lsoa_2011), !is.na(t1_to_t2_reweight)) %>% 
  ungroup() %>% 
  select(lsoa_2001, lsoa_2011, t1_to_t2_reweight) -> reweight_from_lsoa_2001_to_lsoa_2011


# What proportion of total lsoa_2011s have now been covered? 

length(unique(reweight_from_lsoa_2001_to_lsoa_2011$lsoa_2011))
#212
length(unique(simple_match_lsoa_lookup$lsoa_2011))
#33554

length(unique(postcodes_categorised$lsoa_2011))
#34707 

# So around 97.3% covered 

# What about cases where there is mixed apportioning? 

postcodes_categorised %>% 
  filter(match_status != "1_to_1") %>% 
  group_by(lsoa_2001, lsoa_2011) %>% 
  summarise(n = sum(persons)) %>% 
  ungroup() %>% group_by(lsoa_2011) %>% 
  mutate(n_t2 = sum(n)) %>% 
  ungroup() %>% group_by(lsoa_2001) %>% 
  mutate(n_t1 = sum(n)) %>% 
  ungroup() %>% 
  filter(!(lsoa_2011 %in% unique(reweight_from_lsoa_2001_to_lsoa_2011$lsoa_2011))) %>% 
  # View()
   xtabs(n ~ lsoa_2001 + lsoa_2011, .) %>% 
  as.data.frame() %>% tbl_df %>% 
  filter(Freq != 0) %>% 
  rename(n = Freq) %>% 
  arrange(lsoa_2011) %>% 
#  View()
  group_by(lsoa_2001) %>% 
  mutate(n_t1 = sum(n)) %>% 
  mutate(prop_t1_in_t2 = n / n_t1) %>% 
  # View()
  rename(weight_t1_to_t2 = prop_t1_in_t2) %>% 
  select(lsoa_2001, lsoa_2011, weight_t1_to_t2) -> mixed_reweight_from_lsoa_2001_to_lsoa_2011



# Now to do some matching and reweighting 

imd_2004 %>% 
  bind_rows(imd_2007) %>% 
  bind_rows(imd_2010) %>% 
  left_join(simple_match_lsoa_lookup, by = c("lsoa" = "lsoa_2001")) %>% 
  filter(!is.na(lsoa_2011)) %>% 
  select(lsoa_2011, year, pop_id, pop_total) -> imd_simple_matches

imd_2004 %>% 
  bind_rows(imd_2007) %>% 
  bind_rows(imd_2010) %>% 
  left_join(simple_match_lsoa_lookup, by = c("lsoa" = "lsoa_2001")) %>% 
  filter(is.na(lsoa_2011)) %>% 
  select(lsoa_2001 = lsoa, year, pop_id, pop_total) -> imd_harder_matches

imd_harder_matches %>% 
  left_join(reweight_from_lsoa_2001_to_lsoa_2011) %>% 
  filter(!is.na(lsoa_2011)) %>% 
  mutate(pop_id_rwt = pop_id * t1_to_t2_reweight) %>% 
  mutate(pop_total_rwt = pop_total * t1_to_t2_reweight) %>% 
  # group_by(year) %>% 
  # summarise(pop_total = sum(pop_total))
#   # A tibble: 3 × 2
#   year pop_total
# <dbl>     <dbl>
#   1  2004    574700
# 2  2007    577620
# 3  2010    566478
  group_by(year, lsoa_2011) %>% 
  summarise(
    pop_id = sum(pop_id_rwt),
    pop_total = sum(pop_total_rwt)
            ) %>% 
  # group_by(year) %>% 
  # summarise(pop_total = sum(pop_total))
# # A tibble: 3 × 2
# year pop_total
# <dbl>     <dbl>
#   1  2004  287989.5
# 2  2007  289143.9
# 3  2010  284380.9
# Around half the size - is this due to an error
# or a genuine resizing of lsoas covered?
  ungroup() %>% 
  select(lsoa_2011, year, pop_id, pop_total) -> reweighted_harder_matches_1


# Now the other chunk
imd_harder_matches %>% 
  left_join(mixed_reweight_from_lsoa_2001_to_lsoa_2011) %>% 
  filter(!is.na(lsoa_2011)) %>% 
  mutate(pop_id_rwt = pop_id * weight_t1_to_t2) %>% 
  mutate(pop_total_rwt = pop_total * weight_t1_to_t2) %>% 
  # group_by(year) %>%
  # summarise(pop_total = sum(pop_total))
# # A tibble: 3 × 2
# year pop_total
# <dbl>     <dbl>
#   1  2004   1488300
# 2  2007   2043894
# 3  2010   2587116
  group_by(year, lsoa_2011) %>% 
  summarise(
    pop_id = sum(pop_id_rwt),
    pop_total = sum(pop_total_rwt)
  ) %>% 
  # group_by(year) %>%
  # summarise(pop_total = sum(pop_total))
# # A tibble: 3 × 2
# year pop_total
# <dbl>     <dbl>
#   1  2004    708330
# 2  2007    917817
# 3  2010   1119500
  # Again, around half 1/3rd the size - is this due to an error
  # or a genuine resizing of lsoas covered?
  ungroup() %>% 
  select(lsoa_2011, year, pop_id, pop_total) -> reweighted_harder_matches_2

imd_simple_matches %>% 
  bind_rows(reweighted_harder_matches_1) %>% 
  bind_rows(reweighted_harder_matches_2) -> all_matched_and_reweighted_on_lsoa_2011

# Are all lsoas in each year unique?
all_matched_and_reweighted_on_lsoa_2011 %>%
  # group_by(year) %>% 
  # summarise(
  #   nunique = length(unique(lsoa_2011)), 
  #           nrow = length(lsoa_2011)
  #   )
# The number of LSOAs is equal  to the number of unique LSOAs in 2004 and 2007
# But for 2010 there are 
# 32574 unique IDs and 32680 rows 
# I will reaggregate for this reason
  group_by(year, lsoa_2011) %>% 
  summarise(
    pop_id = sum(pop_id),
    pop_total = sum(pop_total)
  ) %>% ungroup() -> all_matched_and_reweighted_on_2011_reaggregated
# From 98272 rows to 98166 rows

# Compare population totals in the original and final(?) reaggregated


imd_2004 %>%
  bind_rows(imd_2007) %>%
  bind_rows(imd_2010) %>%
  group_by(year) %>%
  summarise(
    pop_id = sum(pop_id),
    pop_total = sum(pop_total)
  ) %>% 
  mutate(source = "original") -> tmp1
# # A tibble: 3 × 3
# year  pop_id pop_total
# <dbl>   <dbl>     <dbl>
#   1  2004 6837516  49345520
# 2  2007 7833920  50427759
# 3  2010 7481830  51238293


all_matched_and_reweighted_on_2011_reaggregated %>% 
  group_by(year) %>% 
  summarise(
    pop_id = sum(pop_id),
    pop_total = sum(pop_total)
  ) %>% 
  mutate(source = "modified") -> tmp2

bind_rows(tmp1, tmp2) %>% 
  select(-pop_total) %>% 
  spread(source, pop_id) %>% 
  mutate(
    dif_id = modified / original)
# Differences are under 1% 

bind_rows(tmp1, tmp2) %>% 
  select(-pop_id) %>% 
  spread(source, pop_total) %>% 
  mutate(
    dif_total = modified / original)
# Differences are under 0.5%

all_matched_and_reweighted_on_2011_reaggregated %>% 
  select(lsoa = lsoa_2011, year, pop_id, pop_total) %>% 
  bind_rows(imd_2015) -> imd_allyears
  # group_by(year) %>% 
  # summarise(pop_id = sum(pop_id), pop_total = sum(pop_total))


write_csv(imd_allyears, "data/imd/imd_id_lsoa2011_tidied.csv")






