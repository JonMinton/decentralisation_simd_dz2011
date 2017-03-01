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
    dif_id = modified / original, 
    prop_dif_id = dif_id / original)
# Differences are under 1% 

bind_rows(tmp1, tmp2) %>% 
  select(-pop_id) %>% 
  spread(source, pop_total) %>% 
  mutate(
    dif_total = modified / original, 
    prop_dif_total = dif_total / original)
# Differences are under 0.5%





# Data Stitching code from Gavin ------------------------------------------
# 2011 to 2001

# changes of LSOAs boundaries
changes.boundary <- read_csv("data/england_lookup/LSOA01_LSOA11_LAD11_EW_LU.csv")  %>%
  select(LSOA01CD,LSOA11CD,CHGIND)


changes.boundary.1 <- changes.boundary %>% 
  left_join(imd_2015,by = c("LSOA11CD" = "lsoa")) 


### we use LSOA boundary in 2001 as baseline geographical units
# case 1; split--one lsoa was split into two or more lsoa in 2011. in this case, aggregate the COB data in 2011 
# based on 2001 lsoas
#changes.boundary.1 %>% filter(CHGIND == "S")
#grepl("*.2011",names(changes.boundary.2))

data.split <- changes.boundary.1 %>% filter(CHGIND == "S") %>%
  select(LSOA01CD, pop_id, pop_total) %>% 
  group_by(LSOA01CD) %>%
  summarise(pop_id = sum(pop_id), pop_total = sum(pop_total))


# link back to the
s.ind <- changes.boundary.1$CHGIND =="S"
match.ind <- match(changes.boundary.1$LSOA01CD[s.ind],data.split$LSOA01CD)
changes.boundary.1[s.ind,c("pop_id","pop_total")] <- data.split[match.ind,2:3]


# case 2: two or more LSOAs were merged into a single LSOA in 2011. 
# in this case we need aggregate the 2001 LSOAs accordingly.
changes.boundary.1 %>% filter(CHGIND == "M")

data.merge <- changes.boundary.1 %>% filter(CHGIND == "M") %>%
  select(LSOA11CD,pop_id, pop_total) %>%
  group_by(LSOA11CD) %>%
  summarise(pop_id = sum(pop_id), pop_total = sum(pop_total))


# link back
m.ind <- changes.boundary.1$CHGIND =="M"
match.ind <- match(changes.boundary.1$LSOA11CD[m.ind],data.merge$LSOA11CD)
changes.boundary.1[m.ind,names(data.merge)[-1]] <- data.merge[match.ind,2:3]


# select unique ids of LSOAs in 2001
imd_2015_on2001 <- changes.boundary.1[!duplicated(changes.boundary.1$LSOA01CD),] %>% tbl_df


# drop complex boundary changes, they only account for a very small proportion of the data
#COB.data <- COB.data[!COB.data$CHGIND == "X",]


imd_2015_on2001 <- imd_2015_on2001[!imd_2015_on2001$CHGIND == "X",] %>% tbl_df

imd_2015_on2001 %>% 
  select(lsoa = LSOA01CD, year, pop_id, pop_total) -> imd_2015_on2001

# Combine found data 

imd_allyears <- bind_rows(
  imd_2004,
  imd_2007,
  imd_2010,
  imd_2015_on2001
) %>% 
  group_by(year, lsoa) %>% 
  summarise(pop_id = sum(pop_id), pop_total = sum(pop_total)) %>% 
  arrange(year, lsoa) %>% 
  ungroup()


# Problem: 2015 indicators on new LSOAs 

# Lookup needed 

write_csv(x = imd_allyears, path =  "data/imd/imd_id_tidied.csv")



# Reverse data stitching 
# 2001 to 2011 

# Data Stitching code from Gavin ------------------------------------------
# 2011 to 2001

# changes of LSOAs boundaries
changes.boundary <- read_csv("data/england_lookup/LSOA01_LSOA11_LAD11_EW_LU.csv")  %>%
  select(LSOA01CD,LSOA11CD,CHGIND)


changes.boundary %>% 
  group_by(LSOA01CD, LSOA11CD, CHGIND) %>% 
  tally() %>% 
  group_by(CHGIND) %>% 
  summarise(min_n = min(n), max_n = max(n))

pop_t1 <- imd_allyears %>%   
  filter(year == 2004) %>% 
  select(LSOA01CD = lsoa, pop_t1 = pop_total) 


pop_t2 <- imd_2015 %>% 
  filter(year == 2015) %>% 
  select(LSOA11CD = lsoa, pop_t2 = pop_total)

changes.boundary <- changes.boundary %>% 
  inner_join(pop_t1) %>% 
  inner_join(pop_t2) 


# Code == M 
# Merge: 2 + t1 -> 1 t2

change_m <- changes.boundary %>% filter(CHGIND == "M")


# Code == S 
# Split: 1 t2 -> 2+ t2

change_s <- changes.boundary %>% filter(CHGIND == "S")

# Code == U
# Unchanged: 1 t1 => 1 t2

change_u <- changes.boundary %>% filter(CHGIND == "U")

# Code == X 
# DROP!


# Case where change == M

# Find t1_t2_reweight 

change_m <- change_m %>% 
  group_by(LSOA11CD) %>% 
  mutate(t1_t2_reweight = pop_t1 / sum(pop_t1)) %>% 
  ungroup()

# Case where change == S
# find t2_t1_reweight
change_s <- change_s %>% 
  group_by(LSOA01CD) %>% 
  mutate(t2_t1_reweight = pop_t2 / sum(pop_t2))


# Reweighting in case where change == M 
imd_allyears %>% 
  right_join(change_m, by = c("lsoa" = "LSOA01CD")) %>% 
  filter(year != 2015) %>% 
  mutate(pop_id_rwt = pop_id * t1_t2_reweight) %>% 
  mutate(pop_total_rwt = pop_total * t1_t2_reweight) %>% 
  select(year, lsoa = LSOA11CD, pop_id_rwt, pop_total_rwt) %>% 
  group_by(year, lsoa) %>% 
  summarise(pop_id = sum(pop_id_rwt), pop_total = sum(pop_total_rwt)) %>% 
  ungroup() -> imd_earlier_on_lsoa2011_m

# Splitting in case where change == S 

imd_allyears %>% 
  right_join(change_s, by = c("lsoa" = "LSOA01CD")) %>% 
  filter(year != 2015) %>% 
  mutate(pop_id_rwt = pop_id * t2_t1_reweight) %>% 
  mutate(pop_total_rwt = pop_total * t2_t1_reweight) %>% 
  select(
    year, lsoa = LSOA11CD, pop_id = pop_id_rwt, pop_total = pop_total_rwt
  ) -> imd_earlier_on_lsoa2011_s

# sense check 
imd_earlier_on_lsoa2011_s %>% 
  group_by(year, lsoa) %>% 
  tally() %>% 
  xtabs(~n, .)
# One row per year, lsoa combination - good 


# Now the (hopefully) trivially easy one : 
# Case where change  == U

imd_allyears %>% 
  right_join(change_u, by = c("lsoa" = "LSOA01CD")) %>% 
  filter(year != 2015) %>% 
  select(
    year, lsoa = LSOA11CD, pop_id = pop_id, pop_total = pop_total
  ) -> imd_earlier_on_lsoa2011_u


# Now combine U, S and M (implicitly drop X)

imd_earlier_on_lsoa2011 <- bind_rows(
  imd_earlier_on_lsoa2011_m,
  imd_earlier_on_lsoa2011_s,
  imd_earlier_on_lsoa2011_u
)

# Now add values from 2015 

imd_2015 %>% 
  select(year, lsoa, pop_id, pop_total) %>% 
  bind_rows(imd_earlier_on_lsoa2011) %>% 
  arrange(
    year, lsoa
  ) -> imd_all_on_lsoa2011


write_csv(imd_all_on_lsoa2011, "data/imd/imd_id_lsoa2011_tidied.csv")






