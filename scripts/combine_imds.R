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

imd_denom <- read_excel("data/imd/2015/File_6_ID_2015_Population_denominators.xlsx", sheet = 2)

imd_denom %>% 
  .[,c(1, 5)] -> imd_denom

names(imd_denom) <- c("lsoa", "pop_total") 

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

imd_denom <- read_excel("data/imd/2007/708341 - Population Counts.xls", "Mid-2005 Population at Risk")
imd_denom %>% .[,c(1, 8)] -> imd_denom
names(imd_denom) <- c("lsoa", "pop_total")

imd_num <- read_excel("data/imd/2007/All data  ID 2007 29 May 2008 - Income.xls", "Income")
imd_num %>% .[,c(1, 6)] -> imd_num
names(imd_num) <- c("lsoa", "pop_id")

imd_num %>% 
  inner_join(imd_denom) %>% 
  mutate(year = 2007) %>% 
  select(lsoa, year, pop_id, pop_total) -> imd_2007


# 2004 data 

# No comparable data 


# 2000 data 

# No comparable data 


# Combine found data 

imd_allyears <- bind_rows(
  imd_2007,
  imd_2010,
  imd_2015
) %>% 
  group_by(year, lsoa) %>% 
  summarise(pop_id = sum(pop_id), pop_total = sum(pop_total)) %>% 
  arrange(year, lsoa) %>% 
  ungroup()


# Problem: 2015 indicators on new LSOAs 

# Lookup needed 

write_csv(x = imd_allyears, path =  "data/imd/imd_id_tidied.csv")

# test for errors in 2007, 2010, 2015 -------------------------------------

# Any numerators > denoms? 

imd_allyears %>% 
  mutate(has_error = pop_id > pop_total) %>% 
  filter(has_error)

# No errors of this sort 

# Now some descriptive stats 

# Can only compare 2007 and 2010
# proportion in each area income deprived 

imd_allyears %>% 
  filter(year %in% c(2007, 2010)) %>% 
  mutate(prop_id = pop_id / pop_total) %>% 
  group_by(year) %>% 
  arrange(prop_id)


# Variation between two years 

imd_allyears %>% 
#  filter(year %in% c(2007, 2010)) %>% 
  mutate(prop_id = pop_id / pop_total) %>% 
  select(lsoa, year, prop_id) %>% 
  mutate(year = paste("y", year, sep = "_")) %>%
  spread(year, prop_id) %>% 
  ggplot(., aes(x = y_2010, y = y_2015)) +
  geom_point(shape = ".", alpha = 0.3) + 
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = F) + 
  geom_abline(slope = 1, intercept = 0)

# Some evidence of fall in ID proportions 







imd_allyears %>% 
  filter(year == 2007) -> tmp
unique(tmp$lsoa) %>% length()


  