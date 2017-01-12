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



# TO DO -------------------------------------------------------------------

# same for 2007 and 2004





imd_2015 %>% 
  mutate(pop_id = prop_id * pop_total) %>% 
  mutate(year = 2015) %>% 
  select(lsoa, year, pop_id, pop_total) -> imd_2015

# Load IMDs --------------------------------------------------------------

imd_2015 <- read_csv(
  "data/imd/File_7_ID_2015_All_ranks__deciles_and_scores_for_the_Indices_of_Deprivation__and_population_denominators.csv"
  )

imd_2015 %>% 
  select(
    lsoa = LSOA_code_2011,
    la_name = Local_Authority_District_name_2013,
    imd_score = Index_of_Multiple_Deprivation_IMD_Score
    ) %>% 
  ggplot(aes(x = imd_score, group = la_name, fill = la_name)) +
  geom_density(alpha = 0.2) + 
  guides(fill = F)

# Where is the place with just one IMD score? 
imd_2015 %>% 
  select(
    lsoa = LSOA_code_2011,
    la_name = Local_Authority_District_name_2013,
    imd_score = Index_of_Multiple_Deprivation_IMD_Score
  ) %>% 
  group_by(la_name) %>% 
  summarise(n = n()) %>% 
  arrange(n)
# The Isles of Scilly
# Remove Isles of Scilly & City of London
imd_2015 %>% 
  select(
    lsoa = LSOA_code_2011,
    la_name = Local_Authority_District_name_2013,
    imd_score = Index_of_Multiple_Deprivation_IMD_Score
  ) %>% 
  filter(!la_name %in% c("Isles of Scilly", "City of London")) %>% 
  ggplot(aes(x = imd_score, group = la_name, fill = la_name)) +
  geom_density(alpha = 0.2, colour = NA) + 
  guides(fill = F)


# Now let's look at what we have for other years

# For 2010,
# Each sheet after Notes has to be read in, and linked to through LSOA code

sheets_10 <- excel_sheets("data/imd/ID2010 - IMD and domains.xls")
sheets_10 <- sheets_10[-1] # first one is notes 
imd_10_list <- lapply(
  sheets_10,
  function(x){
    read_excel(
      path = "data/imd/ID2010 - IMD and domains.xls",
      sheet = x         
    )
  }
)

imd_10 <- Reduce(
    inner_join, 
    imd_10_list
)

# Now earlier years

#2007


