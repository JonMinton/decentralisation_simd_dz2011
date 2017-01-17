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


# Data Stitching code from Gavin ------------------------------------------


# changes of LSOAs boundaries
changes.boundary <- read_csv("data/england_lookup/LSOA01_LSOA11_LAD11_EW_LU.csv")  %>%
  select(LSOA01CD,LSOA11CD,CHGIND)


changes.boundary.1 <- changes.boundary %>% left_join(imd_2015,by = c("LSOA11CD" = "lsoa")) 


### we use LSOA boundary in 2001 as baseline geographical units
# case 1; split--one lsoa was split into two or more lsoa in 2011. in this case, aggregate the COB data in 2011 
# based on 2001 lsoas
changes.boundary.1 %>% filter(CHGIND == "S")
#grepl("*.2011",names(changes.boundary.2))

data.split <- changes.boundary.1 %>% filter(CHGIND == "S") %>%
  select(LSOA01CD, pop_id, pop_total) %>% 
  group_by(LSOA01CD) %>%
  summarise(pop_id = sum(pop_id), pop_total = sum(pop_total))


# link back to the
s.ind <- changes.boundary.1$CHGIND =="S"
match.ind <- match(changes.boundary.1$LSOA01CD[s.ind],data.split$LSOA01CD)
changes.boundary.1[s.ind,c("pop_id","pop_total")] <- data.split[match.ind,2:3]

# case 2: two or more LSOAs were merged into a single LSOA in 2011. in this case we need aggregate the 2001 LSOAs accordingly.
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
imd_2015_on2001 <- changes.boundary.1[!duplicated(changes.boundary.1$LSOA01CD),] %>% data.frame

# drop complex boundary changes, they only account for a very small proportion of the data
#COB.data <- COB.data[!COB.data$CHGIND == "X",]
COB.data %>% tbl_df() %>% filter(ttwa %in% c("London","Manchester","Sheffield & Rotherham")) %>%
  filter(CHGIND == "X") %>%
  group_by(ttwa) %>%
  summarise(num=length(ttwa))

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

# test for errors in 2007, 2010, 2015 -------------------------------------
# 
# # Any numerators > denoms? 
# 
# imd_allyears %>% 
#   mutate(has_error = pop_id > pop_total) %>% 
#   filter(has_error)
# 
# # No errors of this sort 
# 
# # Now some descriptive stats 
# 
# # Can only compare 2007 and 2010
# # proportion in each area income deprived 
# 
# imd_allyears %>% 
#   filter(year %in% c(2007, 2010)) %>% 
#   mutate(prop_id = pop_id / pop_total) %>% 
#   group_by(year) %>% 
#   arrange(prop_id)
# 
# 
# # Variation between two years 
# 
# imd_allyears %>% 
# #  filter(year %in% c(2007, 2010)) %>% 
#   mutate(prop_id = pop_id / pop_total) %>% 
#   select(lsoa, year, prop_id) %>% 
#   mutate(year = paste("y", year, sep = "_")) %>%
#   spread(year, prop_id) %>% 
#   ggplot(., aes(x = y_2010, y = y_2015)) +
#   geom_point(shape = ".", alpha = 0.3) + 
#   coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = F) + 
#   geom_abline(slope = 1, intercept = 0)
# 
# # Some evidence of fall in ID proportions 
# 
# 
# 
# 
# 
# 
# 
# imd_allyears %>% 
#   filter(year == 2007) -> tmp
# unique(tmp$lsoa) %>% length()
# 
# 
#   