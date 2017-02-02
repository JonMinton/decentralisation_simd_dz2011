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
# 2011 to 2001

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






