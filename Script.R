rm(list = ls())

require(pacman)

pacman::p_load(
  readr, readxl,
  stringr, tidyr, dplyr, 
  ggplot2,
  tmap
)


# Load SIMDs --------------------------------------------------------------

simd_2012 <- read_excel(path = "data/simd/00410767.xls")
# SIMD 2012
# 00410767.xls

# SIMD 2009
# 0102096.xls
simd_2009 <- read_excel(path = "data/simd/0102096.xls")

# SIMD 2006
# 0041675.xls
simd_2006 <- read_excel(path = "data/simd/0041675.xls")

# SIMD 2004
# 0027000.xls
simd_2004 <- read_excel(path = "data/simd/0027000.xls")


# Key attributes required: 
# population, 
# SIMD score
# SIMD rank
# Income deprivation % 

simd_2004 %>% 
  select(
    datazone = `Data Zone`,
    pop_total = `Total Population (2001 Census)`, 
    pop_workingage = `Working Age Population (men 16-64, women 16-59, 2001 Census)`,
    pop_incomedeprived = `Number of Current Income Deprived`,
    simd_score = `SIMD Score`,
    simd_rank = `SIMD Rank`
    ) %>% 
  mutate(year = 2004) %>% 
  select(datazone, year, everything()) -> simd_2004_simple

simd_2006 %>% 
  select(
    datazone = `Data Zone`,
    pop_total = `Total Population (SAPE 2004)`, 
    pop_workingage = `Working Age Population (men 16-64, women 16-59 SAPE 2004)`,
    pop_incomedeprived = `Number of Current Income Deprived People 2006`,
    simd_score = `SIMD 2006 Score`,
    simd_rank = `SIMD 2006 Rank`
  ) %>% 
  mutate(year = 2006) %>% 
  select(datazone, year, everything()) -> simd_2006_simple

simd_2009 %>% 
  select(
    datazone = `Data Zone`, 
    pop_total = `Total Population (SAPE 2007)`, 
    pop_workingage = `Working Age Population (men 16-64, women 16-59 SAPE 2007)`, 
    pop_incomedeprived = `Number of Income Deprived People 2009 V2 (Revised 19/07/10)`, 
    simd_score = `SIMD 2009 V2 Score (Revised 19/07/10)`, 
    simd_rank = `SIMD 2009 V2 Rank (Revised 19/07/10)`
  ) %>% 
  mutate(year = 2009) %>% 
  select(datazone, year, everything()) -> simd_2009_simple

simd_2012 %>% 
  select(
    datazone = `Data Zone`, 
    pop_total = `Total Population (SAPE 2010)`, 
    pop_workingage =`Best-fit Working Age Population** (men 16-64, women 16-60 SAPE 2010)`, 
    pop_incomedeprived = `Number of Income Deprived People 2012`, 
    simd_score = `Overall SIMD 2012 Score`, 
    simd_rank = `Overall SIMD 2012 Rank`
  ) %>% 
  mutate(year = 2012) %>% 
  select(datazone, year, everything()) -> simd_2012_simple

# combine these 
simd_combined <- bind_rows(
  simd_2004_simple, 
  simd_2006_simple, 
  simd_2009_simple, 
  simd_2012_simple
  ) %>% 
  filter(!is.na(datazone))




# 2001 datazone shapefiles ------------------------------------------------


dz_2001 <- read_shape(file = "shapefiles/scotland_2001_datazones/scotland_dz_2001.shp")

# To visualise this
dz_2001 %>% 
  tm_shape(.) + 
  tm_borders()

# To show simd_scores 


simd_combined %>% 
  select(datazone, year, simd_score) %>%
  spread(year, simd_score) %>% 
  append_data(
    shp = dz_2001, data = . ,
    key.shp = "zonecode", key.data = "datazone"
              ) %>% 
  tm_shape(.) + 
  tm_fill(col = c("2004", "2006", "2009", "2012"))

  # Which of course says don't live in Glasgow...



# 
# # 
# # Gwilym/Jon/Gavin (cc. Mark/Mirjam)
# # 
# # I'd be keen to plan a timetable for this work, and agreeing how we will try to use the data, particularly to 
# look at the question of suburbanisation of poverty. I don't think it is a huge amount of effort and it would be 
# good to have it set up so the full code is available through GitHub so others are able to follow and comment on 
# the process. We'd also try to make the resulting data available through UBDC. Have copied in Mark and Mirjam since 
# they may be interested in contributing or commenting on method. Steps would be as follows:
# # 
# # June/July
# # 1. Get the existing SIMD data for DZ-2001 from 2004 - should have this from the pollution paper work but need to 
#   make sure that, for each year, we have: population, SIMD score and rank, and Income Deprivation %.

# # 
# # 2. Using the old DZ boundaries, run the decentralisation code on the Income Deprived for the four cities for each year, 
# and check whether we see similar pattern to the suburbanisation paper. This also acts as a baseline against which we 
# can check the re-aggregated data.

# # 
# # 3. Agree a procedure for making our 'best' estimate of overall SIMD score/rank, and Income Deprivation, on new DZ-2011 
# boundaries. We can do better than simply looking at the nearest old DZ for each new DZ centroid by using information about 
# variations within each DZ, utilising Census data at OA level. We have: deprivation scores for OAs in 2001 (Mirjam Allik's 
# paper); and a lookup file showing how the population of each OA-2001 is apportioned to each DZ-2011 (Paul Norman's file). 
# The approach would be:
# a. use OA-2001 deprivation decile to apportion DZ-2001 deprivation to the OA-2001;
# b. use Norman's file to re-aggregate to DZ-2011.
# 
# 4. Re-run the decentralisation code using the estimates of Income Deprivation for the new DZ boundaries and compare with 
# 2. To do this, we need to be able to identify the four cities in terms of the DZ-2011 boundaries.
# 
# Early August
# 5. Finally, on the day that the SIMD2016 are released, run the decentralisation code on that data and also produce simple 
# maps showing absolute changes. Put out a short summary.
# 
# Does this sound like a plan?
# 
# Regards
# 
# Nick
#                                 

# 
# From: Paul Norman [mailto:P.D.Norman@leeds.ac.uk] 
# Sent: 18 May 2016 16:29
# To: Mark Livingston
# Cc: Mirjam Allik; Nick Bailey
# Subject: RE: 2001 (and earlier) output areas to 2011 datazones
# 
# Here are the files …
# 
# From: Paul Norman 
# Sent: 18 May 2016 16:25
# To: 'Mark Livingston' <Mark.Livingston@glasgow.ac.uk>
#   Cc: Mirjam Allik <Mirjam.Allik@glasgow.ac.uk>; Nick Bailey <Nick.Bailey@glasgow.ac.uk>
#   Subject: RE: 2001 (and earlier) output areas to 2011 datazones
# 
# Hi Mark & colleagues
# 
# Yep, sure. Here you go. In the correspondence to Alistair I said that I had wanted to delve more into tenure 
# for a revised Townsend over time but haven’t!
#   
#   The conversion tables operate the same way as stepped through in an Excel Northern Ireland spreadsheet version 
#  where this example is 2011 LGDs to the later fewer 2014(?) LGDs. Much of this then is just aggregation but where 
# the weights are less than 1 the source data is apportioned to different LGDs.
# 
# Recipe:
#   Join the original census ‘source’ data to the GCT
# Multiple variables by the weights
# Sum across the ‘target’ zones
# 
# In the Scotland conversions, the older to recent information involves more estimation of locations than the 
# newer stuff. From memory, I think some 71 EDs in the islands struggle because the geocoding and coastline 
# digitisation and differences over time will be somewhat iffy. Obviously if you spot something weird that compromises 
# please shout!
#   
#   Let me know how you get on and if ever you want text to explain for papers etc.
# 
# Best wishes
# 
# Paul
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Dr Paul Norman, School of Geography, University of Leeds
# Programme Manager MSc GIS & Director of Taught Postgraduate Studies
# https://sites.google.com/site/pdqnorman/home
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   ´¯`·.¸¸.·´¯`·.¸ ><((((((º>
#                              `·.¸¸.·´¯`·.¸¸.·´¯`·.¸ ><((((º>
#                                                              
#From: Mark Livingston [mailto:Mark.Livingston@glasgow.ac.uk] 
#Sent: 18 May 2016 16:00
#To: Paul Norman <P.D.Norman@leeds.ac.uk>
#Cc: Mirjam Allik <Mirjam.Allik@glasgow.ac.uk>; Nick Bailey <Nick.Bailey@glasgow.ac.uk>
#Subject: 2001 (and earlier) output areas to 2011 datazones
#                                                            
#Dear Paul,
#
#My colleague Mirjam passed on details of the work you have been doing on deprivation and on comparison over 
# census years which is very interesting.
#                                                            
#Nick Bailey and Myself are currently working on making comparisons of private rental rates and other census 
# data between census years in the UK, but in Scotland in particular, and we are interested in the method 
# that you have used to make your comparisons.
#In the chapters you have described your method for making comparison over years and you indicate that you have 
# “Conversion tables” which  “have been developed for the work” . so we were wondering if it would be possible 
# to get a copy of the conversion tables to allow us to convert data from previous census years to 2011 datazone boundaries.
#                                                            
#Best wishes
#                                                            
#Mark
#                                                            
#Mark Livingston
#Research Fellow, Urban Big Data Centre, Urban Studies
#School of Social and Political Sciences, University of Glasgow
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 