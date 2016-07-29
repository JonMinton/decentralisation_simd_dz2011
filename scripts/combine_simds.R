rm(list = ls())

require(pacman)

pacman::p_load(
  readr, readxl,
  stringr, tidyr, dplyr, 
  rgeos,
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

write_csv(simd_combined, path = "data/simd/simd_combined.csv")
