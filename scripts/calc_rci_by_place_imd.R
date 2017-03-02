# Explore IMD by city 


# TO DO: Wales  IMD 


rm(list = ls())

require(pacman)

pacman::p_load(
  tidyverse,
  readr, readxl,
  forcats,
  stringr,  
  purrr,
  rgeos,
  ggplot2,
  tmap
)

source("scripts/from_gavin/RCI.R")




# Dissimilarity index 

D <- function(minority, total){
  majority      <- total - minority
  MINORITY      <- sum(minority)
  MAJORITY      <- sum(majority)
  
  p1 <- minority / MINORITY
  p2 <- majority / MAJORITY 
  
  ad <- abs(p1 - p2)
  
  out <- 0.5 * sum(ad)
  out
}

# Work out the spatial concentration of people in area
calc_gini <- function(DATA){
  
  df <- data_frame(x = c(0, DATA$area), y = c(0, DATA$pop_total)) %>% 
    arrange(x) %>% 
    mutate(
      sx = cumsum(x) / sum(x),
      sy = cumsum(y) / sum(y)
    )
  
  fn <- approxfun(df$sx, df$sy)
  int_output <- integrate(fn, 0, 1, stop.on.error = F)
  if (int_output$message != "OK") {
    print("Error detected!")
    print(int_output$message)
    return(NA)
  } else {
    auc <- int_output$value
    print(auc)
    out <- auc - 0.5
    return(out)
    }
}

# imd
dist_to_centre <- read_csv("data/lsoa_2011_by_dist_to_centres.csv")
dta <- read_csv("data/imd/imd_id_lsoa2011_tidied.csv", col_types = "cddd")

#SIMD
dist_to_centre_scot <- read_csv("data/dz_2011_by_dist_to_centres.csv")
dta_scot <- read_csv("data/simd/simd_combined_on_2011.csv")

# Inputs to RCI are 
#povvec
#popvec
#order




dta %>% 
  inner_join(dist_to_centre) %>%
  mutate(pdens = pop_total / area) %>% 
  group_by(place, year) %>% 
  nest() %>% 
  filter(place != "Cardiff") %>% 
  mutate(
    pop_area_conc = map_dbl(data, calc_gini),
    pvec = map(data, ~ .[["pop_id"]]),
    tvec = map(data, ~ .[["pop_total"]]),
    ordr = map(data, ~ order(.[["distance"]])),
    ordr_dens = map(data, ~ order(.[["pdens"]], decreasing = T))
         ) %>% 
  mutate(rci_val = pmap_dbl(list(pvec, tvec, ordr), RCI)) %>% 
  mutate(rdi_val = pmap_dbl(list(pvec, tvec, ordr_dens), RCI)) %>% 
  mutate(d_val = pmap_dbl(list(pvec, tvec), D)) %>% 
  select(place, year, rci = rci_val, rdi = rdi_val, d = d_val, pop_area_conc) %>% 
  ungroup -> rci_by_year_place


# Same but for Scotland
dta_scot %>% 
  inner_join(dist_to_centre_scot, by = c("dz_2011" = "dz")) %>%
  mutate(pdens = pop_total / area) %>%
  select(dz_2011, place, year, pop_id = pop_incomedeprived, pop_total, distance, pdens, area) %>% 
  group_by(place, year) %>% 
  nest() %>% 
  mutate(
    pop_area_conc = map_dbl(data, calc_gini),
    pvec = map(data, ~ .[["pop_id"]]),
    tvec = map(data, ~ .[["pop_total"]]),
    ordr = map(data, ~ order(.[["distance"]])),
    ordr_dens = map(data, ~ order(.[["pdens"]], decreasing = T))
  ) %>% 
  mutate(rci_val = pmap_dbl(list(pvec, tvec, ordr), RCI)) %>% 
  mutate(rdi_val = pmap_dbl(list(pvec, tvec, ordr_dens), RCI)) %>% 
  mutate(d_val = pmap_dbl(list(pvec, tvec), D)) %>% 
  select(place, year, rci = rci_val, rdi = rdi_val, d = d_val, pop_area_conc) %>% 
  ungroup -> rci_by_year_place_scot


# Combine England w/ Scotland 

rci_by_year_place <- bind_rows(
  rci_by_year_place, 
  rci_by_year_place_scot
) 
  

# Regression summary? 
# Predictors of RCI 

lm(rci ~ rdi + d, data = rci_by_year_place) %>% summary()

# Year interactions
lm(rci ~ (rdi + d) * I(year - min(year)), data = rci_by_year_place) %>% summary()
# Interactions not stat sig

# Year as independent term
lm(rci ~ (rdi + d) + I(year - min(year)), data = rci_by_year_place) %>% summary()
# Year not stat sig

# Year only
lm(rci ~ I(year - min(year)), data = rci_by_year_place) %>% summary()
# Not stat sig

# Pop area conc
lm(rci ~ pop_area_conc, data = rci_by_year_place) %>% summary()
# not stat sig

# fixed effects for place
lm(rci ~ place , data = rci_by_year_place) %>% 
  broom::tidy() %>% 
  mutate(term = str_replace(term, "place", "")) %>% 
  mutate(term = reorder(term, estimate)) %>% 
  mutate(lower = estimate - 2* std.error, upper = estimate + 2* std.error) %>% 
  ggplot(., aes(y = term)) + 
  geom_point(aes(x = estimate)) + 
  geom_errorbarh(aes(x = estimate, xmin = lower, xmax = upper)) + 
  geom_vline(aes(xintercept = 0)) + 
  labs(x = "Estimate", y = "RCI on Place")

lm(d ~ place, data = rci_by_year_place) %>% 
  broom::tidy() %>% 
  mutate(term = str_replace(term, "place", "")) %>% 
  mutate(term = reorder(term, estimate)) %>% 
  mutate(lower = estimate - 2* std.error, upper = estimate + 2* std.error) %>% 
  ggplot(., aes(y = term)) + 
  geom_point(aes(x = estimate)) + 
  geom_errorbarh(aes(x = estimate, xmin = lower, xmax = upper)) + 
  geom_vline(aes(xintercept = 0)) + 
  labs(x = "Estimate", y = "D on Place")

rci_by_year_place %>% 
  filter(!is.na(pop_area_conc)) %>% 
  lm(pop_area_conc ~ place, data = .) %>% 
  broom::tidy() %>% 
  mutate(term = str_replace(term, "place", "")) %>% 
  mutate(term = reorder(term, estimate)) %>% 
  mutate(lower = estimate - 2* std.error, upper = estimate + 2* std.error) %>% 
  ggplot(., aes(y = term)) + 
  geom_point(aes(x = estimate)) + 
  geom_errorbarh(aes(x = estimate, xmin = lower, xmax = upper)) + 
  geom_vline(aes(xintercept = 0)) + 
  labs(x = "Estimate", y = "Spatial Concentration on Place")



# Want to arrange TTWA by population size in (say) 2010

dta %>% 
  inner_join(dist_to_centre) %>% 
  filter(year == 2010) %>% 
  group_by(place) %>% 
  summarise(place_pop = sum(pop_total)) -> tmp1


dta_scot %>% 
  inner_join(dist_to_centre_scot, by = c("dz_2011" = "dz")) %>% 
  filter(year == 2009) %>% 
  group_by(place) %>% 
  summarise(place_pop = sum(pop_total)) -> tmp2

tmp1 %>% 
  bind_rows(tmp2) %>% 
  arrange(desc(place_pop)) %>% 
  .[["place"]] -> place_by_size_order

rm(tmp1, tmp2)



# pop_area_conc by year and place 

rci_by_year_place %>% 
  mutate(place = factor(place, ordered = T, levels = place_by_size_order)) %>% 
  ggplot(., aes(x = year, y = pop_area_conc)) + 
  facet_wrap(~place) + 
  geom_line() + geom_point()


rci_by_year_place %>% 
  mutate(place = factor(place, ordered = T, levels = place_by_size_order)) %>% 
  ggplot(., aes(x = year, y = pop_area_conc, group = place)) + 
  geom_line(alpha = 0.6) + geom_text(aes(label = place))  + 
  labs(x = "Year", y = "Absolute Concentration Score", title = "Absolute population concentration over time")
ggsave("figures/TTWA/population_concentration_scores.png", height = 30, width = 30, dpi =300, units = "cm")





rci_by_year_place %>% 
  mutate(place = factor(place, ordered = T, levels = place_by_size_order)) %>% 
  ggplot(., aes(x = year, y = rci)) + 
  geom_line() + geom_point() + 
  geom_hline(aes(yintercept = 0)) +  
  facet_wrap(~place) + 
  labs(title = "RCI by year and TTWA", subtitle = "TTWAs arranged by size", x = "Year", y = "Relative Centralisation Index (RCI)")
ggsave("figures/TTWA/rci_by_year_ttwa.png", height= 25, width = 25, units = "cm", dpi = 300)


rci_by_year_place %>% 
  mutate(place = factor(place, ordered = T, levels = place_by_size_order)) %>% 
  ggplot(., aes(x = year, y = rdi)) + 
  geom_line() + geom_point() + 
  geom_hline(aes(yintercept = 0)) +  
  facet_wrap(~place) + 
  labs(title = "RDI by year and TTWA", subtitle = "TTWAs arranged by size", x = "Year", y = "Relative 'Densification' Index (RDI)")
ggsave("figures/TTWA/rdi_by_year_ttwa.png", height= 25, width = 25, units = "cm", dpi = 300)


# RDI and RCI on same plot 
rci_by_year_place %>% 
  mutate(place = factor(place, ordered = T, levels = place_by_size_order)) %>% 
  select(-d) %>% 
  gather(key = "measure", value = "value", rci, rdi) %>% 
  ggplot(., aes(x  = year, y = value, group = measure)) + 
  geom_line(aes(linetype = measure)) + 
  geom_point(aes(shape = measure)) +
  geom_hline(aes(yintercept = 0)) + 
  facet_wrap(~place) +
  labs(title = "RDI and RCI by year and TTWA", subtitle = "TTWAs arranged by size", x = "Year", y = "Score")
ggsave("figures/TTWA/rdi_rci_by_year_ttwa.png", height= 25, width = 25, units = "cm", dpi = 300)




# D - faceted

rci_by_year_place %>% 
  mutate(TTWA = factor(place, ordered = T, levels = place_by_size_order)) %>% 
  ggplot(., aes(x = year, y = d)) + 
  geom_line() + geom_point()  +
  geom_hline(yintercept = 0) + 
  coord_cartesian(ylim = c(0.25, 0.45)) + 
  labs(x = "Year", y = "Dissimilarity Index") + 
  facet_wrap(~TTWA) + 
  labs(title = "D by year and TTWA", subtitle = "TTWAs arranged by size", x = "Year", y = "Dissimilarity Index")
ggsave("figures/TTWA/d_by_year_ttwa.png", height= 25, width = 25, units = "cm", dpi = 300)



# ratio, faceted
rci_by_year_place %>% 
  mutate(TTWA = factor(place, ordered = T, levels = place_by_size_order)) %>% 
  mutate(rtio = abs(rci) / abs(rdi)) %>% 
  ggplot(., aes(x = year, y = rtio)) + 
  geom_line() + geom_point()  +
  scale_y_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1, 2)) + 
  labs(x = "Year", y = "Ratio of |RCI| / |RDI| (log scale)", title = "Ratio of RCI to RDI", subtitle = "Ratio of absolute values"
       ) + 
  facet_wrap(~TTWA) + 
  geom_hline(aes(yintercept = 1), lty = "dashed") 
ggsave("figures/TTWA/ratio_abs_by_ttwa.png", height = 25, width = 25, units = "cm", dpi = 300)


# Share of poor, by decile of density or decile of distance ---------------

dta_scot %>% 
  inner_join(dist_to_centre_scot, by = c("dz_2011" = "dz")) %>% 
  select(year, lsoa = dz_2011, pop_id = pop_incomedeprived, pop_total, place, distance, area) -> tmp1
  
dta %>% 
  inner_join(dist_to_centre) -> tmp2

ypd <- bind_rows(tmp1, tmp2) %>% filter(place != "Cardiff")
rm(tmp1, tmp2)

ypd %>% 
  mutate(
    year = factor(year)
  ) %>% 
  mutate(
    Year = fct_recode(
      year, 
      `2004` = "2004",
      `2006-07` = "2006",
      `2006-07` = "2007",
      `2009-10` = "2009",
      `2009-10` = "2010",
      `2012`      = "2012",    
      `2015-16` = "2015",
      `2015-16` = "2016"
    )
  ) %>% 
  mutate(place = factor(place, ordered = T, levels = place_by_size_order)) %>% 
  group_by(Year, place) %>% 
  mutate(
    dist_decile = ntile(distance, 10)
    ) %>% 
  group_by(Year, place, dist_decile) %>% 
  summarise(pop_id = sum(pop_id)) %>% 
  group_by(Year, place) %>% 
  mutate(share_id = pop_id / sum(pop_id)) %>% 
  ggplot(., aes(x = factor(dist_decile), y = share_id, group = Year)) + 
  geom_point(aes(shape = Year)) + geom_line(aes(colour = Year)) + 
  facet_wrap(~place) + 
  labs(
    x = "Decile of distance from centre in TTWA",
    y = "Share of TTWA's income deprived population", 
    title = "Share of TTWA income deprivation by decile of distance from centre",
    subtitle = "TTWAs arranged by size",
    caption = "Scottish and English IMDs are for different years"
  ) 
ggsave("figures/TTWA/id_share_by_dist_decile_and_ttwa.png", height = 25, width = 25, units = "cm", dpi = 300)




ypd %>% 
  mutate(
    year = factor(year)
  ) %>% 
  mutate(
    Year = fct_recode(
      year, 
      `2004` = "2004",
      `2006-07` = "2006",
      `2006-07` = "2007",
      `2009-10` = "2009",
      `2009-10` = "2010",
      `2012`      = "2012",    
      `2015-16` = "2015",
      `2015-16` = "2016"
    )
  ) %>% 
  mutate(place = factor(place, ordered = T, levels = place_by_size_order)) %>% 
  group_by(Year, place) %>% 
  mutate(
    density = pop_total / area,
    density_decile = 11 - ntile(density, 10)
  ) %>% 
  group_by(Year, place, density_decile) %>% 
  summarise(pop_id = sum(pop_id)) %>% 
  group_by(Year, place) %>% 
  mutate(share_id = pop_id / sum(pop_id)) %>% 
  ggplot(., aes(x = factor(density_decile), y = share_id, group = Year)) + 
  geom_point(aes(shape = Year)) + geom_line(aes(colour = Year)) + 
  facet_wrap(~place) + 
  labs(
    x = "Decile of density in TTWA (1 = most dense)",
    y = "Share of TTWA's income deprived population", 
    title = "Share of TTWA income deprivation by decile of density",
    subtitle = "TTWAs arranged by size",
    caption = "Scottish and English IMDs are for different years"
  ) 
ggsave("figures/TTWA/id_share_by_dens_decile_and_ttwa.png", height = 25, width = 25, units = "cm", dpi = 300)


# Density and distance on a single plot 
ypd %>% 
  mutate(
    year = factor(year)
  ) %>% 
  mutate(
    Year = fct_recode(
      year, 
      `2004` = "2004",
      `2006-07` = "2006",
      `2006-07` = "2007",
      `2009-10` = "2009",
      `2009-10` = "2010",
      `2012`      = "2012",    
      `2015-16` = "2015",
      `2015-16` = "2016"
    )
  ) %>% 
  group_by(Year, place) %>% 
  mutate(
    density = pop_total / area,
    decile = 11 - ntile(density, 10)
  ) %>% 
  group_by(Year, place, decile) %>% 
  summarise(pop_id = sum(pop_id)) %>% 
  group_by(Year, place) %>% 
  mutate(share_id_dens = pop_id / sum(pop_id)) %>% 
  select(Year, place, decile, share_id_dens) -> tmp1

ypd %>% 
  mutate(
    year = factor(year)
  ) %>% 
  mutate(
    Year = fct_recode(
      year, 
      `2004` = "2004",
      `2006-07` = "2006",
      `2006-07` = "2007",
      `2009-10` = "2009",
      `2009-10` = "2010",
      `2012`      = "2012",    
      `2015-16` = "2015",
      `2015-16` = "2016"
    )
  ) %>% 
  group_by(Year, place) %>% 
  mutate(
    decile = ntile(distance, 10)
  ) %>% 
  group_by(Year, place, decile) %>% 
  summarise(pop_id = sum(pop_id)) %>% 
  group_by(Year, place) %>% 
  mutate(share_id_dist = pop_id / sum(pop_id)) %>% 
  select(Year, place, decile, share_id_dist) -> tmp2

dd_share <- inner_join(tmp1, tmp2)

rm(tmp1, tmp2)
  
dd_share %>% 
  gather(key = "dd", value = "share", share_id_dens, share_id_dist) %>%
  mutate(dd = fct_recode(dd, Density = "share_id_dens", Distance = "share_id_dist")) %>% 
  ungroup() %>% 
  mutate(place = factor(place, ordered = T, levels = place_by_size_order)) %>% 
  ggplot(., aes(x = factor(decile), y = share, group = dd)) + 
  geom_point(aes(shape = dd)) + geom_line(aes(linetype = dd)) + 
  facet_grid(place ~ Year) +   theme_minimal() +
  theme(strip.text.y = element_text(angle = 0)) +
  labs(
    title = "Correspondence between Income Deprived share and deciles of density and distance",
    x = "Decile of density or distance",
    y = "Share of TTWA's income deprived population",
    caption = "No English IMD data for 2012"
       ) + 
  guides(
    shape = guide_legend(
      title = "Density/Distance decile"
    ),
    linetype = guide_legend(
      title = "Density/Distance decile"
    )
  )
ggsave("figures/TTWA/dens_dist_decile_correspondence.png", height = 40, width = 30, units = "cm", dpi = 300)


# Correlation between the two? 


ypd %>% 
  group_by(year, place) %>% 
  mutate(
    density = pop_total / area,
    decile = 11 - ntile(density, 10)
  ) %>% 
  group_by(year, place, decile) %>% 
  summarise(pop_id = sum(pop_id)) %>% 
  group_by(year, place) %>% 
  mutate(share_id_dens = pop_id / sum(pop_id)) %>% 
  select(year, place, decile, share_id_dens) -> tmp1

ypd %>% 
  group_by(year, place) %>% 
  mutate(
    decile = ntile(distance, 10)
  ) %>% 
  group_by(year, place, decile) %>% 
  summarise(pop_id = sum(pop_id)) %>% 
  group_by(year, place) %>% 
  mutate(share_id_dist = pop_id / sum(pop_id)) %>% 
  select(year, place, decile, share_id_dist) -> tmp2

dd_share <- inner_join(tmp1, tmp2)

rm(tmp1, tmp2)
dd_share %>% 
  group_by(year, place) %>% 
  nest() %>% 
  mutate(cr = map_dbl(data, function(x) cor(x[,2:3])[2,1])) %>% 
  select(-data) %>% 
  mutate(place = factor(place, ordered = T, levels = place_by_size_order)) %>% 
  ungroup() %>% 
  ggplot(., aes(x = year, y = cr)) + 
  geom_point() + geom_line() + 
  facet_wrap(~place) + geom_hline(aes(yintercept = 0)) + 
  labs(x = "Year", y = "Correlation between rdi and rci by deciles within year")
ggsave("figures/TTWA/cor_between_dd_dec_over_time.png", height = 25, width = 25, units = "cm", dpi = 300)




# RDI, RCI, D - path over time for each TTWA 

rci_by_year_place %>% 
  mutate(place = factor(place, ordered = T, levels = place_by_size_order)) %>% 
  ggplot(., aes(x = rci, y = rdi)) + 
  geom_path(aes(alpha = year), colour = "blue") +
  geom_text(aes(label = year), size = rel(3), data = subset(rci_by_year_place, subset = year %in% c(2004, 2015, 2016))) +
  facet_wrap(~place)

rci_by_year_place %>% 
  mutate(place = factor(place, ordered = T, levels = place_by_size_order)) %>% 
  ggplot(., aes(x = rci, y = d)) + 
  geom_path(aes(alpha = year), colour = "blue") +
  geom_text(aes(label = year), size = rel(3), data = subset(rci_by_year_place, subset = year %in% c(2004, 2015, 2016))) +
  facet_wrap(~place)





# 3D attempt using RGL ----------------------------------------------------


show_3d_change <- function(PLACE){
  rci_by_year_place %>% 
    filter(place == PLACE) %>% 
    arrange(year) -> tmp
  
  n <- nrow(tmp)
  

  with(tmp, rgl::plot3d(rdi, rci, d, type = "l", expand = 1.10))

  with(tmp, rgl::plot3d(rdi, rci, d, type = "s", 
                        col = c("red", rep("black", n - 2), "blue"), 
                        alpha = c(1, rep(0.5, n-2), 1), add = T))
  rgl::planes3d(1,0,0, 0, alpha = 0.2, add = T)
  rgl::planes3d(0,1,0, 0, alpha = 0.2, add = T)
  title3d(PLACE)
  NULL
}

show_3d_change("Glasgow")
show_3d_change("London")
show_3d_change("Edinburgh")
show_3d_change("Leicester")
show_3d_change("Cambridge")
show_3d_change("Leicester")
