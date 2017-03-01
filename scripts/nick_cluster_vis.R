pacman::p_load(
  readr, readxl,
  stringr, tidyr, dplyr, 
  purrr,
  rgeos,
  ggplot2,
  tmap
)

dta <- read.delim("clipboard")


dta
names(dta) <- c("place", "k1","k2","k3", "k4", "k5")
dta %>% gather(
  key = "cluster", value = "value", -place
) %>% 
  mutate(cluster = as.numeric(str_replace(cluster, "k", ""))) %>% 
  group_by(place) %>% 
  mutate(rel = value / value[cluster == 1]) %>% ungroup() %>% 
  ggplot(., aes(x= cluster, y = rel)) + 
  geom_line() + geom_point()  + 
  stat_smooth(method = "lm", se = F, linetype = "dashed") +
  facet_wrap(~place)

