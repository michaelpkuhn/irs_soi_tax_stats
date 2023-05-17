library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(ggbeeswarm)
library(viridis)
library(extrafont)
options(tigris_use_cache = TRUE)

df <- get_acs(geography = "tract", state = "IL",  
              variables = c("B19013_001"), 
              summary_var = "B19013_001", geometry = TRUE) %>%
  mutate(variable = recode(variable, B19013_001 = "Total")) %>%
  group_by(GEOID) %>%
  filter(estimate == max(estimate, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(estimate != 0)

metro <- core_based_statistical_areas(cb = TRUE, class = "sf") %>%
  filter(str_detect(NAME, "Chicago"))

dfw <- df[metro, op = st_within]

taxes <- sapply(dfw$summary_est, chi_income_tax)

ggplot(dfw, aes(x = variable, y = summary_est, color = summary_est)) +
  geom_quasirandom(alpha = 0.5) + 
  coord_flip() + 
  theme_minimal(base_family = "Tahoma") + 
  scale_color_viridis(guide = FALSE) + 
  scale_y_continuous(labels = scales::dollar) + 
  labs(x = "Largest group in Census tract", 
       y = "Median household income", 
       title = "Household income distribution by largest racial/ethnic group", 
       #subtitle = "Census tracts, Dallas-Fort Worth metropolitan area", 
       #caption = "Data source: 2011-2015 ACS"
       ) +
  geom_line(aes(y = taxes))
