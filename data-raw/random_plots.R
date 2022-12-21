## code to prepare `random_plots` dataset goes here

library(tidyverse)

devtools::load_all()

random_plots <- list()

cost_comparison <-
  experiments::generic_archetypes %>%
  mutate(vehicle_type = factor(vehicle_type),
         fuel_type = factor(fuel_type)) %>%
  pivot_longer(cols = c(fix_cost, cost_per_km)) %>%
  mutate(name = factor(name, labels = c("Cost per km", "Fix cost")),
         ga = ifelse(name == "Fix cost", 3860, NA)) %>%
  ggplot(aes(x = reorder(vehicle_type, -value), y = value, fill = fuel_type)) +
  geom_col(position = position_dodge()) +
  geom_hline(aes(yintercept = ga, col = "Price GA 2nd class (3860 CHF)")) +
  facet_wrap(facets = vars(name), scales = "free_y") +
  scale_fill_viridis_d() +
  themer::theme_ivt() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Vehicle type", y = "Cost (CHF)", fill = "Fuel type", col = "Reference")

ggsave("./tmp/cost_comparison.png", width = 12, height = 7)

random_plots$cost_comparison <- cost_comparison


usethis::use_data(random_plots, overwrite = TRUE)
