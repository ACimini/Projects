library(tidyverse)
library(ggplot2)
library(fuzzyjoin)

load("cleaned_tower_layers.Rdata")


# Make sure SIC codes are numeric
included_layers <- included_layers %>%
  mutate(sic_code = as.numeric(sic_code))

excluded_layers <- excluded_layers %>%
  mutate(sic_code = as.numeric(sic_code))

# Create SIC Division Lookup Table
sic_divisions <- tibble(
  division = c(
    "Agriculture, Forestry and Fishing",
    "Mining",
    "Construction",
    "Manufacturing",
    "Transportation, Communications, Electric, Gas and Sanitary service",
    "Wholesale Trade",
    "Retail Trade",
    "Finance, Insurance and Real estate",
    "Services",
    "Public administration",
    "Nonclassifiable"
  ),
  lower = c(100, 1000, 1500, 2000, 4000, 5000, 5200, 6000, 7000, 9100, 9900),
  upper = c(999, 1499, 1799, 3999, 4999, 5199, 5999, 6799, 8999, 9729, 9999)
)

# Join to included_layers
included_layers <- fuzzy_left_join(
  included_layers, sic_divisions,
  by = c("sic_code" = "lower", "sic_code" = "upper"),
  match_fun = list(`>=`, `<=`)
) %>%
  select(-lower, -upper)

# Join to excluded_layers
excluded_layers <- fuzzy_left_join(
  excluded_layers, sic_divisions,
  by = c("sic_code" = "lower", "sic_code" = "upper"),
  match_fun = list(`>=`, `<=`)
) %>%
  select(-lower, -upper)

# Quick check
included_layers %>%
  count(division, sort = TRUE)

#number of individual towers in each of the datasets
n_distinct(included_layers$client_identifier)
n_distinct(excluded_layers$client_identifier)


included_layers %>%
  filter(!is.na(rate_relativity), !is.na(division)) %>%
  ggplot(aes(x = layer_number, y = rate_relativity, color = division)) +
  geom_smooth(method = "loess", se = FALSE, size = 1.2) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Rate Relativity by Layer Number (Grouped by SIC Division)",
    x = "Layer Number",
    y = "Rate Relativity",
    color = "SIC Division"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

included_layers %>%
  filter(!is.na(rate_relativity), !is.na(division)) %>%
  ggplot(aes(x = layer_number, y = rate_relativity)) +
  geom_line(aes(group = client_identifier), alpha = 0.1) +
  stat_summary(fun = mean, geom = "line", color = "blue", size = 1.2) +
  facet_wrap(~ division) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Tower Rate Relativity Profiles by SIC Division",
    x = "Layer Number",
    y = "Rate Relativity"
  ) +
  theme_minimal()







# Total tower limit per client
tower_limits <- included_layers %>%
  group_by(client_identifier) %>%
  summarise(total_tower_limit = sum(total_limit, na.rm = TRUE), .groups = "drop")

# Join it back to included_layers
included_layers_labeled <- included_layers %>%
  left_join(tower_limits, by = "client_identifier")











included_layers_labeled %>%
  filter(!is.na(rate_relativity), !is.na(division)) %>%
  ggplot(aes(x = total_tower_limit, y = rate_relativity, color = division)) +
  geom_smooth(method = "loess", se = FALSE, size = 1.2) +
  coord_cartesian(ylim = c(0, 1.5)) +
  scale_x_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Rate Relativity by Total Tower Limit (Grouped by SIC Division)",
    x = "Total Tower Limit (Millions)",
    y = "Rate Relativity",
    color = "SIC Division"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


included_layers_labeled %>%
  filter(!is.na(rate_relativity), !is.na(division)) %>%
  ggplot(aes(x = total_tower_limit, y = rate_relativity)) +
  geom_line(aes(group = client_identifier), alpha = 0.1) +
  stat_summary(fun = mean, geom = "line", color = "blue", size = 1.2) +
  facet_wrap(~ division) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_x_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Tower Rate Relativity Profiles by Total Limit (Faceted by SIC Division)",
    x = "Total Tower Limit (Millions)",
    y = "Rate Relativity"
  ) +
  theme_minimal()




