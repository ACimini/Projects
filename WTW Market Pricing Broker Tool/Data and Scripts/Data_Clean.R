
library(tidyverse)

setwd("C:/Users/alexc/OneDrive/Desktop/Final Files/Data")

data = read.csv("final_data_with_sic.csv")

towers = data |> select(id, industry_vertical, region, sic_code,
                        umbrella_limit, lead_premium, total_limit,
                        xs_layer_1_limit,xs_layer_1_attachment, xs_layer_1_premium,
                        xs_layer_2_limit,xs_layer_2_attachment, xs_layer_2_premium,
                        xs_layer_3_limit,xs_layer_3_attachment, xs_layer_3_premium,
                        xs_layer_4_limit,xs_layer_4_attachment, xs_layer_4_premium,
                        xs_layer_5_limit,xs_layer_5_attachment, xs_layer_5_premium,
                        xs_layer_6_limit,xs_layer_6_attachment, xs_layer_6_premium,
                        xs_layer_7_limit,xs_layer_7_attachment, xs_layer_7_premium,
                        xs_layer_8_limit,xs_layer_8_attachment, xs_layer_8_premium,
                        xs_layer_9_limit,xs_layer_9_attachment, xs_layer_9_premium,
                        xs_layer_10_limit,xs_layer_10_attachment, xs_layer_10_premium,
                        xs_layer_11_limit,xs_layer_11_attachment, xs_layer_11_premium,
                        xs_layer_12_limit,xs_layer_12_attachment, xs_layer_12_premium,
                        xs_layer_13_limit,xs_layer_13_attachment, xs_layer_13_premium,
                        xs_layer_14_limit,xs_layer_14_attachment, xs_layer_14_premium,
                        xs_layer_15_limit,xs_layer_15_attachment, xs_layer_15_premium,
                        xs_layer_16_limit,xs_layer_16_attachment, xs_layer_16_premium,
                        xs_layer_17_limit,xs_layer_17_attachment, xs_layer_17_premium,
                        xs_layer_18_limit,xs_layer_18_attachment, xs_layer_18_premium,
                        xs_layer_19_limit,xs_layer_19_attachment, xs_layer_19_premium,
                        xs_layer_20_limit,xs_layer_20_attachment, xs_layer_20_premium)

# Pivot excess layers to long format
towers_long = towers %>%
  pivot_longer(
    cols = matches("xs_layer_\\d+_(limit|attachment|premium)"),
    names_to = c("original_layer", ".value"),
    names_pattern = "xs_layer_(\\d+)_(limit|attachment|premium)"
  ) %>%
  mutate(original_layer = as.integer(original_layer))

# Create umbrella layer
umbrella_layer = towers %>%
  transmute(
    id,
    attachment = 0,
    limit = umbrella_limit,
    premium = lead_premium,
    original_layer = 0
  )

# Combine, summarize, and renumber layers
combined_layers = bind_rows(umbrella_layer, towers_long) %>%
  filter(!is.na(attachment)) %>%
  group_by(id, attachment) %>%
  summarise(
    total_limit = sum(limit, na.rm = TRUE),
    total_premium = sum(premium, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(id, attachment) %>%
  group_by(id) %>%
  mutate(layer_number = row_number() - 1) %>%
  ungroup()

# Add price per million and rate relativity
combined_layers = combined_layers %>%
  mutate(price_per_million = ifelse(total_limit > 0, total_premium / (total_limit / 1e6), NA)) %>%
  group_by(id) %>%
  arrange(layer_number, .by_group = TRUE) %>%
  mutate(rate_relativity = price_per_million / lag(price_per_million)) %>%
  ungroup() %>%
  mutate(rate_relativity = ifelse(is.finite(rate_relativity), rate_relativity, NA))

# ---- Exclusion logic with reasons ----

# Exclude towers with missing or zero limits/premiums
excluded_bad = combined_layers %>%
  filter(is.na(total_limit) | total_limit == 0 |
           is.na(total_premium) | total_premium == 0) %>%
  mutate(exclusion_reason = "missing_or_zero_limit_or_premium")

# Exclude umbrella-only towers
umbrella_only_ids = combined_layers %>%
  group_by(id) %>%
  filter(n() == 1 & layer_number == 0) %>%
  pull(id)

excluded_umbrella_only = combined_layers %>%
  filter(id %in% umbrella_only_ids) %>%
  mutate(exclusion_reason = "umbrella_only")

# Temporarily define included_layers
included_layers = combined_layers %>%
  filter(!(id %in% c(
    excluded_bad$id,
    excluded_umbrella_only$id
  )))

# Exclude towers with any layer rate relativity > 5
high_relativity_ids = included_layers %>%
  filter(rate_relativity > 1.25) %>%
  pull(id) %>%
  unique()

excluded_high_relativity = included_layers %>%
  filter(id %in% high_relativity_ids) %>%
  mutate(exclusion_reason = "rate_relativity_over_5")

# Combine all exclusions
excluded_layers = bind_rows(
  excluded_bad,
  excluded_umbrella_only,
  excluded_high_relativity
)

# Keep only valid towers
included_layers = combined_layers %>%
  filter(!(id %in% excluded_layers$id))

# Add industry, region, sic_code back in
tower_metadata = towers %>%
  select(id, industry_vertical, region, sic_code)

included_layers = included_layers %>%
  left_join(tower_metadata, by = "id")

excluded_layers = excluded_layers %>%
  left_join(tower_metadata, by = "id")

length(unique(included_layers$id))

write.csv(included_layers, "Final_Clean_Data.csv", row.names = FALSE)






# ---- Plotting ----

ggplot(included_layers %>% filter(!is.na(rate_relativity)), 
       aes(x = total_limit, y = rate_relativity)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  scale_x_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Rate Relativity vs. Layer Limit",
    x = "Layer Limit (in Millions)",
    y = "Rate Relativity"
  ) +
  theme_minimal()

ggplot(included_layers %>% filter(!is.na(price_per_million)), 
       aes(x = total_limit, y = price_per_million)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "darkgreen") +
  scale_x_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Price per Million vs. Layer Limit",
    x = "Layer Limit (in Millions)",
    y = "Price per Million ($)"
  ) +
  theme_minimal()

ggplot(included_layers %>% filter(!is.na(rate_relativity)), 
       aes(x = layer_number, y = rate_relativity, group = id)) +
  geom_line(alpha = 0.1) +
  geom_smooth(aes(group = 1), method = "loess", se = FALSE, color = "purple", size = 1.2) +
  labs(
    title = "Rate Relativity by Layer Number",
    x = "Layer Number",
    y = "Rate Relativity"
  ) +
  theme_minimal()

save(
  included_layers,
  file = "cleaned_tower_layers.RData"
)
