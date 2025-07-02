
library(ggplot2)
setwd("C:/Users/alexc/OneDrive/Desktop/Final Files/Data")
data = read.csv("Matt_data_with_SIC.csv")

summary(data)

library(dplyr)

# Step 1: Get IDs of towers with all relativities < 1.25 (or NA)
good_ids <- data %>%
  group_by(ClientID) %>%
  summarise(all_below_1.25 = all(Relativity < 1.25 | is.na(Relativity))) %>%
  filter(all_below_1.25) %>%
  pull(ClientID)

# Step 2: Filter for those towers AND remove rows with capacity >= 500M
data_cleaned <- data %>%
  filter(ClientID %in% good_ids) %>%      # keep only good towers
  filter(Capacity < 3.5e8)                  # and capacity under 500 million






ggplot(data_cleaned %>% filter(!is.na(Relativity)), 
       aes(x = LayerNumber, y = Relativity)) +
  geom_point(alpha = 0.1, size = 1, color = "black") +
  geom_smooth(method = "loess", se = FALSE, color = "purple", size = 1.2) +
  labs(
    title = "Rate Relativity by Layer Number",
    x = "Layer Number",
    y = "Rate Relativity"
  ) +
  theme_minimal()


ggplot(data_cleaned %>% filter(!is.na(Relativity)), 
       aes(x = Capacity, y = Relativity)) +
  geom_point(alpha = 0.1, size = 1, color = "black") +
  geom_smooth(method = "loess", se = FALSE, color = "purple", size = 1.2) +
  labs(
    title = "Rate Relativity by Layer Number",
    x = "Layer Number",
    y = "Rate Relativity"
  ) +
  theme_minimal()

length(unique(data_cleaned$ClientID))



data_cleaned <- data_cleaned %>%
  mutate(SIC.Division = case_when(
    SIC.Code >= 100 & SIC.Code <= 999   ~ "Ag, Forestry, Fishing",
    SIC.Code >= 1000 & SIC.Code <= 1499 ~ "Mining",
    SIC.Code >= 1500 & SIC.Code <= 1799 ~ "Construction",
    SIC.Code >= 2000 & SIC.Code <= 3999 ~ "Manufacturing",
    SIC.Code >= 4000 & SIC.Code <= 4999 ~ "Transport & Utilities",
    SIC.Code >= 5000 & SIC.Code <= 5199 ~ "Wholesale Trade",
    SIC.Code >= 5200 & SIC.Code <= 5999 ~ "Retail Trade",
    SIC.Code >= 6000 & SIC.Code <= 6799 ~ "Finance, Insurance & RE",
    SIC.Code >= 7000 & SIC.Code <= 8999 ~ "Services",
    SIC.Code >= 9100 & SIC.Code <= 9729 ~ "Public Administration",
    SIC.Code >= 9900 & SIC.Code <= 9999 ~ "Nonclassifiable",
    TRUE                                ~ "Other"
  ))

library(dplyr)
library(forcats)

data_cleaned <- data_cleaned %>%
  mutate(SIC.Division = fct_infreq(SIC.Division))  # reorders by descending count


library(ggplot2)

library(ggplot2)

ggplot(data_cleaned %>% filter(!is.na(Relativity), !is.na(SIC.Division)), 
       aes(x = LayerNumber, y = Relativity)) +
  geom_point(alpha = 0.1, size = 0.5, color = "black") +
  geom_smooth(method = "loess", se = FALSE, color = "purple", size = 1.5) +
  facet_wrap(~ SIC.Division, scales = "free") +
  labs(
    title = "Rate Relativity vs. Layer Number by SIC Division",
    x = "Layer Number",
    y = "Rate Relativity"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 9, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 8)
  )


ggplot(data_cleaned %>% filter(!is.na(Relativity), !is.na(SIC.Division)), 
       aes(x = Capacity, y = Relativity)) +
  geom_point(alpha = 0.2, size = 1, color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "darkorange", size = 1) +
  facet_wrap(~ SIC.Division, scales = "free") +
  labs(
    title = "Rate Relativity vs. Capacity by SIC Division (Most Frequent First)",
    x = "Capacity ($)",
    y = "Rate Relativity"
  ) +
  scale_x_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "M")) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 8)
  )

data_cleaned %>%
  group_by(SIC.Division, LayerNumber) %>%
  summarise(avg_rel = mean(Relativity, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = LayerNumber, y = fct_rev(SIC.Division), fill = avg_rel)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Average Rate Relativity by Layer and SIC Division",
    x = "Layer Number",
    y = "SIC Division",
    fill = "Avg Relativity"
  ) +
  theme_minimal()

ggplot(data_cleaned %>% filter(!is.na(Relativity), !is.na(SIC.Division)), 
       aes(x = LayerLimit, y = Relativity)) +
  geom_point(alpha = 0.2, size = 1, color = "darkgreen") +
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 1) +
  facet_wrap(~ SIC.Division, scales = "free") +
  scale_x_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Rate Relativity vs. Layer Limit by SIC Division",
    x = "Layer Limit ($M)",
    y = "Rate Relativity"
  ) +
  theme_minimal()

ggplot(data_cleaned %>% filter(!is.na(Relativity)), 
       aes(x = SIC.Division, y = Relativity)) +
  geom_violin(fill = "lightblue", alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  coord_flip() +
  labs(
    title = "Distribution of Rate Relativity by SIC Division",
    x = "SIC Division",
    y = "Rate Relativity"
  ) +
  theme_minimal()

ggplot(data_cleaned %>% filter(!is.na(PPM)), 
       aes(x = LayerNumber, y = PPM)) +
  geom_point(alpha = 0.2, color = "grey") +
  geom_smooth(method = "loess", se = FALSE, color = "purple") +
  labs(
    title = "Price Per Million vs. Layer Number",
    x = "Layer Number",
    y = "Price Per Million ($)"
  ) +
  theme_minimal()

ggplot(data_cleaned %>% filter(!is.na(PPM)), 
       aes(x = Capacity, y = PPM)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", se = FALSE, color = "darkgreen") +
  scale_x_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "M")) +
  labs(
    title = "PPM vs. Layer Capacity",
    x = "Capacity ($)",
    y = "Price Per Million"
  ) +
  theme_minimal()

data_cleaned %>%
  group_by(SIC.Division) %>%
  summarise(avg_ppm = mean(PPM, na.rm = TRUE)) %>%
  ggplot(aes(x = fct_reorder(SIC.Division, avg_ppm), y = avg_ppm)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(
    title = "Average PPM by SIC Division",
    x = "SIC Division",
    y = "Avg Price Per Million ($)"
  ) +
  theme_minimal()

data_cleaned %>%
  group_by(ClientID) %>%
  mutate(rate_jump = abs(Relativity - lag(Relativity))) %>%
  filter(rate_jump > 0.5) %>%
  ggplot(aes(x = LayerNumber, y = Relativity, group = ClientID)) +
  geom_line(alpha = 0.2) +
  geom_point(color = "red") +
  labs(
    title = "Towers with Sharp Relativity Changes",
    x = "Layer Number",
    y = "Relativity"
  ) +
  theme_minimal()

ggplot(data_cleaned %>% filter(!is.na(Relativity)), 
       aes(x = factor(LayerNumber), y = Relativity)) +
  geom_boxplot(outlier.alpha = 0.2) +
  labs(
    title = "Rate Relativity Distribution by Layer Number",
    x = "Layer Number",
    y = "Relativity"
  ) +
  theme_minimal()













