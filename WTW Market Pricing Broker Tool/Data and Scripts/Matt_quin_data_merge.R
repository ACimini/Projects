library(dplyr)

data_matt = read.csv("data_cleaned_matt.csv")
data_quin = read.csv("final_data_with_sic.csv")


# Step 1: Rename `id` in data_quin to match `ClientID`
data_quin <- data_quin %>%
  rename(ClientID = id)

# Step 2: Join to bring in `sic_code` and overwrite old SIC
data_matt_updated <- data_matt %>%
  select(-SIC.Code) %>%  # Remove the old SIC
  left_join(data_quin %>% select(ClientID, sic_code), by = "ClientID") %>%
  rename(SIC.Code = sic_code)  # Rename to match old structure

write.csv(data_matt_updated, file = "Matt_data_with_SIC.csv", row.names = FALSE)


data_alex = read.csv("Final_Clean_data.csv")

length(unique(data_matt_updated$SIC.Code))

length(unique(data_alex$id))

ggplot(data_alex %>% filter(!is.na(rate_relativity)), 
       aes(x = attachment, y = rate_relativity)) +
  geom_point(alpha = 0.1, size = 1, color = "black") +
  geom_smooth(method = "loess", se = FALSE, color = "purple", size = 1.2) +
  labs(
    title = "Rate Relativity by Layer Number",
    x = "Layer Number",
    y = "Rate Relativity"
  ) +
  theme_minimal()

length(unique(data_cleaned$ClientID))
