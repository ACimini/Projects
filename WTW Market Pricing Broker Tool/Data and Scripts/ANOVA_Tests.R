setwd("C:/Users/alexc/OneDrive/Desktop/Final Files/Data")
load("TheFinalFinalISTGFinal_data.Rdata")


set.seed(123)  # for reproducibility

library(dplyr)

data_cleaned = data_cleaned  |> 
  group_by(ClientID)  |> 
  mutate(UmbrellaPPM = PPM[LayerNumber == 0][1])  |> 
  ungroup()


# Step 1: Get unique tower IDs
unique_ids = unique(data_cleaned$ClientID)

# Step 2: Randomly sample 80% of them for training
train_ids = sample(unique_ids, size = 0.8 * length(unique_ids))

# Step 3: Create training and test sets based on tower ID
train_data = data_cleaned  |> 
  filter(ClientID %in% train_ids)

test_data = data_cleaned  |> 
  filter(!ClientID %in% train_ids)

library(mgcv)

#GAM because of non linear relationship

#Anova tests on whole data
gam_base = gam(Relativity ~ LayerLimit + LayerAttachment, data = train_data)
gam_Base1 = gam(Relativity ~ s(LayerLimit) + s(LayerAttachment), data = train_data)
gam_interaction_base = gam(Relativity ~ te(LayerLimit, LayerAttachment), data = train_data)

anova.gam(gam_base, gam_Base1, gam_interaction_base, test = "F")

gam_inter_UM_PPM = gam(Relativity ~ te(LayerLimit, LayerAttachment) + s(UmbrellaPPM), data = train_data)

anova.gam(gam_inter_UM_PPM)
anova.gam(gam_interaction_base ,gam_inter_UM_PPM, test = "F")

# Predict from the base model
pred_base = predict(gam_interaction_base, newdata = test_data)

# Predict from the enhanced model with UmbrellaPPM
pred_with_UM = predict(gam_inter_UM_PPM, newdata = test_data)

# Compute RMSE for each
rmse_base = sqrt(mean((pred_base - test_data$Relativity)^2, na.rm = TRUE))
rmse_with_UM = sqrt(mean((pred_with_UM - test_data$Relativity)^2, na.rm = TRUE))

# Print results
cat("Base Model RMSE:", rmse_base, "\n")
cat("Model with UmbrellaPPM RMSE:", rmse_with_UM, "\n")

summary(gam_inter_UM_PPM)










# Model 1: SIC Division as a random effect
gam_sic_re = gam(
  Relativity ~ te(LayerLimit, LayerAttachment) + s(SIC.Division, bs = "re"),
  data = train_data
)

# Model 2: SIC Division as a fixed effect
gam_sic_fixed = gam(
  Relativity ~ te(LayerLimit, LayerAttachment) + SIC.Division,
  data = train_data
)

# Model 3: Separate smooths per SIC Division (interaction smooths)
gam_sic_by = gam(
  Relativity ~ te(LayerLimit, LayerAttachment, by = SIC.Division) + SIC.Division,
  data = train_data
)

# Predict and compute RMSE for each model
pred_re = predict(gam_sic_re, newdata = test_data)
rmse_re = sqrt(mean((pred_re - test_data$Relativity)^2, na.rm = TRUE))

pred_fixed = predict(gam_sic_fixed, newdata = test_data)
rmse_fixed = sqrt(mean((pred_fixed - test_data$Relativity)^2, na.rm = TRUE))

pred_by = predict(gam_sic_by, newdata = test_data)
rmse_by = sqrt(mean((pred_by - test_data$Relativity)^2, na.rm = TRUE))

# Print all RMSEs
cat("RMSE (Random Effect):", rmse_re, "\n")
cat("RMSE (Fixed Effect):", rmse_fixed, "\n")
cat("RMSE (By-Smooth):", rmse_by, "\n")









test_data$pred_with_UM = pred_with_UM
test_data$resid_with_UM = test_data$Relativity - pred_with_UM

ggplot(test_data, aes(x = Relativity, y = pred_with_UM)) +
  geom_point(alpha = 0.3, color = "red") +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  labs(
    title = "Predicted vs. Actual Relativity (With Umbrella PPM)",
    x = "Actual Relativity",
    y = "Predicted Relativity"
  ) +
  theme_minimal()

ggplot(test_data, aes(x = pred_with_UM, y = resid_with_UM)) +
  geom_point(alpha = 0.3, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Residuals vs. Predicted Relativity (With Umbrella PPM)",
    x = "Predicted Relativity",
    y = "Residual (Actual - Predicted)"
  ) +
  theme_minimal()

ggplot(test_data, aes(x = resid_with_UM)) +
  geom_histogram(bins = 40, fill = "red", color = "white") +
  labs(
    title = "Distribution of Prediction Errors (With Umbrella PPM)",
    x = "Residual",
    y = "Count"
  ) +
  theme_minimal()




plot(gam_inter_UM_PPM, select = 1, se = TRUE, shade = TRUE, col = "darkgreen", 
     main = "Effect of Umbrella PPM on Rate Relativity")




vis.gam(gam_inter_UM_PPM, view = c("LayerLimit", "LayerAttachment"), 
        plot.type = "contour", color = "topo", main = "GAM Surface for LayerLimit & Attachment")

vis.gam(gam_inter_UM_PPM,
        view = c("LayerLimit", "LayerAttachment"),
        plot.type = "persp",    # 3D perspective
        theta = 50, phi = 30,   # angles
        ticktype = "detailed",
        color = "topo",
        main = "3D GAM Surface: LayerLimit vs. LayerAttachment")


vis.gam(gam_inter_UM_PPM,
        view = c("LayerLimit", "LayerAttachment"),
        plot.type = "persp",
        theta = 45, phi = 30,
        ticktype = "detailed",
        color = "topo",
        main = "",              # Turn off default title
        xlab = "Layer Limit",
        ylab = "Layer Attachment",
        zlab = "Estimated Relativity")
title("3D GAM Surface: LayerLimit vs. LayerAttachment", line = 2.5)






  
        
library(ggplot2)

preds_with_se = predict(gam_inter_UM_PPM, newdata = test_data, se.fit = TRUE)

# Store predicted values and standard errors in test_data
test_data$fit = preds_with_se$fit
test_data$se = preds_with_se$se.fit

# Calculate 95% confidence interval bounds
test_data$upper = test_data$fit + 1.96 * test_data$se
test_data$lower = test_data$fit - 1.96 * test_data$se


ggplot(test_data, aes(x = Relativity, y = fit)) +
  geom_point(alpha = 0.3, color = "red") +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, alpha = 0.2) +
  labs(
    title = "Predicted vs. Actual Relativity (With Umbrella PPM) with 95% CI",
    x = "Actual Relativity",
    y = "Predicted Relativity"
  ) +
  theme_minimal()


gam_UM_Full = gam(Relativity ~ te(LayerLimit, LayerAttachment) + s(UmbrellaPPM), data = data_cleaned)

summary(gam_UM_Full)
formula(gam_UM_Full)
gam_UM_Full$smooth
coef(gam_UM_Full)




library(dplyr)

test_data_clean = test_data  |> 
  mutate(
    pred_with_UM = unlist(pred_with_UM),
    LayerNumber = unlist(LayerNumber),
    UmbrellaPPM = unlist(UmbrellaPPM),
    LayerLimit = unlist(LayerLimit),
    LayerPremium = unlist(LayerPremium)
  )  |> 
  arrange(ClientID, LayerNumber)


layer_avg = test_data_clean  |> 
  group_by(ClientID, LayerNumber)  |> 
  summarise(
    avg_rel = mean(pred_with_UM, na.rm = TRUE),
    limit = mean(LayerLimit, na.rm = TRUE),
    umbrella_ppm = first(UmbrellaPPM),
    .groups = "drop"
  )  |> 
  arrange(ClientID, LayerNumber)


layer_with_ppm = layer_avg  |> 
  group_by(ClientID)  |> 
  mutate(predicted_ppm = {
    ppm_vec = numeric(n())
    ppm_vec[1] = umbrella_ppm[1]  
    for (i in 2:n()) {
      ppm_vec[i] = ppm_vec[i - 1] * avg_rel[i]
    }
    ppm_vec
  })  |> 
  mutate(predicted_premium = predicted_ppm * (limit / 1e6))  |> 
  select(ClientID, LayerNumber, predicted_ppm, predicted_premium)

test_data_final = test_data_clean  |> 
  left_join(layer_with_ppm, by = c("ClientID", "LayerNumber"))  |> 
  mutate(
    mape = abs(predicted_premium - LayerPremium) / LayerPremium
  )

mean_mape = mean(test_data_final$mape, na.rm = TRUE)
cat("MAPE :", round(mean_mape * 100, 2), "%\n")





