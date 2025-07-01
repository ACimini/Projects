

library(mgcv)
library(quantreg)

#Sets the Working Directory to the folder the R file is in
# Set working directory to script location — works in Rscript and RStudio
get_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  path <- sub("--file=", "", args[grep("--file=", args)])
  if (length(path) == 0) {
    # Fallback if running interactively
    path <- rstudioapi::getSourceEditorContext()$path
  }
  return(normalizePath(path))
}

get_script_folder <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  path <- sub("--file=", "", args[grep("--file=", args)])
  if (length(path) == 0) {
    # Fallback for RStudio
    path <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(normalizePath(path)))
}

script_folder <- get_script_folder()


script_path = get_script_path()
setwd(dirname(script_path))

#Reads in the input file with the needed information
input = read.csv("temp_input.csv")

input_attachment = as.numeric(input[1,])
input_limit = as.numeric(input[2,])
input_UM_PPM = as.numeric(input[3,])

layer_1_attachment = as.numeric(input[4,])
layer_1_limit = as.numeric(input[5,])
layer_1_prem = as.numeric(input[6,])

um_attachment = as.numeric(input[7,])
um_limit = as.numeric(input[8,])
um_prem = as.numeric(input[9,])


quinn_model_predict = function(input_limit, input_attachment, lower_layers) {
  if (nrow(lower_layers) < 2) {
    warning("Need at least 2 layers to predict upper layer")
    return(NA)
  }
  
  if (!exists("collapsed_data_all")) {
    collapsed_data_all <<- read.csv("collapsed.csv")
  }
  
  if (!exists("qmodel") || !exists("lmodel") || !exists("KNOWN_LAYER_COUNT") || KNOWN_LAYER_COUNT != nrow(lower_layers)) {
    KNOWN_LAYER_COUNT <<- nrow(lower_layers)
    collapsed_data = collapsed_data_all[collapsed_data_all$layers_in_tower > KNOWN_LAYER_COUNT, ]
    collapsed_data$midpoint = collapsed_data$attachment + collapsed_data$limit / 2
    
    qx = collapsed_data$midpoint / 1e6
    qy = collapsed_data$lead_ratio
    qmodel <<- nlrq(qy ~ a / (qx + exp(b)), start = list(a = 1, b = 0.05), tau = 0.5)
    qcoef = coef(qmodel)
    qmodel_lo_50 <<- nlrq(qy ~ a / (qx + exp(b)), start = as.list(qcoef), tau = 0.5-0.5/2)
    qmodel_hi_50 <<- nlrq(qy ~ a / (qx + exp(b)), start = as.list(qcoef), tau = 0.5+0.5/2)
    qmodel_lo_90 <<- nlrq(qy ~ a / (qx + exp(b)), start = as.list(qcoef), tau = 0.5-0.9/2)
    qmodel_hi_90 <<- nlrq(qy ~ a / (qx + exp(b)), start = as.list(qcoef), tau = 0.5+0.9/2)
    qmodel_lo_95 <<- nlrq(qy ~ a / (qx + exp(b)), start = as.list(qcoef), tau = 0.5-0.95/2)
    qmodel_hi_95 <<- nlrq(qy ~ a / (qx + exp(b)), start = as.list(qcoef), tau = 0.5+0.95/2)
    
    collapsed_data$pred_lead_ratio = predict(qmodel, newdata = data.frame(qx = collapsed_data$midpoint/1e6))
    collapsed_data$ratio_scalar = collapsed_data$pred_lead_ratio / collapsed_data$lead_ratio
    
    lxr = rep(
      collapsed_data[collapsed_data$layer_number==KNOWN_LAYER_COUNT-1, ]$ratio_scalar,
      times=collapsed_data[collapsed_data$layer_number==KNOWN_LAYER_COUNT-1, ]$layers_in_tower - KNOWN_LAYER_COUNT
    )
    lyr = collapsed_data[collapsed_data$layer_number>=KNOWN_LAYER_COUNT, ]$ratio_scalar
    lmodel <<- lm(log(lyr) ~ log(lxr))
  }
  
  # Find the lead and top known layer
  lead_layer = lower_layers[1, ]
  top_known_layer = lower_layers[KNOWN_LAYER_COUNT, ]
  
  # Calculate the lead ratio of the top layer
  lead_per_mil = lead_layer$premium / (lead_layer$limit / 1e6)
  top_per_mil = top_known_layer$premium / (top_known_layer$limit / 1e6)
  top_lead_ratio = top_per_mil / lead_per_mil
  
  # Predict the top lead ratio
  pred_top_lead_ratio = predict(
    qmodel,
    newdata = data.frame(qx = (top_known_layer$attachment + top_known_layer$limit / 2) / 1e6)
  )
  
  newdata = data.frame(qx = (input_attachment + input_limit / 2) / 1e6)
  query_lead_ratios = predict(qmodel, newdata = newdata)
  query_lead_ratios_lo_50 = predict(qmodel_lo_50, newdata = newdata)
  query_lead_ratios_hi_50 = predict(qmodel_hi_50, newdata = newdata)
  query_lead_ratios_lo_90 = predict(qmodel_lo_90, newdata = newdata)
  query_lead_ratios_hi_90 = predict(qmodel_hi_90, newdata = newdata)
  query_lead_ratios_lo_95 = predict(qmodel_lo_95, newdata = newdata)
  query_lead_ratios_hi_95 = predict(qmodel_hi_95, newdata = newdata)
  
  top_ratio_scalar = pred_top_lead_ratio / top_lead_ratio
  
  # Calculate epsilon
  epsilon = exp(predict(
    lmodel,
    newdata = data.frame(lxr=top_ratio_scalar)
  ))
  query_scaled_lead_ratios <<- query_lead_ratios / epsilon
  
  # Calculate rate relativities
  rr = c(query_scaled_lead_ratios, NA) / c(top_lead_ratio, query_scaled_lead_ratios)
  rr = rr[-length(rr)]
  
  premiums = query_scaled_lead_ratios * lead_per_mil * input_limit / 1e6
  
  return(data.frame(
    rate_rels = rr,
    attachment = input_attachment,
    limit = input_limit,
    ppm = premiums / (input_limit / 1e6),
    premium = premiums,
    ci_50_lo = (query_scaled_lead_ratios * query_lead_ratios_lo_50 / query_lead_ratios) * lead_per_mil * input_limit / 1e6,
    ci_50_hi = (query_scaled_lead_ratios * query_lead_ratios_hi_50 / query_lead_ratios) * lead_per_mil * input_limit / 1e6,
    ci_90_lo = (query_scaled_lead_ratios * query_lead_ratios_lo_90 / query_lead_ratios) * lead_per_mil * input_limit / 1e6,
    ci_90_hi = (query_scaled_lead_ratios * query_lead_ratios_hi_90 / query_lead_ratios) * lead_per_mil * input_limit / 1e6,
    ci_95_lo = (query_scaled_lead_ratios * query_lead_ratios_lo_95 / query_lead_ratios) * lead_per_mil * input_limit / 1e6,
    ci_95_hi = (query_scaled_lead_ratios * query_lead_ratios_hi_95 / query_lead_ratios) * lead_per_mil * input_limit / 1e6
  ))
}






QR_df = data.frame(
  attachment = c(um_attachment, layer_1_attachment),
  limit = c(um_limit, layer_1_limit),
  premium = c(um_prem, layer_1_prem)
)

qr_pred = quinn_model_predict(input_limit,input_attachment,
                              QR_df)












new_layer = data.frame(
  LayerLimit = input_limit,
  LayerAttachment = input_attachment,
  UmbrellaPPM = input_UM_PPM
)

#Loads in the model data
data = read.csv("Final_Data.csv")

#Computes the actual GAM
gam_test = gam(Relativity ~ te(LayerAttachment, LayerLimit) + s(UmbrellaPPM), data = data)


#predicts the RR from the GAM
Prediction = predict(gam_test, newdata = new_layer, se.fit = TRUE)

predRR = Prediction$fit
SE = Prediction$se.fit

# Calculate 95% confidence interval bounds
upper = predRR + 1.96 * SE
lower = predRR - 1.96 * SE


#gets the output
output = data.frame(as.numeric(predRR),upper,lower, qr_pred[,5],qr_pred[,10], qr_pred[,11])

#Writes to the CSV
write.csv(output, "temp_output.csv", row.names = FALSE)

