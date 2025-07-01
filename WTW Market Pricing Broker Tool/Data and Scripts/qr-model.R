library(quantreg)

### SETUP

KNOWN_LAYER_COUNT = 2
stopifnot(KNOWN_LAYER_COUNT > 1)

collapsed_train = collapsed[collapsed$is_train2==1 & collapsed$layers_in_tower > KNOWN_LAYER_COUNT, ]
collapsed_test = collapsed[collapsed$is_train2==0 & collapsed$layers_in_tower > KNOWN_LAYER_COUNT, ]

### MAKE QUANTILE

qx = collapsed_train$midpoint / 1e6
qy = collapsed_train$lead_ratio
qmodel = nlrq(qy ~ a / (qx + exp(b)), start = list(a = 1, b = 0.05), tau = 0.5)
qcoef = coef(qmodel)
qmodel25 = nlrq(qy ~ a / (qx + exp(b)), start = as.list(qcoef), tau = 0.25)
qmodel75 = nlrq(qy ~ a / (qx + exp(b)), start = as.list(qcoef), tau = 0.75)
qmodel10 = nlrq(qy ~ a / (qx + exp(b)), start = as.list(qcoef), tau = 0.1)
qmodel90 = nlrq(qy ~ a / (qx + exp(b)), start = as.list(qcoef), tau = 0.9)

plot(
    qx * 1e6,
    qy,
    # col=collapsed_train$colors,
    col="darkgrey",
    cex=0.2,
    xlab="Layer Midpoint ($)",
    ylab="Lead Ratio",
    main="Quantile Regression on Lead Ratio"
)
qpred = predict(qmodel, newdata = data.frame(qx = 1:200))
lines((1:200) * 1e6, qpred, col = "black", lwd = 2)
qpred25 = predict(qmodel25, newdata = data.frame(qx = 1:200))
lines((1:200) * 1e6, qpred25, col = "black", lwd = 2, lty=2)
qpred75 = predict(qmodel75, newdata = data.frame(qx = 1:200))
lines((1:200) * 1e6, qpred75, col = "black", lwd = 2, lty=2)
qpred10 = predict(qmodel10, newdata = data.frame(qx = 1:200))
lines((1:200) * 1e6, qpred10, col = "black", lwd = 2, lty=3)
qpred90 = predict(qmodel90, newdata = data.frame(qx = 1:200))
lines((1:200) * 1e6, qpred90, col = "black", lwd = 2, lty=3)

# wtf = collapsed[collapsed$client_id==101, ]
# lines(wtf$midpoint, wtf$lead_ratio, col="red")

### ADD PREDICTED LEAD RATIOS

# integration_prediction = function(limit, attachment) {
#     lower_x = attachment / 1e6
#     upper_x = (attachment + limit) / 1e6
#     a = qcoef[1]
#     b = qcoef[2]
#     lower_anti = a * log(lower_x + b)
#     upper_anti = a * log(upper_x + b)
#     area = upper_anti - lower_anti
#     return(area / (upper_x - lower_x))
# }

collapsed_train$pred_lead_ratio10 = predict(qmodel10, newdata = data.frame(qx = collapsed_train$midpoint/1e6))
collapsed_train$pred_lead_ratio25 = predict(qmodel25, newdata = data.frame(qx = collapsed_train$midpoint/1e6))
collapsed_train$pred_lead_ratio = predict(qmodel, newdata = data.frame(qx = collapsed_train$midpoint/1e6))
collapsed_train$pred_lead_ratio75 = predict(qmodel75, newdata = data.frame(qx = collapsed_train$midpoint/1e6))
collapsed_train$pred_lead_ratio90 = predict(qmodel90, newdata = data.frame(qx = collapsed_train$midpoint/1e6))

# collapsed_train$ratio_scalar10 = collapsed_train$pred_lead_ratio10 / collapsed_train$lead_ratio
# collapsed_train$ratio_scalar25 = collapsed_train$pred_lead_ratio25 / collapsed_train$lead_ratio
collapsed_train$ratio_scalar = collapsed_train$pred_lead_ratio / collapsed_train$lead_ratio
# collapsed_train$ratio_scalar75 = collapsed_train$pred_lead_ratio75 / collapsed_train$lead_ratio
# collapsed_train$ratio_scalar90 = collapsed_train$pred_lead_ratio90 / collapsed_train$lead_ratio

# collapsed_train$pred_lead_ratio_area = integration_prediction(collapsed_train$limit, collapsed_train$attachment)
# collapsed_train$ratio_scalar_area = collapsed_train$pred_lead_ratio_area / collapsed_train$lead_ratio

collapsed_test$pred_lead_ratio10 = predict(qmodel10, newdata = data.frame(qx = collapsed_test$midpoint/1e6))
collapsed_test$pred_lead_ratio25 = predict(qmodel25, newdata = data.frame(qx = collapsed_test$midpoint/1e6))
collapsed_test$pred_lead_ratio = predict(qmodel, newdata = data.frame(qx = collapsed_test$midpoint/1e6))
collapsed_test$pred_lead_ratio75 = predict(qmodel75, newdata = data.frame(qx = collapsed_test$midpoint/1e6))
collapsed_test$pred_lead_ratio90 = predict(qmodel90, newdata = data.frame(qx = collapsed_test$midpoint/1e6))

### BUILD REGRESSION

lxr = rep(
    collapsed_train[collapsed_train$layer_number==KNOWN_LAYER_COUNT-1, ]$ratio_scalar,
    times=collapsed_train[collapsed_train$layer_number==KNOWN_LAYER_COUNT-1, ]$layers_in_tower - KNOWN_LAYER_COUNT
)
lxa = rep(
    collapsed_train[collapsed_train$layer_number==KNOWN_LAYER_COUNT-1, ]$ratio_scalar_area,
    times=collapsed_train[collapsed_train$layer_number==KNOWN_LAYER_COUNT-1, ]$layers_in_tower - KNOWN_LAYER_COUNT
)
lxm = collapsed_train[collapsed_train$layer_number>=KNOWN_LAYER_COUNT, ]$midpoint / 1e6
lxi = collapsed_train[collapsed_train$layer_number>=KNOWN_LAYER_COUNT, ]$layer_number
lxl = collapsed_train[collapsed_train$layer_number>=KNOWN_LAYER_COUNT, ]$limit / 1e6
# lyr10 = collapsed_train[collapsed_train$layer_number>=KNOWN_LAYER_COUNT, ]$ratio_scalar10
# lyr25 = collapsed_train[collapsed_train$layer_number>=KNOWN_LAYER_COUNT, ]$ratio_scalar25
lyr = collapsed_train[collapsed_train$layer_number>=KNOWN_LAYER_COUNT, ]$ratio_scalar
# lyr75 = collapsed_train[collapsed_train$layer_number>=KNOWN_LAYER_COUNT, ]$ratio_scalar75
# lyr90 = collapsed_train[collapsed_train$layer_number>=KNOWN_LAYER_COUNT, ]$ratio_scalar90

# lmodel10 = lm(log(lyr10) ~ log(lxr))
# lmodel25 = lm(log(lyr25) ~ log(lxr))
lmodel = lm(log(lyr) ~ log(lxr))
# lmodel75 = lm(log(lyr75) ~ log(lxr))
# lmodel90 = lm(log(lyr90) ~ log(lxr))

resid = collapsed_train$lead_ratio - predict(qmodel)
q_lo_50 = quantile(resid, 0.25)
q_hi_50 = quantile(resid, 0.75)
q_lo_80 = quantile(resid, 0.10)
q_hi_80 = quantile(resid, 0.90)

### VERIFY TRAIN FIT AND BUILD OUT DATA FOR ENSEMBLE

train_ratio_errors = c()
interval_50_count = 0
interval_80_count = 0
interval_denom = 0

collapsed_train$quinn_pred_rr = NA

for (client_id in unique(collapsed_train$client_id)) {
    client_program = collapsed_train[collapsed_train$client_id == client_id, ]
    
    top_known_layer = client_program[client_program$layer_number==KNOWN_LAYER_COUNT-1, ] #Top excess
    top_ratio_scalar = top_known_layer$pred_lead_ratio[1] / top_known_layer$lead_ratio[1]
    
    client_program_prediction = client_program[client_program$layer_number>=KNOWN_LAYER_COUNT, ]
    
    newdata = data.frame(
        lxr=top_ratio_scalar,
        lxi=client_program_prediction$layer_number,
        lxm=client_program_prediction$midpoint / 1e6,
        lxl=client_program_prediction$limit / 1e6
    )
    epsilon = exp(predict(lmodel, newdata = newdata))

    pred_scaled_lead_ratios = client_program_prediction$pred_lead_ratio / epsilon
    actu_lead_ratios = client_program_prediction$lead_ratio
    ratios = pred_scaled_lead_ratios / actu_lead_ratios
    train_ratio_errors = c(train_ratio_errors, ratios)
    
    # Evaluate 50% confidence interval
    pred_scaled_lead_ratios25 = pred_scaled_lead_ratios * (client_program_prediction$pred_lead_ratio25 / client_program_prediction$pred_lead_ratio)
    pred_scaled_lead_ratios75 = pred_scaled_lead_ratios * (client_program_prediction$pred_lead_ratio75 / client_program_prediction$pred_lead_ratio)
    in_50_interval = client_program_prediction$lead_ratio < pred_scaled_lead_ratios75 & client_program_prediction$lead_ratio > pred_scaled_lead_ratios25
    interval_50_count = interval_50_count + sum(in_50_interval)
    
    # Evaluate 80% confidence interval
    pred_scaled_lead_ratios10 = pred_scaled_lead_ratios * (client_program_prediction$pred_lead_ratio10 / client_program_prediction$pred_lead_ratio)
    pred_scaled_lead_ratios90 = pred_scaled_lead_ratios * (client_program_prediction$pred_lead_ratio90 / client_program_prediction$pred_lead_ratio)
    in_80_interval = client_program_prediction$lead_ratio < pred_scaled_lead_ratios90 & client_program_prediction$lead_ratio > pred_scaled_lead_ratios10
    interval_80_count = interval_80_count + sum(in_80_interval)
    
    interval_denom = interval_denom + nrow(client_program_prediction)
    
    # Add predicted rrs for ensemble
    rr = c(pred_scaled_lead_ratios, NA) / c(top_known_layer$lead_ratio, pred_scaled_lead_ratios)
    rr = rr[-length(rr)]
    collapsed_train[collapsed_train$client_id == client_id, ]$quinn_pred_rr = c(rep(NA, KNOWN_LAYER_COUNT), rr)
}

print(mean(train_ratio_errors))
rm(train_ratio_errors)

print(sprintf("Expected 50%% in, found %.2f%%", interval_50_count / interval_denom * 100))
print(sprintf("Expected 80%% in, found %.2f%%", interval_80_count / interval_denom * 100))



##### FORECAST NEW LAYERS #####



ratio_errors = c()
quinn_prem_errors = c()
quinn_rr_errors = c()
quinn_layer_numbers = c()
quinn_layer_limits = c()
quinn_actual_rate_rels = c()
quinn_predicted_rate_rels = c()
interval_50_count = 0
interval_80_count = 0
interval_denom = 0

collapsed_test$quinn_pred_rr = NA

for (client_id in unique(collapsed_test$client_id)) {
    client_program = collapsed_test[collapsed_test$client_id == client_id, ]

    top_known_layer = client_program[client_program$layer_number==KNOWN_LAYER_COUNT-1, ] #Top excess
    top_ratio_scalar = top_known_layer$pred_lead_ratio[1] / top_known_layer$lead_ratio[1]
    # second_known_layer = client_program[client_program$layer_number==KNOWN_LAYER_COUNT-2, ] #Second to top excess
    # second_ratio_scalar = second_known_layer$pred_lead_ratio[1] / second_known_layer$lead_ratio[1]

    client_program_prediction = client_program[client_program$layer_number>=KNOWN_LAYER_COUNT, ]

    epsilon = (predict(
        lmodel,
        newdata = data.frame(
            lxr=top_ratio_scalar,
            lxi=client_program_prediction$layer_number,
            lxm=client_program_prediction$midpoint / 1e6,
            lxl=client_program_prediction$limit / 1e6
        )
    ))
    epsilon = exp(epsilon)

    pred_scaled_lead_ratios = client_program_prediction$pred_lead_ratio / epsilon
    actu_lead_ratios = client_program_prediction$lead_ratio

    ratios = pred_scaled_lead_ratios / actu_lead_ratios
    ratio_errors = c(ratio_errors, ratios)

    # Calculate MAPE
    lead_ppm = client_program$per_mil[1]
    pred_premium = client_program_prediction$limit / 1e6 * pred_scaled_lead_ratios * lead_ppm
    actu_premium = client_program_prediction$premium
    perc_err = (pred_premium - actu_premium) / actu_premium
    quinn_prem_errors = c(quinn_prem_errors, perc_err)

    # Calculate RMSE RR
    rr = c(pred_scaled_lead_ratios, NA) / c(top_known_layer$lead_ratio, pred_scaled_lead_ratios)
    rr = rr[-length(rr)]
    quinn_actual_rate_rels = c(quinn_actual_rate_rels, client_program_prediction$rate_rel)
    quinn_predicted_rate_rels = c(quinn_predicted_rate_rels, rr)
    quinn_layer_numbers = c(quinn_layer_numbers, client_program_prediction$layer_number)
    quinn_layer_limits = c(quinn_layer_limits, client_program_prediction$limit)
    perc_err_rr = (rr - client_program_prediction$rate_rel) / client_program_prediction$rate_rel
    quinn_rr_errors = c(quinn_rr_errors, perc_err_rr)
    
    collapsed_test[collapsed_test$client_id == client_id, ]$quinn_pred_rr = c(
        rep(NA, KNOWN_LAYER_COUNT),
        rr
    )

    # Calculate Intervals

    # Evaluate 50% confidence interval
    pred_scaled_lead_ratios25 = pred_scaled_lead_ratios * (client_program_prediction$pred_lead_ratio25 / client_program_prediction$pred_lead_ratio)
    pred_scaled_lead_ratios75 = pred_scaled_lead_ratios * (client_program_prediction$pred_lead_ratio75 / client_program_prediction$pred_lead_ratio)
    in_50_interval = client_program_prediction$lead_ratio < pred_scaled_lead_ratios75 & client_program_prediction$lead_ratio > pred_scaled_lead_ratios25
    interval_50_count = interval_50_count + sum(in_50_interval)
    
    # Evaluate 80% confidence interval
    pred_scaled_lead_ratios10 = pred_scaled_lead_ratios * (client_program_prediction$pred_lead_ratio10 / client_program_prediction$pred_lead_ratio)
    pred_scaled_lead_ratios90 = pred_scaled_lead_ratios * (client_program_prediction$pred_lead_ratio90 / client_program_prediction$pred_lead_ratio)
    in_80_interval = client_program_prediction$lead_ratio < pred_scaled_lead_ratios90 & client_program_prediction$lead_ratio > pred_scaled_lead_ratios10
    interval_80_count = interval_80_count + sum(in_80_interval)

    interval_denom = interval_denom + nrow(client_program_prediction)
}

print(sprintf("Expected 50%% in, found %.2f%%", interval_50_count / interval_denom * 100))
print(sprintf("Expected 80%% in, found %.2f%%", interval_80_count / interval_denom * 100))

# print(mean(quinn_prem_errors))
(quinn_rmse_rr = sqrt(mean( (quinn_actual_rate_rels-quinn_predicted_rate_rels)^2 )))
(quinn_mape_pr = mean(abs(quinn_prem_errors)))
(quinn_mape_rr = mean(abs(quinn_rr_errors)))
