library(tidyverse)
library(keras3)
library(tensorflow)

set.seed(1001)

setwd("C:/Users/alexc/OneDrive/Desktop/ECNS 460/GitHub Materials/460-Term-Project/Data")
data_date = read.csv("Complete_Dataset.csv")
data = read.csv("Complete_Dataset.csv")

data <- data[nrow(data):1, ]
data_date = data_date[nrow(data_date):1, ]
data$Date = as.Date(data$Date)

data = data |> select(- X, - Date, - Event) #|> mutate(Date = as.Date(Date))

close_scaled <- scale(data$StockClose)
predictors_scaled <- scale(data[, -which(colnames(data) == "StockClose")])
data_scaled <- cbind(close_scaled, predictors_scaled)


# Parameters
window_size <- 10  # Length of each sequence
num_samples <- nrow(data_scaled) - window_size
n_features <- ncol(data_scaled)

# Define indices for training, validation, and test sets
train_end <- floor(0.65 * num_samples)  # 65% for training
val_end <- floor(0.85 * num_samples)    # Next 20% for validation

# Initialize arrays for input sequences and targets
X_train <- array(0, dim = c(train_end, window_size, n_features))
y_train <- numeric(train_end)

X_val <- array(0, dim = c(val_end - train_end, window_size, n_features))
y_val <- numeric(val_end - train_end)

X_test <- array(0, dim = c(num_samples - val_end, window_size, n_features))
y_test <- numeric(num_samples - val_end)

# Populate the arrays
for (i in 1:train_end) {
  X_train[i, , ] <- data_scaled[i:(i + window_size - 1), , drop = FALSE]
  y_train[i] <- data_scaled[i + window_size, 1]
}

for (i in (train_end + 1):val_end) {
  idx <- i - train_end  # Adjust index for validation array
  X_val[idx, , ] <- data_scaled[i:(i + window_size - 1), , drop = FALSE]
  y_val[idx] <- data_scaled[i + window_size, 1]
}

for (i in (val_end + 1):num_samples) {
  idx <- i - val_end  # Adjust index for test array
  X_test[idx, , ] <- data_scaled[i:(i + window_size - 1), , drop = FALSE]
  y_test[idx] <- data_scaled[i + window_size, 1]
}

# Define the LSTM model
model <- keras_model_sequential() %>%
  layer_lstm(units = 125, input_shape = c(window_size, n_features)) %>%
  layer_dense(units = 1)

# Compile the model
model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam()
)

# Train the model using the training and validation sets
history <- model %>% fit(
  x = X_train,
  y = y_train,
  epochs = 50,
  batch_size = 32,
  validation_data = list(X_val, y_val)
)

# Evaluate the model on the test set
test_loss <- model %>% evaluate(X_test, y_test)

# Make predictions on the test set
predictions <- model %>% predict(X_test)

# Denormalize predictions and actual values
predictions_denorm <- predictions * attr(close_scaled, "scaled:scale") + attr(close_scaled, "scaled:center")
actual_values_test <- y_test * attr(close_scaled, "scaled:scale") + attr(close_scaled, "scaled:center")

# Extract the corresponding dates for the test set
test_dates <- data_date$Date[(val_end + window_size + 1):(num_samples + window_size)]

# Combine dates with actual and predicted values
comparison <- data.frame(
  Date = test_dates,
  Actual = actual_values_test,
  Predicted = predictions_denorm
)
comparison$Date = as.Date(comparison$Date)

# Plot actual vs predicted for the test set
main_plot = ggplot(comparison, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), col = 'black') +
  geom_line(aes(y = Predicted, color = "Predicted"), col = 'red') +
  labs(title = "Actual vs Predicted Stock Prices (Test Set)", x = "Date", y = "Price") +
  theme_bw()

ggsave("Results.png", plot = main_plot, width = 15, height = 5, dpi = 300)


comparison$Model_error = comparison$Actual - comparison$Predicted

Errors = ggplot(comparison, aes(x = Date, y = Model_error)) + geom_line() + geom_line(aes(y=0)) + theme_bw()

ggsave("Errors.png", plot = Errors, width = 15, height = 5, dpi = 300)

acf(comparison$Model_error, 
    main = "Autocorrelation of Model Errors",  
    xlab = "Lag",                             
    ylab = "ACF")

data_date$Date = as.Date(data_date$Date)

SP = ggplot(data_date, aes(x = Date, y = StockClose)) + geom_line() +
  labs(title = "Microsoft Stock Price", y = "Price",x = "Date") +
  theme_bw()

ggsave("SP.png", plot = SP, width = 15, height = 5, dpi = 300)

# Define split points
train_end_date <- as.Date(data_date$Date[train_end + window_size])  # End of training set
val_end_date <- as.Date(data_date$Date[val_end + window_size])      # End of validation set

split = ggplot(data_date, aes(x = Date, y = StockClose)) + 
  geom_line() +
  geom_vline(xintercept = train_end_date, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = val_end_date, color = "darkred", linetype = "dashed", size = 1) +
  labs(
    title = "Microsoft Stock Price with Data Splits",
    y = "Price",
    x = "Date"
  ) +
  annotate("text", x = train_end_date, y = max(data_date$StockClose, na.rm = TRUE), 
           label = "Training/Validation Split", color = "blue", vjust = 2) +
  annotate("text", x = val_end_date, y = max(data_date$StockClose, na.rm = TRUE), 
           label = "Validation/Test Split", color = "darkred") +
  theme_bw()

ggsave("split.png", plot = split, width = 15, height = 5, dpi = 300)
