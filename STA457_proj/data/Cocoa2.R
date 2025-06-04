# Set your file path (adjust if needed)
file_path <- "/Users/karimhijazi/Desktop/coco457/Daily Prices_ICCO.csv"  # Cocoa price data

# Load libraries
library(readr)
library(ggplot2)
library(scales)
library(dplyr)
library(forecast)
library(zoo)

# ------------------------------------------------------------- #
# Load cocoa data
cocoa_data <- read_csv(file_path)
cocoa_data$Date <- as.Date(cocoa_data$Date, format = "%d/%m/%Y")

# Plot cocoa prices
ggplot(cocoa_data, aes(x = Date, y = `ICCO daily price (US$/tonne)`)) +
  geom_line(color = "brown", alpha = 0.6) +
  labs(title = "Daily Cocoa Prices Over Time",
       x = "Year",
       y = "Price (USD/tonne)") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ------------------------------------------------------------- #
# Load and prepare Ghana weather
ghana_data <- read_csv("/Users/karimhijazi/Desktop/coco457/Ghana_weather.csv")
names(ghana_data)[names(ghana_data) == "DATE"] <- "Date"
ghana_data$Date <- as.Date(ghana_data$Date, format = "%d/%m/%Y")
ghana_weather <- ghana_data %>%
  group_by(Date) %>%
  summarise(
    TAVG = mean(as.numeric(TAVG), na.rm = TRUE),
    PRCP = mean(as.numeric(PRCP), na.rm = TRUE)
  )

# Load and prepare Ivory Coast weather
ivory_data <- read_csv("/Users/karimhijazi/Desktop/coco457/Ivory_weather.csv")

# Convert Date column from character to Date
ivory_data$Date <- as.Date(ivory_data$Date, format = "%Y/%m/%d")

# Group by date and calculate daily means
ivory_weather <- ivory_data %>%
  group_by(Date) %>%
  summarise(
    IC_TAVG = mean(as.numeric(TAVG), na.rm = TRUE),
    IC_PRCP = mean(as.numeric(PRCP), na.rm = TRUE)
  )

# Load cleaned Ghana and Ivory Coast currency data
ghana_curr <- read_csv("/Users/karimhijazi/Desktop/coco457/cleaned_ghana_cedi.csv")
ivory_curr <- read_csv("/Users/karimhijazi/Desktop/coco457/cleaned_xof_cfa.csv")


#Load and clean Nigera, Cameroon, Ecuador weather
# Load
ecuador <- read_csv("/Users/karimhijazi/Desktop/coco457/Ecuador_data.csv")
nigeria <- read_csv("/Users/karimhijazi/Desktop/coco457/Nigeria_data.csv")
cameroon <- read_csv("/Users/karimhijazi/Desktop/coco457/Cameroon_data.csv")

#rename DATE
ecuador <- ecuador %>%
  rename(Date = DATE,
         TAVG = TEMP)

nigeria <- nigeria %>%
  rename(Date = DATE,
         TAVG = TEMP)

cameroon <- cameroon %>%
  rename(Date = DATE,
         TAVG = TEMP)

ecuador <- ecuador %>%
  group_by(Date) %>%
  summarise(TAVG_EC = mean(TAVG, na.rm = FALSE))

nigeria <- nigeria %>%
  group_by(Date) %>%
  summarise(TAVG_NI = mean(TAVG, na.rm = FALSE))

cameroon <- cameroon %>%
  group_by(Date) %>%
  summarise(TAVG_CAM = mean(TAVG, na.rm = FALSE))
# ------------------------------------------------------------- #

#check the available dates
range(cocoa_data$Date)
range(ghana_weather$Date)
range(ivory_weather$Date)
range(ghana_curr$Date)
range(ivory_curr$Date)
range(ecuador$Date)
range(nigeria$Date)
range(cameroon$Date)

start_date <- as.Date("1999-02-01")
end_date <- as.Date("2024-11-28")

ghana_weather <- filter(ghana_weather, Date >= start_date & Date <= end_date)
ivory_weather <- filter(ivory_weather, Date >= start_date & Date <= end_date)
ghana_curr <- filter(ghana_curr, Date >= start_date & Date <= end_date)
ivory_curr <- filter(ivory_curr, Date >= start_date & Date <= end_date)
cocoa_data <- filter(cocoa_data, Date >= start_date & Date <= end_date)
cameroon <- filter(cameroon, Date >= start_date & Date <= end_date)
nigeria <- filter(nigeria, Date >= start_date & Date <= end_date)
ecuador <- filter(ecuador, Date >= start_date & Date <= end_date)
# ------------------------------------------------------------- #

# Merge all datasets
merged_data <- cocoa_data %>%
  left_join(ghana_weather, by = "Date") %>%
  left_join(ivory_weather, by = "Date") %>%
  left_join(ghana_curr, by = "Date") %>%
  left_join(ivory_curr, by = "Date") %>%
  left_join(cameroon, by = "Date") %>%
  left_join(nigeria, by = "Date") %>%
  left_join(ecuador, by = "Date")

# Rename currency columns
colnames(merged_data)[colnames(merged_data) == "Mid Rate"] <- "GHS"
colnames(merged_data)[colnames(merged_data) == "Price"] <- "XOF"

# ------------------------------------------------------------- #
# Create dummy variables
merged_data$Covid_Lag <- ifelse(merged_data$Date >= as.Date("2020-03-01") & 
                                  merged_data$Date <= as.Date("2022-12-31"), 1, 0)
merged_data$Post_2023 <- ifelse(merged_data$Date >= as.Date("2023-01-01"), 1, 0)
merged_data$Post_2023_Trend <- ifelse(merged_data$Date >= as.Date("2023-01-01"),
                                      as.numeric(merged_data$Date - as.Date("2023-01-01")),
                                      0)


# ------------------------------------------------------------- #
# Create time series and exogenous variables
merged_data <- na.omit(merged_data)
price_ts <- ts(merged_data$`ICCO daily price (US$/tonne)`, frequency = 365)

xreg_vars <- cbind(
  merged_data$TAVG,
  merged_data$IC_TAVG,
  merged_data$TAVG_EC,
  merged_data$TAVG_NI,
  merged_data$TAVG_CAM,
  merged_data$GHS,
  merged_data$XOF,
  merged_data$Covid_Lag,
  merged_data$Post_2023,
  merged_data$Post_2023_Trend
)

colnames(xreg_vars) <- c("Ghana_TAVG", "Ivory_TAVG", "Ecuador_TAVG", "Nigeria_TAVG", "Cameroon_TAVG",
                         "GHS", "XOF", "Covid_Lag", "Post_2023", "Post_2023_Trend")

# ------------------------------------------------------------- #
# Fit SARIMAX models
model_212 <- Arima(price_ts, order = c(2,1,2), seasonal = list(order = c(1,0,1), period = 7), xreg = xreg_vars)
model_313 <- Arima(price_ts, order = c(3,1,3), seasonal = list(order = c(1,0,1), period = 7), xreg = xreg_vars)
model_414 <- Arima(price_ts, order = c(4,1,4), seasonal = list(order = c(1,0,1), period = 7), xreg = xreg_vars)
model_513 <- Arima(price_ts, order = c(5,1,3), seasonal = list(order = c(1,0,1), period = 7), xreg = xreg_vars)

# Compare models
comparison <- data.frame(
  Model = c("SARIMAX(2,1,2)", "SARIMAX(3,1,3)", "SARIMAX(4,1,4)", "SARIMAX(5,1,3)"),
  AIC = c(AIC(model_212), AIC(model_313), AIC(model_414), AIC(model_513)),
  BIC = c(BIC(model_212), BIC(model_313), BIC(model_414), BIC(model_513))
)
print(comparison)

# ------------------------------------------------------------- #
# ------------------------------------------------------------- #
# Choose best model (example: SARIMAX(4,1,4), update if different)
best_model <- model_513

# Create future exogenous variables for 120 days
last_date <- max(merged_data$Date)
future_dates <- seq(from = last_date + 1, by = "day", length.out = 120)

future_xreg <- as.matrix(data.frame(
  Ghana_TAVG = mean(merged_data$TAVG, na.rm = TRUE),
  Ivory_TAVG = mean(merged_data$IC_TAVG, na.rm = TRUE),
  Ecuador_TAVG = mean(merged_data$TAVG_EC, na.rm = TRUE),
  Nigeria_TAVG = mean(merged_data$TAVG_NI, na.rm = TRUE),
  Cameroon_TAVG = mean(merged_data$TAVG_CAM, na.rm = TRUE),
  GHS = mean(merged_data$GHS, na.rm = TRUE),
  XOF = mean(merged_data$XOF, na.rm = TRUE),
  Covid_Lag = 0,
  Post_2023 = 1,
  Post_2023_Trend = as.numeric(future_dates - as.Date("2023-01-01"))
))


# Forecast 120 days ahead
forecast_120 <- forecast(best_model, xreg = future_xreg, h = 120)

#----------------------------
# Rebuild plot_df for 120-day forecast
full_price <- c(as.numeric(price_ts), as.numeric(forecast_120$mean))
upper_95 <- c(rep(NA, length(price_ts)), forecast_120$upper[,2])
lower_95 <- c(rep(NA, length(price_ts)), forecast_120$lower[,2])
all_dates <- c(merged_data$Date, future_dates)

plot_df <- data.frame(
  Date = all_dates,
  Price = full_price,
  Upper = upper_95,
  Lower = lower_95,
  Type = c(rep("Historical", length(price_ts)), rep("Forecast", 120))
)

# Plot using ggplot with dates and CI
ggplot(plot_df, aes(x = Date)) +
  geom_line(aes(y = Price, color = Type), linewidth = 1) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
  labs(title = "Cocoa Prices with 120-Day Forecast",
       x = "Date", y = "Price (USD/tonne)") +
  scale_color_manual(values = c("Historical" = "brown", "Forecast" = "blue")) +
  theme_minimal()


#---------------------------
# Metrics:

# Reload the full cocoa data (with extended date range)
full_cocoa_data <- read_csv(file_path)
full_cocoa_data$Date <- as.Date(full_cocoa_data$Date, format = "%d/%m/%Y")

# Extract actual 120 prices after the last date in merged_data
real_prices_120 <- full_cocoa_data %>%
  filter(Date > last_date & Date <= last_date + 120) %>%
  pull(`ICCO daily price (US$/tonne)`)

# Compare forecast to actual values
predicted <- as.numeric(forecast_120$mean)
actual <- real_prices_120  # make sure this is a numeric vector of length 120

# Calculate metrics
rmse <- sqrt(mean((actual - predicted)^2))
mae <- mean(abs(actual - predicted))
mape <- mean(abs((actual - predicted) / actual)) * 100

# Print results
cat("RMSE:", round(rmse, 2), "\n")
cat("MAE:", round(mae, 2), "\n")
cat("MAPE:", round(mape, 2), "%\n")


#------------------------------------------#
#FIGURES:

# === (1) Model Training and Validation ===
# Comparison table of all trained models
print(comparison)  # Already contains Model, AIC, and BIC


# === (2) Performance Evaluation Using Metrics ===



# Create metrics table
metrics_df <- data.frame(
  Metric = c("RMSE", "MAE", "MAPE (%)"),
  Value = round(c(rmse, mae, mape), 2)
)

# Print in console
print(metrics_df)

# Save metrics table as PNG to Downloads
library(gridExtra)
png("~/Downloads/forecast_metrics_table.png", width = 600, height = 300, res = 150)
grid.table(metrics_df)
dev.off()



# === Forecast vs Actual Only (no historical) ===

# Trim forecast and dates to match actual
n_actual <- length(actual)
forecast_only_df <- data.frame(
  Date = future_dates[1:n_actual],
  Forecast = as.numeric(forecast_120$mean[1:n_actual]),
  Actual = actual[1:n_actual],
  Lower = forecast_120$lower[1:n_actual, 2],
  Upper = forecast_120$upper[1:n_actual, 2]
)

# Plot
png("~/Downloads/forecast_vs_actual_120days.png", width = 1200, height = 800)

ggplot(forecast_only_df, aes(x = Date)) +
  geom_line(aes(y = Forecast), color = "blue", linewidth = 1) +
  geom_line(aes(y = Actual), color = "red", linetype = "dashed", linewidth = 1) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
  labs(
    title = "Forecast (blue line) vs Actual Cocoa Prices (red dotted-line) (120-Day Horizon)",
    subtitle = "Blue = Forecast | Red dashed = Actual",
    x = "Date (2024-2025)", y = "Price (USD/tonne)"
  ) +
  theme_minimal(base_size = 16)

dev.off()


# Load required libraries
library(corrplot)
library(ggplot2)

# Create correlation matrix
weather_corr <- cor(merged_data[, c("TAVG", "IC_TAVG", "TAVG_NI", "TAVG_CAM", "TAVG_EC")], use = "complete.obs")

# Save correlation matrix as PNG (mac Downloads folder)
png("~/Downloads/weather_correlation_matrix_full.png", width = 1000, height = 800, res = 150)
corrplot(weather_corr, method = "color", type = "upper", 
         tl.col = "black", tl.cex = 1.2, number.cex = 1.1, 
         addCoef.col = "black", diag = FALSE, title = "Correlation Matrix: Average Temperature by Country",
         mar = c(0,0,2,0))
dev.off()

