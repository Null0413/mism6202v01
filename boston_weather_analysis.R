# ===============================================
# Boston Weather Time Series Analysis
# Class: 11/18
# Student: Null0413
# ===============================================

# Load required package
# If mism6202v01 is not installed, uncomment and run:
# install.packages("mism6202v01")
library(mism6202v01)

# Load the BostonWeather data
data("BostonWeather")

# View the structure and first few rows
str(BostonWeather)
head(BostonWeather)

# ===============================================
# DATA PREPARATION
# ===============================================

# Factor the month variable in correct calendar order
BostonWeather$month <- factor(BostonWeather$month,
    levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
)

# Verify the factor levels are correct
levels(BostonWeather$month)

# ===============================================
# SECTION 1: TREND MODEL
# ===============================================

# Run linear regression: avg.temp ~ t (time index)
# This models the long-term trend in average temperature
trend.model <- lm(avg.temp ~ t, data = BostonWeather)

# Display model summary
summary(trend.model)

# The coefficient b1 represents the expected change in temperature
# from one month to the next (the trend)

# Make a prediction for next period (t = 96, December 2021)
next_period <- data.frame(t = 96)
prediction_trend <- predict(trend.model, newdata = next_period)
cat("\nPredicted temperature for t=96 (Dec 2021):", prediction_trend, "°F\n")

# ===============================================
# SECTION 2: SEASON MODEL
# ===============================================

# Run linear regression: avg.temp ~ month (seasonality)
# This models the seasonal patterns in temperature
season.model <- lm(avg.temp ~ month, data = BostonWeather)

# Display model summary
summary(season.model)

# ===============================================
# OPTIONAL: VISUALIZATIONS
# ===============================================

# Plot 1: Temperature over time with trend line
plot(BostonWeather$t, BostonWeather$avg.temp,
     main = "Boston Average Temperature Over Time",
     xlab = "Time Index (t)",
     ylab = "Average Temperature (°F)",
     pch = 19, col = "blue")
abline(trend.model, col = "red", lwd = 2)

# Plot 2: Temperature by month (seasonality)
boxplot(avg.temp ~ month, data = BostonWeather,
        main = "Boston Temperature by Month",
        xlab = "Month",
        ylab = "Average Temperature (°F)",
        col = "lightblue",
        las = 2)  # Rotate x-axis labels

# ===============================================
# END OF SCRIPT
# ===============================================
