# ===============================================
# Boston Weather Seasonal Analysis
# Section 2: Season Model with Trend
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
# This is REQUIRED before running the season.model
BostonWeather$month <- factor(BostonWeather$month,
    levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
)

# Verify the factor levels are in correct order
levels(BostonWeather$month)
cat("\nMonth variable has been factored in calendar order.\n")

# ===============================================
# SECTION 2: SEASON MODEL (with Trend)
# ===============================================

# Run multiple regression: avg.temp ~ t + month
# This models BOTH trend and seasonal effects
# January will be the reference category (excluded dummy variable)
season.model <- lm(avg.temp ~ t + month, data = BostonWeather)

# Display full model summary
summary(season.model)

# ===============================================
# INTERPRETATION
# ===============================================

cat("\n===============================================\n")
cat("MODEL INTERPRETATION:\n")
cat("===============================================\n")
cat("- Intercept (b0): Average temp for January at t=0\n")
cat("- t coefficient (b1): Monthly trend (controlling for season)\n")
cat("- month coefficients: Difference from January (reference month)\n")
cat("  - Positive = warmer than January\n")
cat("  - Negative = colder than January\n")
cat("===============================================\n")

# Extract coefficients
coefs <- coef(season.model)
cat("\nTrend coefficient (b1):", coefs["t"], "\n")

# ===============================================
# PREDICTIONS
# ===============================================

# Example: Predict December 2021 (t = 96, month = Dec)
next_period <- data.frame(t = 96, month = factor("Dec", levels = levels(BostonWeather$month)))
prediction_dec <- predict(season.model, newdata = next_period, interval = "confidence")

cat("\nPrediction for December 2021 (t=96):\n")
print(prediction_dec)

# Example: Predict January 2022 (t = 97, month = Jan)
jan_2022 <- data.frame(t = 97, month = factor("Jan", levels = levels(BostonWeather$month)))
prediction_jan <- predict(season.model, newdata = jan_2022, interval = "confidence")

cat("\nPrediction for January 2022 (t=97):\n")
print(prediction_jan)

# ===============================================
# MODEL DIAGNOSTICS
# ===============================================

# Check R-squared improvement compared to trend-only model
cat("\nModel R-squared:", summary(season.model)$r.squared, "\n")
cat("Adjusted R-squared:", summary(season.model)$adj.r.squared, "\n")

# ===============================================
# OPTIONAL: VISUALIZATIONS
# ===============================================

# Plot 1: Actual vs Fitted values
plot(BostonWeather$avg.temp, season.model$fitted.values,
     main = "Actual vs Fitted Temperature",
     xlab = "Actual Temperature (°F)",
     ylab = "Fitted Temperature (°F)",
     pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2)  # 45-degree line

# Plot 2: Residuals over time
plot(BostonWeather$t, season.model$residuals,
     main = "Residuals Over Time",
     xlab = "Time Index (t)",
     ylab = "Residuals",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)

# Plot 3: Seasonal effects (coefficient plot for months)
month_coefs <- coefs[grep("month", names(coefs))]
month_names <- c("Feb", "Mar", "Apr", "May", "Jun", 
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
barplot(month_coefs, 
        main = "Seasonal Effects (vs. January)",
        xlab = "Month",
        ylab = "Temperature Difference (°F)",
        col = "lightblue",
        names.arg = month_names,
        las = 2)
abline(h = 0, col = "red", lwd = 2)

# Plot 4: Q-Q plot for normality check
qqnorm(season.model$residuals, main = "Q-Q Plot of Residuals")
qqline(season.model$residuals, col = "red", lwd = 2)

# ===============================================
# END OF SCRIPT
# ===============================================
