# Read the dataset 'bmi2_data.csv' into R
D <- read.table("bmi2_data.csv", header = TRUE, sep = ";")


# Add log-BMI to the dataset
D$logbmi <- log(D$bmi)


# Subset containing the first 840 observations (for model estimation)
D_model <- subset(D, id <= 840)

# Subset containing the last 7 observations (for validation)
D_test <- subset(D, id >= 841)


# Estimate multiple linear regression model
fit <- lm(logbmi ~ age + fastfood, data = D_model)

# Show parameter estimates etc.
summary(fit)


# Plots for model validation

# Observations against fitted values
plot(fit$fitted.values, D_model$logbmi, xlab = "Fitted values",     
       ylab = "log(BMI)")

# Residuals against each of the explanatory variables
plot(D_model$EXPLANATORY_VARIABLE, fit$residuals, 
        xlab = "INSERT TEXT", ylab = "Residuals")

# Residuals against fitted values
plot(fit$fitted.values, fit$residuals, xlab = "Fitted values", 
     ylab = "Residuals")

# Normal QQ-plot of the residuals
qqnorm(fit$residuals, ylab = "Residuals", xlab = "Z-scores", 
       main = "")
qqline(fit$residuals)


# Confidence intervals for the model coefficients
confint(fit, level = 0.95)


# Predictions and 95% prediction intervals
pred <- predict(FINAL_MODEL, newdata = D_test, 
                interval = "prediction", level = 0.95)

# Observed values and predictions
cbind(id = D_test$id, logbmi = D_test$logbmi, pred)

