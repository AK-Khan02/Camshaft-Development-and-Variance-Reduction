# Variance Transmission Investigation
library(ggplot2)

df = read.csv('Search For Cause 1.csv')

# View of Data
head(df)

# Create the first plot
plot1 <- ggplot(df, aes(x=y100, y=y200)) +
  geom_point() +
  geom_smooth(method='lm', color='blue', fill='grey') +
  labs(title='Variation of y100 vs. y200') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create the second plot
plot2 <- ggplot(df, aes(x=y200, y=y300)) +
  geom_point() +
  geom_smooth(method='lm', color='blue', fill='grey') +
  labs(title='Variation of y200 vs. y300') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create the third plot
plot3 <- ggplot(df, aes(x=y100, y=y300)) +
  geom_point() +
  geom_smooth(method='lm', color='blue', fill='grey') +
  labs(title='Variation of y100 vs. y300') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Output the plots
print(plot1)
print(plot2)

# Linear regression analysis comparing y100 and y200 variables
model <- lm(y200 ~ y100, data = df)

# Calculate standard error of the model's residuals
model_se <- sigma(model)

# Calculate standard deviation of the y200 variable
y200_sd <- sd(df$y200)

# Compute and report the ratio of the corrected standard deviation of y200 to its original standard deviation
# This ratio adjusts for the model's error and provides insight into the variance explained by the model
ratio <- sqrt((y200_sd^2 - model_se^2) / y200_sd^2)
# Include the calculated ratio in the report

# Perform ANOVA to assess the significance of the model
anova_results <- anova(model)
# Review the ANOVA table to understand the model's explanatory power and variable significance


print(paste("Sigma: ", round(model_se, digits = 4)))
print(paste("SD y300: ", round(y200_sd, digits = 4)))
print(paste("Ratio: ", round(ratio, digits = 4)))
summary(model)
anova_results

# Load necessary libraries
library(stats)

# Fit linear regression model with y300 as the dependent variable and y200 as the independent variable
model <- lm(y300 ~ y200, data = df)

# Calculate the residual standard error of the model
residual_SE <- sigma(model)

# Calculate the standard deviation of the dependent variable y300
y300_SD <- sd(df$y300)

# Compute the ratio of the square root of the difference between the variance of y300 and the squared residual standard error,
# divided by the standard deviation of y300. This ratio may provide insights into the model's explanatory power beyond the residual error.
ratio <- sqrt(var(df$y300) - residual_SE^2) / y300_SD


# Generate an ANOVA table for the linear regression model to assess the significance of the independent variables
model_anova <- anova(model)

print(paste("Sigma: ", round(residual_SE, digits = 4)))
print(paste("SD y300: ", round(y300_SD, digits = 4)))
print(paste("Ratio: ", round(ratio, digits = 4)))
print(model_anova)
summary(model)

