# Loading the necessary library
library(ggplot2)

# Importing Datasets
combined_data = read.csv('Data Combined.csv')

(combined_data)

# Fit the linear model
model <- lm(y100 ~ part, data = combined_data)

# Display the summary of the model
summary(model)

residuals <- residuals(model)
std_dev_residuals <- sd(residuals)

# Print the standard deviation of the residuals
print(std_dev_residuals)

# Convert 'part' to a factor
combined_data$part <- as.factor(combined_data$part)

# Create the linear model with an interaction term between 'part' and 'y100'
model <- lm(y100 ~ part + part:y100, data = combined_data)

# Check the summary of the model to see results
summary(model)

combined_data

# Convert 'part' to a factor
combined_data$part <- as.factor(combined_data$part)

# Fit the linear model
model <- lm(y100 ~ part, data = combined_data)

# Summary of the model
summary(model)

# Perform ANOVA
anova(model)

# Diagnostic plots (optional but recommended)
par(mfrow = c(2, 2))
plot(model)

sds <- aggregate(y100 ~ part, data = combined_data, FUN = sd)

print(sds)

# Calculating SD of partnum 27897, with the removal of that outlier

# Assuming 'data' is your dataframe

# Filter out the specific outlier value for part 27897
data_filtered <- combined_data[!(combined_data$part == "27897" & combined_data$y100 == -12.4), ]

# Calculate the new standard deviation for part 27897 without the outlier
new_sd_27897 <- sd(data_filtered$y100[data_filtered$part == "27897"])

# Display the new standard deviation
new_sd_27897

# Calculating Overall Standard Deviation
overall_sd_y100 = sd(combined_data$y100)
overall_sd_y100

filtered_sd_y100 = sd(data_filtered$y100)
filtered_sd_y100

