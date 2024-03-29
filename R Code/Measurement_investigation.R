library(dplyr)
library(ggplot2)

df = read.csv('Measurement Investigation Data.csv')

head(df)

result <- df %>%
  group_by(partnum) %>%
  summarise(
    mean_y300 = mean(y300, na.rm = TRUE),  # Mean of y300, excluding NA
    min_y300 = min(y300, na.rm = TRUE),    # Minimum of y300, excluding NA
    max_y300 = max(y300, na.rm = TRUE),    # Maximum of y300, excluding NA
    sd_y300 = sd(y300, na.rm = TRUE)       # Standard deviation of y300, excluding NA
  ) %>%
  ungroup()  # Ensure the result is no longer grouped

# View the optimized result
print(result)

"""$$
\mathrm{sd}(measurement)=\sqrt{\frac{1.577685^2+1.128009^2+1.397718^2}{3}} = 1.38023064145454 $\approx 1.38
$$
"""

# Calculation of standard deviation measurement
# Using the sqrt and sum functions for clarity and efficiency
sd_measurement <- sqrt(sum(c(1.577685, 1.128009, 1.397718)^2) / 3)
sd_measurement  # Display the calculated standard deviation measurement

# Calculation of the overall mean of y300, excluding NA values
# Using the mean function with na.rm = TRUE to handle missing values
overall_mean_y300 <- mean(df$y300, na.rm = TRUE)
overall_mean_y300  # Display the overall mean of y300

# Plot for Standard Deviations
ggplot(result, aes(x = factor(partnum), y = sd_y300)) +
  geom_col(fill = "skyblue") +
  geom_hline(yintercept = sd_measurement, linetype = "dashed", color = "red") +
  labs(title = "Standard Deviation by Partnum and Overall",
       x = "Partnum",
       y = "Standard Deviation") +
  theme_minimal()

# Plot for Means
ggplot(result, aes(x = factor(partnum), y = mean_y300)) +
  geom_col(fill = "lightgreen") +
  geom_hline(yintercept = overall_mean_y300, linetype = "dashed", color = "red") +
  labs(title = "Mean by Partnum and Overall",
       x = "Partnum",
       y = "Mean") +
  theme_minimal()

# Calculate Mean, Min, Max, and SD By Shift
result_by_shift <- df %>%
  group_by(shift) %>%
  summarise(
    mean_y300 = mean(y300, na.rm = TRUE),
    min_y300 = min(y300, na.rm = TRUE),
    max_y300 = max(y300, na.rm = TRUE),
    sd_y300 = sd(y300, na.rm = TRUE)
  )

print("--------------------------")
# Calculate Mean, Min, Max, and SD By Day
result_by_day <- df %>%
  group_by(daycount) %>%
  summarise(
    mean_y300 = mean(y300, na.rm = TRUE),
    min_y300 = min(y300, na.rm = TRUE),
    max_y300 = max(y300, na.rm = TRUE),
    sd_y300 = sd(y300, na.rm = TRUE)
  )

  result_by_shift
  print("--------------------------")
 print("--------------------------")

  result_by_day

sd_shift = sqrt( (9.73861^2 + 10.25036^2 + 10.01450^2)/3)
sd_shift

sd_day = sqrt( (10.150245^2 + 9.996723^2 + 10.129880^2 + 	10.207689^2 + 9.897860^2) / 5)
sd_day

# Graphs for Shift

# Plot for Standard Deviations
ggplot(result_by_shift, aes(x = factor(shift), y = sd_y300)) +
  geom_col(fill = "skyblue") +
  geom_hline(yintercept = sd_shift, linetype = "dashed", color = "red") +
  labs(title = "Standard Deviation by Shift and Overall",
       x = "Shift",
       y = "Standard Deviation") +
  theme_minimal()

# Plot for Means
ggplot(result_by_shift, aes(x = factor(shift), y = mean_y300)) +
  geom_col(fill = "lightgreen") +
  geom_hline(yintercept = overall_mean_y300, linetype = "dashed", color = "red") +
  labs(title = "Mean by Shift and Overall",
       x = "Shift",
       y = "Mean") +
  theme_minimal()

# Graphs for Mean

# Plot for Standard Deviations
ggplot(result_by_day, aes(x = factor(daycount), y = sd_y300)) +
  geom_col(fill = "skyblue") +
  geom_hline(yintercept = sd_day, linetype = "dashed", color = "red") +
  labs(title = "Standard Deviation by Day and Overall",
       x = "Day",
       y = "Standard Deviation") +
  theme_minimal()

# Plot for Means
ggplot(result_by_day, aes(x = factor(daycount), y = mean_y300)) +
  geom_col(fill = "lightgreen") +
  geom_hline(yintercept = overall_mean_y300, linetype = "dashed", color = "red") +
  labs(title = "Mean by Day and Overall",
       x = "Day",
       y = "Mean") +
  theme_minimal()

# Create a histogram of the 'y300' column
  hist(df$y300, main="Histogram of y300 Values", xlab="y300 Values", ylab="Frequency", col="lightgreen", border="black")

# Assuming df is your data frame and y300 is the column of interest
y <- df$y300

# Step 1: Calculate mean and standard deviation
mean_y <- mean(y)
sd_y <- sd(y)

# Step 2: Calculate residuals
residuals <- y - mean_y

# Step 3 and 4: Calculate pseudo-studentized residuals
studentized_residuals <- sapply(1:length(y), function(i) {
  y_minus_i <- y[-i]
  sqrt_loo_var <- sqrt(var(y_minus_i))
  (y[i] - mean(y_minus_i)) / sqrt_loo_var
})

# Step 5: Plot the pseudo-studentized residuals
plot(studentized_residuals,
     ylab = "Pseudo-Studentized Residuals",
     xlab = "Observation Index",
     main = "Studentized Residual Plot of y300",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lty = 2)

# Basic Summary Statistics for the 'y300' column
summary_data <- summary(df$y300)

# Specifications
USL <- 60  # Upper Specification Limit
LSL <- 40  # Lower Specification Limit

# Ppk Calculation for 'y300'
mean_y300 <- mean(df$y300)
sd_y300 <- sd(df$y300)
Ppk <- min((USL - mean_y300) / (3 * sd_y300), (mean_y300 - LSL) / (3 * sd_y300))

# Combine Summary Data with Ppk for 'y300'
summary_list <- c(summary_data, Ppk=Ppk)

# Print the combined summary
print(summary_list)sn

df %>%
  mutate(part = factor(partnum), factor(shift)) %>%
  group_by(part) %>%
  summarise(
    mean_partnum = mean(y300),
    min_partnum = min(y300),
    max_partnum = max(y300),
    sd_partnum = sd(y300)
  )

overall_sd = sqrt( ( 1.2316840^2 + 0.9070431^2 + 1.1213324^2 )/3 )
overall_sd

df %>% mutate(partnum = factor(partnum)) -> df
lm(data = df, y300 ~ partnum) -> model_part
model_part %>% anova
sigma(model_part)

sqrt(251.4649 / 132) # Estimate of the Root Mean Square

# From First Analaysis : sd(total) = 4.8054743

sd_total = 4.8054743
sd_process = sqrt(sd_total^2 - sd_measurement^2)
sd_process

D = sd_process / sd_measurement
D

df %>% ggplot(aes(x = partnum, y = y300))+geom_point()

# Step 1: Calculate the mean of y300 for each partnum and create a new column with the subtracted value
df <- df %>%
  group_by(partnum) %>%
  mutate(y_mean = mean(y300), y_dif = y300 - y_mean) %>%
  ungroup()  # Ungroup to avoid issues in further data manipulation

# Step 2: Plot using the adjusted y-values
ggplot(df, aes(x = partnum, y = y_dif)) + geom_point()

df %>% ggplot(aes(x = shift, y = y300))+geom_point()

# Step 1: Calculate the mean of y300 for each partnum and create a new column with the subtracted value
df <- df %>%
  group_by(shift) %>%
  mutate(y_mean = mean(y300), y_dif = y300 - y_mean) %>%
  ungroup()  # Ungroup to avoid issues in further data manipulation

# Step 2: Plot using the adjusted y-values
ggplot(df, aes(x = shift, y = y_dif)) + geom_point()

df %>% ggplot(aes(x = daycount, y = y300))+geom_point()

# Step 1: Calculate the mean of y300 for each partnum and create a new column with the subtracted value
df <- df %>%
  group_by(daycount) %>%
  mutate(y_mean = mean(y300), y_dif = y300 - y_mean) %>%
  ungroup()  # Ungroup to avoid issues in further data manipulation

# Step 2: Plot using the adjusted y-values
ggplot(df, aes(x = daycount, y = y_dif)) + geom_point()

