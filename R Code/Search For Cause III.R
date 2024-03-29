library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)

data = read.csv('Baseline Investigation y100.csv')
head(data)

# Initialize vectors for top 8 highest values and their partnums
top_8_y100 <- rep(-Inf, 8)  # Use -Inf as a placeholder for initialization
top_8_partnum <- rep(NA, 8)

# Initialize vectors for bottom 8 lowest values and their partnums
bottom_8_y100 <- rep(Inf, 8)  # Use Inf as a placeholder for initialization
bottom_8_partnum <- rep(NA, 8)

# Loop through the dataset to find top 8 highest y100 values
for (i in 1:nrow(data)) {
  current_y100 <- data$y100[i]
  current_partnum <- data$partnum[i]

  # For top 8 highest values
  if (current_y100 > min(top_8_y100)) {
    min_index <- which.min(top_8_y100)  # Find the index of the minimum value
    top_8_y100[min_index] <- current_y100  # Replace the minimum value with the current value
    top_8_partnum[min_index] <- current_partnum  # Update the corresponding partnum
  }

  # For bottom 8 lowest values
  if (current_y100 < max(bottom_8_y100)) {
    max_index <- which.max(bottom_8_y100)  # Find the index of the maximum value
    bottom_8_y100[max_index] <- current_y100  # Replace the maximum value with the current value
    bottom_8_partnum[max_index] <- current_partnum  # Update the corresponding partnum
  }
}

# Combine the results into data frames for easier viewing
top_8_results <- data.frame(partnum = top_8_partnum, y100 = top_8_y100)
bottom_8_results <- data.frame(partnum = bottom_8_partnum, y100 = bottom_8_y100)

# Print the results
print("Top 8 highest y100 values and respective partnum:")
print(top_8_results)
print("############")
print("Bottom 8 lowest y100 values and respective partnum:")
print(bottom_8_results)

# Sort top_8_results from largest to lowest y100 values
top_8_sorted <- top_8_results %>%
  arrange(desc(partnum))

# Print the sorted top 8 results
print("Top 8 highest y100 values sorted:")
print(top_8_sorted)

# Sort bottom_8_results from largest to lowest y100 values
bottom_8_sorted <- bottom_8_results %>%
  arrange(desc(partnum))

# Print the sorted bottom 8 results
print("Bottom 8 lowest y100 values sorted (from largest to lowest):")
print(bottom_8_sorted)

# Define the dataframe
data_output <- data.frame(
  daycount = c(16, 17, 17, 17, 17, 17, 18, 18, 18, 18, 19, 19, 19, 20, 20, 20),
  shift = c(2, 1, 1, 2, 3, 3, 1, 1, 2, 2, 1, 3, 3, 1, 2, 2),
  partnum = c(22410, 23106, 23193, 23809, 24226, 24252, 24742, 24810, 25177, 25299, 26035, 27127, 27208, 27675, 27897, 28147),
  y100 = c(8.2, -8.6, -9, 7.8, 8.4, -8.2, 8, -8.6, 7.8, 8, -8.2, -9.2, 9.6, 8.2, -12.4, -10.6),
  x10 = c(1.8, 4.6, 1.6, 3.2, 1.5, 3.8, 3.3, 5.4, 5.6, 4.8, 0.2, 6.2, 4.1, 3.6, 4.2, 2),
  x11 = c(11.8, 0.5, 7.2, -0.4, 8.5, 3.1, 0.5, 0.5, 3.2, -1.2, 11, 4.1, -0.8, 7.9, 1.9, 5.4),
  x12 = c(16.8, 1.4, -1.7, 8.4, 13.2, 3.3, 14.5, -2.4, 15.5, 9.9, 1.8, 4.6, 14.4, 6.7, 0.4, -0.4)
)

# Display the first few rows of the dataframe
head(data_output)

# Categorize partnum into "Low" and "High"
data_output$category <- ifelse(data_output$partnum %in% c(27675, 27208, 25299, 25177, 24742, 24226, 23809, 22410), "High", "Low")


# Create the scatter plot
ggplot(data_output, aes(x = category, y = x10)) +
  geom_jitter(width = 0.2, size = 3, shape = 16, color = "red") + # geom_jitter to avoid overplotting
  labs(title = "Individual Value Plot of X10 vs Category", x = "Category", y = "X10 Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # If you have more categories and want to angle the x labels

# Create the scatter plot for x2 with blue triangles
ggplot(data_output, aes(x = category, y = x11)) +
  geom_jitter(width = 0.2, size = 3, shape = 17, color = "blue") + # Blue triangles
  labs(title = "Individual Value Plot of X11 vs Category", x = "Category", y = "X11 Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angled x labels

# Create the scatter plot for x3 with green diamonds
ggplot(data_output, aes(x = category, y = x12)) +
  geom_jitter(width = 0.2, size = 3, shape = 18, color = "green") + # Green diamonds
  labs(title = "Individual Value Plot of X12 vs Category", x = "Category", y = "X12 Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angled x labels

# Categorize partnum into "Low" and "High"
data_output$category <- ifelse(data_output$partnum %in% c(27675, 27208, 25299, 25177, 24742, 24226, 23809, 22410), "High", "Low")

# Reshape the data to long format
data_long <- gather(data_output, key = "variable", value = "value", x10, x11, x12)

# Create the scatter plot with different shapes and colors for x1, x2, and x3
ggplot(data_long, aes(x = category, y = value, shape = variable, color = variable)) +
  geom_jitter(width = 0.2, size = 3) +
  labs(title = "Individual Value Plot of X10, X11, X12 vs Category", x = "Category", y = "Value") +
  scale_shape_manual(values = c(16, 17, 18)) + # Different shapes for x1, x2, x3
  scale_color_manual(values = c("red", "blue", "green")) + # Different colors for x1, x2, x3
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(shape = guide_legend(title = "Variable"), color = guide_legend(title = "Variable"))

# Load necessary libraries
library(ggplot2)
library(tidyr)

# Categorize partnum into "Low" and "High"
data_output$category <- ifelse(data_output$partnum %in% c(27675, 27208, 25299, 25177, 24742, 24226, 23809, 22410), "High", "Low")

# Reshape the data to long format
data_long <- gather(data_output, key = "variable", value = "value", x10, x11, x12)

# Create the scatter plot with different filled shapes for x1, x2, and x3
ggplot(data_long, aes(x = category, y = value, shape = variable, color = variable, fill = variable)) +
  geom_point(size = 4, position = position_jitter(width = 0.15), stroke = 1.5) +
  scale_shape_manual(values = c(24, 21, 22)) + # Triangle, Circle, Square
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) + # Colors that are colorblind-friendly
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) + # Same colors for fill
  labs(title = "Value Distribution of X1, X2, X3 by Category",
       subtitle = "Distinguished by shape and color",
       x = "Category",
       y = "Value",
       caption = "Data Source: Company Database") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(face = "italic"),
    legend.position = "right",
    legend.title.align = 0.5,
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(hjust = 0, face = "italic"),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA)
  ) +
  guides(shape = guide_legend(title = "Variable"), color = guide_legend(title = "Variable"), fill = guide_legend(title = "Variable"))

# Print the plot
print(ggplot_object)

df = read.csv('Input_Output Investigation data.csv') # Input / Output Investigation Data
head(df)

model <- lm(y100 ~ x10 + x11 + x12 + x10*x11*x12 , data=df)
summary(model)

model <- lm(y100 ~ x10 + x11 + x12, data=df)
summary(model)

anova(model)

df %>% ggplot(aes(y = y100, x = `x10`))+geom_point()

df %>% ggplot(aes(y = y100, x = `x11`))+geom_point()

df %>% ggplot(aes(y = y100, x = `x12`))+geom_point()

summary(df$y100)

mean(df$y100)
sd(df$y100)

# From Baseline Investigation, SD = 4.28 Mean = -0.005

# if Res SD <<< Baseline SD : One or More Suspects is a dominant cause
