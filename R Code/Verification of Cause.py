verification_data <- data.frame(
  daycount = c(30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30),
  shift = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3),
  partnum = c(42721, 42722, 42723, 42724, 42725, 42726, 42727, 42728, 42729, 42730, 42731, 42732),
  y100 = c(-7, 6.4, -9.6, 10.8, -5.8, 10, -3.2, 11.2, -6.8, 10, -4.8, 7.4),
  x12 = c(-2.4, 16.8, -2.4, 16.8, -2.4, 16.8, -2.4, 16.8, -2.4, 16.8, -2.4, 16.8)
)
head(verification_data)

# Create the strip chart
stripchart(x12 ~ y100, data = verification_data,
           main = "Strip Chart of y100 vs x12",
           xlab = "x12 values",
           ylab = "y100 values",
           method = "jitter",
           pch = 20, # Type of point. 20 is a solid circle.
           col = "blue") # Color of points

# Load ggplot2 library
library(ggplot2)

# Create the plot
p <- ggplot(verification_data, aes(x = factor(x12), y = y100)) +
  geom_jitter(aes(colour = factor(x12)), width = 0.1) +  # Add jitter to points for visibility
  scale_color_manual(values = c("red", "blue")) +        # Set custom colors
  labs(title = "Strip Chart of y100 vs x12",
       x = "x12 values",
       y = "y100 values",
       colour = "factor(x12)") +    # Label for the legend
  theme_minimal() +                 # Minimal theme for the plot
  theme(legend.position = "right")  # Position the legend to the right

# Print the plot
print(p)

model <- lm(y100 ~ 0 + x12, data = verification_data)
summary(model)



