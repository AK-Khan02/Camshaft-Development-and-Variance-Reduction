import matplotlib.pyplot as plt
import pandas as pd

data = pd.read_csv('Data Combined.csv')
data

# Create the figure and axis objects
fig, ax = plt.subplots(figsize=(8, 6))

# Plot the general scatter points
ax.scatter(data['part'], data['y100'], s=50, alpha=0.6, edgecolor='k', color='skyblue', label='General y100 values')

# Identify and plot the special y100 values with a different symbol
special_values = [9.6, -12.4]
for value in special_values:
    special_data = data[data['y100'] == value]
    ax.scatter(special_data['part'], special_data['y100'], s=100, marker='*', color='red', label=f'Special value: {value}')

# Set the title and labels
ax.set_title('Stripchart of y100 values by Part', fontsize=15)
ax.set_xlabel('Part', fontsize=12)
ax.set_ylabel('y100', fontsize=12)

# Improve the look of the x-axis
ax.set_xticks(data['part'].unique())
ax.set_xticklabels(data['part'].unique(), rotation=45)

# Add a legend to differentiate the special points
ax.legend()

# Add grid for better readability
ax.grid(True, which='both', linestyle='--', linewidth=0.5)

# Show the plot
plt.tight_layout()
plt.show()

