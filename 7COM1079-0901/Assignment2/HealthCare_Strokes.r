# Load necessary libraries
library(plotly) # For interactive plots
library(ggplot2) # For data visualization

# Load the dataset
stroke_data <- read.csv("C:\\Users\\QASIM\\Downloads\\healthcare-dataset-stroke-data.csv")

# Preview dataset structure
str(stroke_data) # To understand variable types and dataset dimensions

# Visualizing average glucose levels by stroke occurrence
# Create a static boxplot using ggplot2
p <- ggplot(stroke_data, aes(x = factor(stroke), y = avg_glucose_level, fill = factor(stroke))) + 
  geom_boxplot() +  # Adds the boxplot layer
  scale_fill_manual(values = c("lightblue", "pink")) +  # Customize boxplot colors
  labs(title = "Glucose Levels by Stroke Occurrence",
       x = "Stroke (0 = No, 1 = Yes)",
       y = "Average Glucose Levels (mg/dL)") +  # Add labels for title and axes
  theme_minimal() +  # Use a clean theme
  scale_x_discrete(labels = c("No Stroke", "Stroke"))  # Custom labels for stroke groups
