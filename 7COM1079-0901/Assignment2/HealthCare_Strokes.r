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

# Convert the static ggplot to an interactive plot using Plotly
interactive_plot <- ggplotly(p)

# Display the interactive plot
interactive_plot

# Chi-square test for hypertension and stroke
# Create a contingency table
table_hyp <- table(stroke_data$hypertension, stroke_data$stroke)

# Perform Chi-square test
chi_square_result <- chisq.test(table_hyp)
print(chi_square_result)

# T-test for average glucose levels by stroke occurrence
# Compare means between stroke (1) and non-stroke (0) groups
t_test_result <- t.test(avg_glucose_level ~ stroke, data = stroke_data)
print(t_test_result)
