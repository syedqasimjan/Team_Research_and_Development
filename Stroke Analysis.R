
## Importing Libraries
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(pROC)
library(reshape2)
library(corrplot)


## Loading the Dataset
healthcare.dataset.stroke.data <- read.csv("C:/data/healthcare-dataset-stroke-data.csv")
View(healthcare.dataset.stroke.data)



## Data Exploration
str(healthcare_data)
summary(healthcare.dataset.stroke.data)

# Check for missing values
colSums(is.na(healthcare.dataset.stroke.data))


## Exploratory Data Analysis (EDA)


# Age Distribution
ggplot(healthcare.dataset.stroke.data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Frequency")

# Stroke Distribution by Gender
ggplot(healthcare.dataset.stroke.data, aes(x = gender, fill = as.factor(stroke))) +
  geom_bar(position = "dodge") +
  labs(title = "Stroke Distribution by Gender", x = "Gender", y = "Count", fill = "Stroke")

# Average Glucose Level by Stroke Status
ggplot(healthcare.dataset.stroke.data, aes(x = as.factor(stroke), y = avg_glucose_level, fill = as.factor(stroke))) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e")) +  
  labs(title = "Average Glucose Level by Stroke Status", 
       x = "Stroke", 
       y = "Average Glucose Level", 
       fill = "Stroke") +
  theme_minimal()


# Stroke Distribution by Smoking Status
ggplot(healthcare.dataset.stroke.data, aes(x = smoking_status, fill = as.factor(stroke))) +
  geom_bar(position = "stack") + 
  scale_fill_manual(values = c("0" = "#2ca02c", "1" = "#d62728")) +  
  labs(title = "Stroke Distribution by Smoking Status", 
       x = "Smoking Status", 
       y = "Count", 
       fill = "Stroke") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Retaining angled text for readability


#BMI vs Age Scatter Plot (Colored by Stroke)
set.seed(123)  
sampled_data <- healthcare.dataset.stroke.data[sample(nrow(healthcare.dataset.stroke.data), size = 500), ]  # Adjust sample size as needed

top_200_bmi_data <- healthcare.dataset.stroke.data %>%
  arrange(desc(bmi)) %>%
  head(200)

ggplot(top_200_bmi_data, aes(x = age, y = bmi, color = as.factor(stroke))) +
  geom_point(alpha = 0.7) +
  labs(title = "BMI vs Age (Top 200 BMI Values, Colored by Stroke)", 
       x = "Age", 
       y = "BMI", 
       color = "Stroke") +
  theme_minimal()


# Stroke Count by Work Type
work_type_stroke_count <- healthcare.dataset.stroke.data %>%
  group_by(work_type, stroke) %>%
  summarise(count = n()) %>%
  ungroup()
ggplot(work_type_stroke_count, aes(x = "", y = count, fill = as.factor(work_type))) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +  
  scale_fill_brewer(palette = "Set3") +  
  labs(title = "Stroke Count by Work Type", fill = "Work Type") +
  theme_void() + 
  theme(axis.text.x = element_blank())  


# Correlation Heatmap
numeric_vars <- healthcare.dataset.stroke.data %>% select(where(is.numeric))
correlation_matrix <- cor(numeric_vars, use = "complete.obs")
corrplot(correlation_matrix, method = "color", type = "upper", tl.cex = 0.8)



## Statistical Analysis
# Chi-Square Test between 'stroke' and 'work_type'
chi_test <- chisq.test(table(healthcare.dataset.stroke.data$stroke, healthcare.dataset.stroke.data$work_type))
chi_test


# T-Test to compare age between stroke and non-stroke patients
t_test_age <- t.test(age ~ stroke, data = healthcare.dataset.stroke.data)
t_test_age


## Predictive Modeling

# Random Forest Model 
rf_model <- randomForest(stroke ~ age + bmi + hypertension + heart_disease + avg_glucose_level + gender,
                         data = healthcare.dataset.stroke.data, 
                         importance = TRUE, 
                         ntree = 500)  
print(rf_model)

# ROC Curve for Random FOrest Model
predicted_probabilities_rf <- predict(rf_model, healthcare.dataset.stroke.data, type = "response")
roc_curve_rf <- roc(healthcare.dataset.stroke.data$stroke, predicted_probabilities_rf)

plot(roc_curve_rf, main = "Random Forest Model ROC Curve")

auc_rf <- auc(roc_curve_rf)
print(paste("AUC:", auc_rf))


# Logistics Regression
log_model <- glm(stroke ~ age + bmi + hypertension + heart_disease + avg_glucose_level + gender, 
                 data = healthcare.dataset.stroke.data, 
                 family = binomial)

summary(log_model)

predicted_probabilities_log <- predict(log_model, healthcare.dataset.stroke.data, type = "response")

# ROC curve for Logistics Regression
roc_curve_log <- roc(healthcare.dataset.stroke.data$stroke, predicted_probabilities_log)

plot(roc_curve_log, main = "ROC Curve for Logistic Regression Model", col = "blue", lwd = 2)
auc_log <- auc(roc_curve_log)
print(paste("AUC for Logistic Regression:", auc_log))






