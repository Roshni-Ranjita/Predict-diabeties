# Load the necessary library
library(dplyr)
library(pheatmap)
library(ggplot2)

# Read the CSV file
df <- read.csv('Data/Raw_Data/diabetes_data_file.csv')

# Display the first few rows of the data frame
head(df)
# Generate summary statistics only for numeric columns
summary_df <- df %>%
summarise(across(where(is.numeric), 
                 list(Mean = ~mean(.x, na.rm = TRUE),
                      Median = ~median(.x, na.rm = TRUE),
                      SD = ~sd(.x, na.rm = TRUE),
                      Min = ~min(.x, na.rm = TRUE),
                      Max = ~max(.x, na.rm = TRUE),
                      N = ~sum(!is.na(.x)))))

library(tidyr)

summary_long <- summary_df %>%
  pivot_longer(everything(),
               names_to = c("Variable", "Statistic"),
               names_sep = "_",
               values_to = "Value")

write.csv(as.data.frame(summary_long), 'Data/Data_Reports/Data_summary_long.csv')
          
# View the summary data frame
print(summary_df)

# Transpose the data frame to have statistics in separate columns
summary_df <- t(summary_df)
summary_df <- as.data.frame(summary_df)


# Write the summary statistics to a CSV file
write.csv(as.data.frame(summary_df), 'Data/Data_Reports/Data_summary.csv')

# ----------
#EDA


# Calculate the correlation matrix
numeric_df <- select_if(df, is.numeric)
correlation_matrix <- cor(numeric_df)

# Print the correlation matrix
print(correlation_matrix)
write.csv(as.data.frame(correlation_matrix), 'Data/Data_Reports/Data_correlation.csv')


# Extract correlations with Diagnosis
diagnosis_correlations <- correlation_matrix["Diagnosis", ]

# Sort by absolute values of correlations (excluding the Diagnosis itself)
sorted_correlations <- sort(abs(diagnosis_correlations), decreasing = TRUE)

# Get the top 10 correlations (excluding Diagnosis itself)
top10_vars <- c("Diagnosis", names(sorted_correlations)[2:11])  # Skipping the first element if it's the self-correlation
top10_correlations <- correlation_matrix[top10_vars, top10_vars]
diag(top10_correlations) <- NA



# Plot the heatmap of the top 10 correlations
pheatmap(top10_correlations, 
         cluster_rows = TRUE, 
         cluster_cols = TRUE, 
         display_numbers = TRUE, 
         main = "Top 10 Correlations with Diagnosis")

# Bar plot for SystolicBP in Diabetic individuals
ggplot(df, aes(x = factor(Diagnosis))) + 
  geom_bar(fill = "lightcoral", color = "black") +
  labs(title = "Diabetic Individuals", x = "Diagnosis", y = "Count")

# Histogram for fasting blood
hist(df$FastingBloodSugar, 
     main = "Histogram of Fasting Blood Sugar Levels", 
     xlab = "Fasting Blood Sugar", 
     col = "lightblue", 
     border = "black")

# Histogram for HbA1c
hist(df$HbA1c, 
     main = "Histogram of HbA1c Levels", 
     xlab = "HbA1c", 
     col = "lightgreen", 
     border = "black")

# Histogram for SystolicBP
hist(df$SystolicBP, 
     main = "Histogram of SystolicBP Levels", 
     xlab = "SystolicBP", 
     col = "lightblue", 
     border = "black")





# Bar plot for FrequentUrination vs Diabetes
ggplot(df, aes(x = factor(FrequentUrination), fill = factor(Diagnosis))) + 
  geom_bar(position = "dodge") +
  labs(title = "Frequent Urination vs Diabetes", x = "Frequent Urination", y = "Count", fill = "Diabetes") +
  scale_fill_manual(values = c("lightblue", "darkblue"), labels = c("No Diabetes", "Diabetes"))


# Bar plot for Hypertension vs Diabetes
ggplot(df, aes(x = factor(Hypertension), fill = factor(Diagnosis))) + 
  geom_bar(position = "dodge") +
  labs(title = "Hypertension vs Diabetes", x = "Hypertension", y = "Count", fill = "Diabetes") +
  scale_fill_manual(values = c("lightgreen", "darkgreen"), labels = c("No Diabetes", "Diabetes"))



# Bar plot for SystolicBP vs Diabetes
ggplot(df, aes(x = factor(SystolicBP), fill = factor(Diagnosis))) + 
  geom_bar(position = "dodge") +
  labs(title = "Systolic Blood Pressure vs Diabetes", x = "Systolic BP", y = "Count", fill = "Diabetes") +
  scale_fill_manual(values = c("lightcoral", "darkred"), labels = c("No Diabetes", "Diabetes"))


# Scatter plot between Fasting Blood Sugar and HbA1c, color by Diagnosis
ggplot(df, aes(x = FastingBloodSugar, y = HbA1c, color = factor(Diagnosis))) + 
  geom_point() +
  labs(title = "Fasting Blood Sugar Level vs HbA1c", x = "Fasting Blood Sugar Level", y = "HbA1c") +
  scale_color_manual(values = c("blue", "red"), labels = c("No Diagnosis", "Diagnosis")) +
  theme_minimal()

# Scatter plot between Fasting Blood Sugar and SystolicBP, color by Diagnosis
ggplot(df, aes(x = FastingBloodSugar, y = SystolicBP, color = factor(Diagnosis))) + 
  geom_point() +
  labs(title = "Fasting Blood Sugar Level vs Systolic BP", x = "Fasting Blood Sugar Level", y = "Systolic BP") +
  scale_color_manual(values = c("blue", "red"), labels = c("No Diagnosis", "Diagnosis")) +
  theme_minimal()


# Scatter plot between HbA1c and SystolicBP, color by Diagnosis
ggplot(df, aes(x = HbA1c, y = SystolicBP, color = factor(Diagnosis))) + 
  geom_point() +
  labs(title = "HbA1c vs Systolic BP", x = "HbA1c", y = "Systolic BP") +
  scale_color_manual(values = c("blue", "red"), labels = c("No Diagnosis", "Diagnosis")) +
  theme_minimal()


df$Diagnosis <- as.factor(df$Diagnosis)


# Histogram for Age, color-coded by Diabetic Status (stacked)
ggplot(df, aes(x = Age, fill = Diagnosis)) +
  geom_histogram(binwidth = 5, position = "stack", color = "black") +  # Stack bars on top of each other
  labs(title = "Age Distribution by Diabetic Status", x = "Age", y = "Frequency") +
  scale_fill_manual(values = c("lightblue", "pink")) +  # Custom colors for diabetic status
  theme_minimal()



# Box plot for Age by Gender, color-coded by Diabetic Status
ggplot(df, aes(x = Gender, y = Age, fill = Diagnosis, group = interaction(Gender, Diagnosis))) +
  geom_boxplot() +
  labs(title = "Age Distribution by Gender and Diabetic Status", x = "Gender", y = "Age") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal()



# Bar plot of Diabetic Status by Education Level
ggplot(df, aes(x = EducationLevel, fill = Diagnosis)) +
  geom_bar(position = "dodge", stat = "count") +  # Specify stat="count" for proper grouping
  labs(title = "Diabetic Status by Education Level", x = "Education Level", y = "Count") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal()



# Smoking and alcohol
# Box plot for Alcohol Consumption by Smoking status, color-coded by Diabetic Status
ggplot(df, aes(x = factor(Smoking), y = AlcoholConsumption, fill = Diagnosis)) +
  geom_boxplot() +  # Box plot to show distribution of alcohol consumption
  labs(title = "Alcohol Consumption by Smoking and Diabetic Status", 
       x = "Smoking Status (0 = Non-Smoker, 1 = Smoker)", 
       y = "Alcohol Consumption") +
  scale_fill_manual(values = c("lightblue", "pink")) +  # Custom colors for diabetic status
  theme_minimal()


# BMI physical activity
# Scatter plot for BMI vs PhysicalActivity, color-coded by Diabetic Status
ggplot(df, aes(x = BMI, y = PhysicalActivity, color = Diagnosis)) +
  geom_point(size = 3) +  # Scatter plot points
  labs(title = "BMI vs Physical Activity by Diabetic Status", 
       x = "BMI", 
       y = "Physical Activity") +
  scale_color_manual(values = c("blue", "red")) +  # Custom colors for diabetic status
  theme_minimal()  # Clean theme



# DietQuality SleepQuality
ggplot(df, aes(x = DietQuality, y = SleepQuality, color = Diagnosis)) +
  geom_point(size = 3) +  # Scatter plot points
  labs(title = "DietQuality vs SleepQuality by Diabetic Status", 
       x = "Diet Quality", 
       y = "Sleep Quality") +
  scale_color_manual(values = c("blue", "red")) +  # Custom colors for diabetic status
  theme_minimal()  # Clean theme



# BP SleepQuality
ggplot(df, aes(x = SystolicBP, y = DiastolicBP, color = Diagnosis)) +
  geom_point(size = 3) +  # Scatter plot points
  labs(title = "SystolicBP vs SleepQuality by Diabetic Status", 
       x = "SystolicBP", 
       y = "Sleep Quality") +
  scale_color_manual(values = c("blue", "red")) +  # Custom colors for diabetic status
  theme_minimal()  # Clean theme





