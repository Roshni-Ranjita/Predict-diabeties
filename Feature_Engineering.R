# Load the necessary library
library(dplyr)
library(fastDummies)
library(reshape2)
library(ggplot2)

# Read the CSV file
df <- read.csv('Data/Raw_Data/diabetes_data_file.csv')

# Display the first few rows of the data frame
head(df)

# Creating BMI categories
df <- df %>%
  mutate(BMI_Category = case_when(
    BMI < 18.5 ~ "Underweight",
    BMI >= 18.5 & BMI < 24.9 ~ "Normal",
    BMI >= 25 & BMI < 29.9 ~ "Overweight",
      TRUE ~ "Obese"
  ))

# Age bins
df <- df %>%
  mutate(Age_Bin = cut(Age, breaks = c(0, 18, 35, 55, 100), labels = c("Child", "Young Adult", "Middle Aged", "Elderly")))



# Hypertension and BMI Interaction
df<- df%>%
  mutate(Hypertension_BMI_interaction = Hypertension * BMI)


# Cholesterol to HDL Ratio
df<- df%>%
  mutate(
    Cholesterol_HDL_ratio = ifelse(
      CholesterolHDL > 0, CholesterolTotal / CholesterolHDL, NA
    )
  )

# Blood Pressure Variance (Pulse Pressure)
df<- df%>%
  mutate(Pulse_Pressure = SystolicBP - DiastolicBP)



# Relation with diabeties history
df <- df %>%
  mutate(Diabetic_Encounter = FamilyHistoryDiabetes + PreviousPreDiabetes)


# Other Disease
df <- df %>%
  mutate(Other_Disease = GestationalDiabetes + PolycysticOvarySyndrome + Hypertension)


# Medication
df <- df %>%
  mutate(Medication = AntihypertensiveMedications + Statins + AntidiabeticMedications)

# Other health issues
df <- df %>%
  mutate(Other_health_issues = FrequentUrination + ExcessiveThirst + UnexplainedWeightLoss + BlurredVision + SlowHealingSores + TinglingHandsFeet)

# Hazardous Exposure
df <- df %>%
  mutate(Hazardous_Exposure = HeavyMetalsExposure + OccupationalExposureChemicals + WaterQuality)


# Create one-hot encoded columns for Age_Bin
df <- dummy_cols(df, select_columns = "Age_Bin")
df <- dummy_cols(df, select_columns = "BMI_Category")


cols <- c("Diagnosis", "BMI_Category", "Age_Bin", "Hypertension_BMI_interaction", 
          "Cholesterol_HDL_ratio", "Pulse_Pressure", "Diabetic_Encounter", 
          "Other_Disease", "Medication", "Other_health_issues", 
          "BMI_Category_Normal", "BMI_Category_Obese", 
          "BMI_Category_Overweight", "BMI_Category_Underweight")

df_selected <- subset(df, select = cols)

# Replace NA values with 0
df_selected[is.na(df_selected)] <- 0

# Identify constant columns with zero standard deviation
constant_cols <- sapply(df_selected, function(x) {
  if (is.numeric(x)) {
    sd(x, na.rm = TRUE) == 0
  } else {
    length(unique(x)) == 1  # Check if all values in non-numeric columns are identical
  }
})

# Remove constant columns
df_selected <- df_selected[, !constant_cols]

# Compute correlation matrix
numeric_cols <- sapply(df_selected, is.numeric)
df_numeric <- df_selected[, numeric_cols]

cor_matrix <- cor(df_numeric, use = "complete.obs")

# Extract correlations with "Diagnosis"
cor_diagnosis <- sort(abs(cor_matrix["Diagnosis", ]), decreasing = TRUE)

# Get the top 10 features correlated with "Diagnosis" (excluding itself)
top_features <- names(cor_diagnosis)[2:11] # Skip "Diagnosis" itself

# Subset the correlation matrix for these features
top_features <-c(top_features, "Diagnosis")
top_cor_matrix <- cor_matrix[top_features, top_features]

# Remove self-correlation
diag(top_cor_matrix) <- NA



melted_cor <- melt(top_cor_matrix, na.rm = TRUE)  # Exclude NA from the plot
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top 10 Correlated Features with Diagnosis (No Self-Correlation)", 
       x = "", 
       y = "", 
       fill = "Correlation")

write.csv(cor_matrix, "Data/Data_Reports/correlation_matrix_feature_engineering.csv", row.names = TRUE)

write.csv(df, "Data/Modeling_Data/Post_Feature_Engineering.csv", row.names = TRUE)