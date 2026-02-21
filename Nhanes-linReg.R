# Load libraries
# Run this once to install everything needed
install.packages(c("tableone", "corrplot", "ggfortify", "patchwork"))

library(tidyverse)  # For data manipulation and visualization
library(tableone)   # For medical-style summary tables
library(corrplot)   # For correlation heatmaps
library(ggfortify)  # For PCA visualization
library(patchwork)  # For combining plots

# Load datasets
demo <- read_csv("demographics.csv")
exam <- read_csv("examination.csv")
lab  <- read_csv("laboratory.csv")
diet <- read_csv("dietary.csv")
ques <- read_csv("questionnaire.csv")

nhanes <- demo |>
  left_join(exam, by = "SEQN") |>
  left_join(lab,  by = "SEQN") |>
  left_join(diet, by = "SEQN") |>
  left_join(ques, by = "SEQN")

head(nhanes)
head(exam)
head(diet)
#quick check for BMI nulls
summary(nhanes$BMXBMI)
summary(is.na(nhanes$BMXBMI))

# Corrected Data Cleaning Block
nhanes_clean <- nhanes |>
  select(
    SEQN, 
    Age = RIDAGEYR,       # Corrected variable for Age
    Sex_Code = RIAGENDR, 
    PIR = INDFMPIR,       # SES
    BMI = BMXBMI, 
    Cholesterol = LBXTC, 
    Glucose = LBXGLU, 
    Protein_Intake = DR1TPROT, 
    Sugar_Intake = DR1TSUGR, 
    Diabetes_Code = DIQ010
  ) |>
  # 1. Quality Control: Filter out outliers
  filter(BMI > 10 & BMI < 80) |> 
  # 2. Variable Recoding
  mutate(
    Sex = factor(Sex_Code, levels = c(1, 2), labels = c("Male", "Female")),
    Age_Group = if_else(Age < 18, "Pediatric", "Adult"),
    Diabetes = factor(Diabetes_Code, levels = c(1, 2), labels = c("Yes", "No")),
    
    BMI_Category = case_when(
      BMI < 18.5 ~ "Underweight",
      BMI < 25   ~ "Normal",
      BMI < 30   ~ "Overweight",
      TRUE       ~ "Obese"
    ),
    
    SES = case_when(
      PIR < 1.3  ~ "Low",
      PIR < 3.5  ~ "Middle",
      TRUE       ~ "High"
    )
  ) |>
  # 3. Handle Missing Data
  drop_na(BMI, Cholesterol, Protein_Intake)

# Verify the result
summary(nhanes_clean$Age)

# Variables we want to summarize
vars_to_summary <- c("Age", "BMI", "Cholesterol", "Protein_Intake", "Sugar_Intake", "SES")

# Create the table stratified by Age Group and Sex
tab1 <- CreateTableOne(
  vars = vars_to_summary, 
  strata = c("Age_Group", "Sex"), 
  data = nhanes_clean, 
  factorVars = "SES"
)

# Print it
print(tab1)




# Linear Regression: Does Protein and BMI affect Cholesterol?
model1 <- lm(Cholesterol ~ Protein_Intake + Age + Sex + BMI, data = nhanes_clean)

summary(model1)


# Interaction Model: Does the effect of Sugar on Glucose change for Adults vs Kids?
model2 <- lm(Glucose ~ Sugar_Intake * Age_Group, data = nhanes_clean)


summary(model2)






# Visualization 1: BMI Distribution
p1 <- ggplot(nhanes_clean, aes(x = BMI, fill = Age_Group)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Adult" = "#2c3e50", "Pediatric" = "#e74c3c")) +
  theme_minimal() +
  labs(title = "BMI Distribution by Age Group")

# Visualization 2: Cholesterol vs Nutrition
p2 <- ggplot(nhanes_clean, aes(x = Protein_Intake, y = Cholesterol, color = Sex)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Cholesterol vs. Protein Intake")

# Combine them using the 'patchwork' library
p1 / p2


# 1. Correlation Heatmap for Clinical Metrics
library(corrplot)
clinical_corr <- nhanes_clean |> 
  select(Age, BMI, Cholesterol, Glucose, Protein_Intake, Sugar_Intake) |> 
  cor(use = "complete.obs")

corrplot(clinical_corr, method = "ellipse", type = "upper", 
         tl.col = "black", title = "Clinical Variable Correlations", mar=c(0,0,1,0))

# 2. Glucose Levels by Diabetes Status (Boxplot)
p3 <- ggplot(nhanes_clean, aes(x = Diabetes, y = Glucose, fill = Diabetes)) +
  geom_boxplot(notch = TRUE) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(title = "Glucose Metric by Diagnosis", y = "Plasma Glucose (mg/dL)")

# 3. SES vs BMI Category (Stacked Bar Chart)
p4 <- ggplot(nhanes_clean, aes(x = SES, fill = BMI_Category)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "BMI Distribution across SES", y = "Proportion")

# 4. Faceted Distribution of Cholesterol by Sex and Age Group
p5 <- ggplot(nhanes_clean, aes(x = Cholesterol)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_grid(Sex ~ Age_Group) +
  theme_light() +
  labs(title = "Cholesterol Distribution Facets")

# Combine for a Skill-heavy Dashboard
(p3 + p4) / p5 / (p1 + p2)

p6
corrplot(clinical_corr, method = "shade", type = c("full"), 
         tl.col = "black", title = "Clinical Variable Correlations", mar=c(1,1,1,1))








