rm(list = ls())

#Install packages
install.packages("tidyverse")

#Load libraries
library(ggplot2)
library(dplyr)

#read csv

heart <- read.csv("https://raw.githubusercontent.com/mohd-nor/R-PROJECT-MCSD-1113/main/Heart_Disease_Prediction.csv")
View(heart)

#check first rows
head(heart)

# Number of column
ncol(heart)

# Number of row
nrow(heart)

# DATA CLEANING

# Check null value
sum(is.na(heart))

heart

#Check data type for each column

# Display the structure of the dataset
str(heart)

# Transform categorical variable to R factors
heart$Sex <- as.factor(heart$Sex)
heart$Chest.pain.type <- as.factor(heart$Chest.pain.type)
heart$FBS.over.120<- as.factor(heart$FBS.over.120)
heart$Exercise.angina<- as.factor(heart$Exercise.angina)
heart$Thallium<- as.factor(heart$Thallium)

# Give a better name to the factor values for the graphs
levels(heart$Sex) <- c("Female", "Male")
levels(heart$Chest.pain.type) <- c("Typical angina", "Atypical angina", "No angina", "Asymptomatic")
levels(heart$FBS.over.120) <- c("False", "True")
levels(heart$Exercise.angina) <- c("No", "Yes")
levels(heart$Thallium) <- c("Normal", "Fixed Defect", "Reversable Defect")

heart

# Check for missing values
missing_values<- colSums(is.na(heart))
missing_values

#check unique values for each colum
checkUniqueValues <- function(data) {
  for (col in names(data)) {
    unique_values <- unique(data[[col]])
    print(paste("Unique values in column", col, ":"))
    print(unique_values)
    print("-----------------------------")
  }
}
checkUniqueValues(heart)

#VISUALISATION

#Barchart

ggplot(heart, aes(Heart.Disease, fill=Heart.Disease)) + 
  geom_bar() +
  labs(title = "Heart Disease Presence", x="Heart Disease", y="Number of patients") +
  guides(fill=FALSE)

ggplot(heart, aes(Sex, fill=Heart.Disease)) + 
  geom_bar() +
  labs(fill="Heart Disease", x="Sex", y="Number of patients")

ggplot(heart, aes(Chest.pain.type, fill=Heart.Disease)) +
  geom_bar() +
  labs(fill="Heart Disease", x="Chest pain type", y="Number of patients")

ggplot(heart, aes(FBS.over.120, fill=Heart.Disease)) +
  geom_bar() +
  labs(fill="Heart Disease", x="Sugar level > 120 mg/dl", y="Number of patients")

ggplot(heart, aes(EKG.results, fill=Heart.Disease)) +
  geom_bar() +
  labs(fill="Heart Disease", x="Electrocardiogram on rest", y="Number of patients")

ggplot(heart, aes(Exercise.angina, fill=Heart.Disease)) +
  geom_bar() +
  labs(fill="Heart Disease", x="Presence of angina during exercise", y="Number of patients")

ggplot(heart, aes(Slope.of.ST, fill=Heart.Disease)) +
  geom_bar() +
  labs(fill="Heart Disease", x="Slope of the ST segment", y="Number of patients")

ggplot(heart, aes(Thallium, fill=Heart.Disease)) +
  geom_bar() +
  labs(fill="Heart Disease", x="Results of the blood flow", y="Number of patients")

ggplot(heart, aes(Number.of.vessels.fluro, fill=Heart.Disease)) +
  geom_bar() +
  labs(fill="Heart Disease", x="Number of main blood vessels coloured", y="Number of patients")


# Piechart

create_pie_chart <- function(table_data, main_title) {
  pie_percent <- prop.table(table_data) * 100  # Calculate percentages
  
  # Create the pie chart with formatted percentage labels and matching legend colors
  pie(table_data, labels = sprintf("%.1f%%", pie_percent), main = main_title, col = rainbow(length(table_data)))
  
  # Add legend to the right of the chart
  x_legend <- 1  # Adjust as needed
  y_legend <- 1  # Adjust as needed
  
  # Add legend to the specified position
  legend(x = x_legend, y = y_legend, legend = names(table_data), fill = rainbow(length(table_data)), cex = 0.5)
  
}

# Create pie charts with formatted percentage labels and matching legend colors
create_pie_chart(table(heart$Chest.pain.type), "Chest Pain Distribution")
create_pie_chart(table(heart$Number.of.vessels.fluro), "Number of Vessel Fluro Distribution")



#Stem and Leaf

BP <- heart$BP
stem(BP)


Cholesterol <- heart$Cholesterol
stem(Cholesterol)


Max.HR <- heart$Max.HR
stem(Max.HR)


#Histogram

ggplot(heart, aes(Age, fill=Heart.Disease)) +
  geom_histogram(binwidth=10) +
  labs(fill="Heart Disease", x="Age", y="Number of patients")

ggplot(heart, aes(BP, fill=Heart.Disease)) +
  geom_histogram(binwidth=3) +
  labs(fill="Heart Disease", x="Blood Pressure", y="Number of patients")

ggplot(heart, aes(Cholesterol, fill=Heart.Disease)) +
  geom_histogram(binwidth=10) +
  labs(fill="Heart Disease", x="Cholesterol (mg/dl)", y="Number of patients")

ggplot(heart, aes(Max.HR, fill=Heart.Disease)) +
  geom_histogram(binwidth=10) +
  labs(fill="Heart Disease", x="Maximum Heart Rate", y="Number of patients")

ggplot(heart, aes(ST.depression, fill=Heart.Disease)) +
  geom_histogram(binwidth=0.25) +
  labs(fill="Heart Disease", x="ST Depression", y="Number of patients")


#Box plot

par(mfrow = c(3, 2))
selected_variables <- c("Age", "BP", "Cholesterol", "Max.HR", "ST.depression")
for (variable in selected_variables) {
  boxplot(heart[[variable]], col = "skyblue", xlab = variable, main = variable)
}


# Summary statistics of the dataset
summary(heart)


# Selecting only numeric variables
cont_data <- select(heart, Age, BP, Cholesterol, Max.HR, ST.depression)
cont_data <- lapply(cont_data, as.numeric)

# Creating a custom function to calculate the desired summary statistics
custom_summary <- function(x) {
  round(
    c(Min = min(x),
      Quartile_1 = quantile(x, 0.25),
      Median = median(x),
      Mean = mean(x),
      Quartile_3 = quantile(x, 0.75),
      Max = max(x),
      SD = sd(x),
      Variance = var(x)),
    digits = 3)
}

summary_table <- sapply(cont_data, custom_summary)
print(summary_table)


# Hypothesis Testing - 1-Sample T-Test
t_test_1 <- t.test(heart$Age, mu = 54)
print(t_test_1)

# Hypothesis Testing - 2-Sample T-Test

# Separate data into two groups based on heart disease status
heart_disease <- heart$Age[heart$Heart.Disease == 'Presence']
no_heart_disease <- heart$Age[heart$Heart.Disease == 'Absence']

# Perform 2-sample t-test
t_test_2 <- t.test(heart_disease, no_heart_disease)
print(t_test_2)


# Goodness of Fit Test
observed_frequencies <- table(heart$`Chest.pain.type`)
print(observed_frequencies)

goodness_fit_test <- chisq.test(table(heart$`Chest.pain.type`), p = c(0.10, 0.20, 0.30, 0.40))
print(goodness_fit_test)

# Chi-Square Test of Independence
chisq.test(table(heart$Sex, heart$`Exercise.angina`))


# Correlation

cor_test <- cor.test(heart$BP, heart$Cholesterol)

# Scatter Plot

plot(heart$BP, heart$Cholesterol, main="Blood Pressure vs Cholesterol", xlab="Blood Pressure", ylab="Cholesterol", pch=19, col='green')
model_2 <- lm(heart$Cholesterol ~ heart$BP, data=heart)
abline(model_2, col="red")

# Print Correlation Test Result
print(cor_test)


# Simple Linear Regression

# Fit a linear regression model
lm_model <- lm(Max.HR ~ Age, data = heart)

# Plotting scatter plot
plot(heart$Age, heart$Max.HR, main = "Age vs. Max Heart Rate",
     xlab = "Age", ylab = "Max Heart Rate", pch = 16, col = "purple")

# Adding regression line to the plot
abline(lm_model, col = "red")

# Display the regression equation and R-squared value
summary(lm_model)

# ANOVA

# Perform ANOVA
anova_result <- aov(Max.HR ~ Chest.pain.type, data = heart)

# Summary of ANOVA
summary(anova_result)


