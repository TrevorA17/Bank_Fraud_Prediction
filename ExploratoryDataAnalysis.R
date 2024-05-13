# Load dataset
bank_data <- read.csv("data/fraud_dataset.csv", colClasses = c(
  transaction_id = "integer",
  transaction_amount = "numeric",
  location = "factor",
  merchant = "factor",
  age = "numeric",
  gender = "factor",
  fraud_label = "factor"
))

# Display the structure of the dataset
str(bank_data)

# View the first few rows of the dataset
head(bank_data)

# View the dataset in a separate viewer window
View(bank_data)

# Calculate frequency of categorical variables
frequency_location <- table(bank_data$location)
frequency_merchant <- table(bank_data$merchant)
frequency_gender <- table(bank_data$gender)
frequency_fraud_label <- table(bank_data$fraud_label)

# Print frequency tables
print("Frequency of Location:")
print(frequency_location)

print("Frequency of Merchant:")
print(frequency_merchant)

print("Frequency of Gender:")
print(frequency_gender)

print("Frequency of Fraud Label:")
print(frequency_fraud_label)

# Calculate measures of central tendency for numerical variables
mean_transaction_amount <- mean(bank_data$transaction_amount)
median_transaction_amount <- median(bank_data$transaction_amount)
mode_transaction_amount <- as.numeric(names(sort(-table(bank_data$transaction_amount))[1]))

mean_age <- mean(bank_data$age)
median_age <- median(bank_data$age)
mode_age <- as.numeric(names(sort(-table(bank_data$age))[1]))

# Print measures of central tendency
print("Measures of Central Tendency for Transaction Amount:")
print(paste("Mean:", mean_transaction_amount))
print(paste("Median:", median_transaction_amount))
print(paste("Mode:", mode_transaction_amount))

print("Measures of Central Tendency for Age:")
print(paste("Mean:", mean_age))
print(paste("Median:", median_age))
print(paste("Mode:", mode_age))


# Calculate measures of distribution for numerical variables
range_transaction_amount <- range(bank_data$transaction_amount)
variance_transaction_amount <- var(bank_data$transaction_amount)
standard_deviation_transaction_amount <- sd(bank_data$transaction_amount)

range_age <- range(bank_data$age)
variance_age <- var(bank_data$age)
standard_deviation_age <- sd(bank_data$age)

# Print measures of distribution
print("Measures of Distribution for Transaction Amount:")
print(paste("Range:", range_transaction_amount[2] - range_transaction_amount[1]))
print(paste("Variance:", variance_transaction_amount))
print(paste("Standard Deviation:", standard_deviation_transaction_amount))

print("Measures of Distribution for Age:")
print(paste("Range:", range_age[2] - range_age[1]))
print(paste("Variance:", variance_age))
print(paste("Standard Deviation:", standard_deviation_age))

# Calculate correlation coefficient for numerical variables
correlation_matrix <- cor(bank_data[c("transaction_amount", "age")])

# Print correlation matrix
print("Correlation Matrix:")
print(correlation_matrix)

# Create contingency tables for categorical variables
contingency_location_fraud_label <- table(bank_data$location, bank_data$fraud_label)
contingency_merchant_fraud_label <- table(bank_data$merchant, bank_data$fraud_label)
contingency_gender_fraud_label <- table(bank_data$gender, bank_data$fraud_label)

# Print contingency tables
print("Contingency Table: Location vs. Fraud Label")
print(contingency_location_fraud_label)

print("Contingency Table: Merchant vs. Fraud Label")
print(contingency_merchant_fraud_label)

print("Contingency Table: Gender vs. Fraud Label")
print(contingency_gender_fraud_label)

# Load necessary libraries (if not already loaded)
library(stats)

# Perform ANOVA
anova_result <- aov(transaction_amount ~ location, data = bank_data)

# Print ANOVA results
print(summary(anova_result))

# Load necessary libraries
library(ggplot2)
library(gridExtra)

# Univariate Plot: Transaction Amount
univariate_plot_transaction_amount <- ggplot(bank_data, aes(x = transaction_amount)) +
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Transaction Amount", x = "Transaction Amount", y = "Frequency")

# Univariate Plot: Age
univariate_plot_age <- ggplot(bank_data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Age", x = "Age", y = "Frequency")

# Multivariate Plot: Transaction Amount vs. Age
multivariate_plot <- ggplot(bank_data, aes(x = transaction_amount, y = age)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Transaction Amount vs. Age", x = "Transaction Amount", y = "Age")

# Display plots in R
grid.arrange(univariate_plot_transaction_amount, univariate_plot_age, multivariate_plot, ncol = 2, nrow = 2)

# Boxplot: Transaction Amount by Fraud Label
boxplot_transaction_amount_fraud_label <- ggplot(bank_data, aes(x = fraud_label, y = transaction_amount, fill = fraud_label)) +
  geom_boxplot() +
  labs(title = "Transaction Amount by Fraud Label", x = "Fraud Label", y = "Transaction Amount") +
  theme_minimal()

# Barplot: Frequency of Fraud Label
barplot_fraud_label <- ggplot(bank_data, aes(x = factor(fraud_label))) +
  geom_bar(fill = "skyblue") +
  labs(title = "Frequency of Fraud Label", x = "Fraud Label", y = "Frequency") +
  theme_minimal()

# Barplot: Frequency of Location
barplot_location <- ggplot(bank_data, aes(x = location)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Frequency of Location", x = "Location", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Barplot: Frequency of Merchant
barplot_merchant <- ggplot(bank_data, aes(x = merchant)) +
  geom_bar(fill = "coral") +
  labs(title = "Frequency of Merchant", x = "Merchant", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display plots in R
grid.arrange(boxplot_transaction_amount_fraud_label, barplot_fraud_label, barplot_location, barplot_merchant, ncol = 2, nrow = 2)

