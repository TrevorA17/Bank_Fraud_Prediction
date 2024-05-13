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

