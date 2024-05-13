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

