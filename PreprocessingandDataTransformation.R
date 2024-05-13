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

# Check for missing values
missing_values <- sum(is.na(bank_data))

# Display summary of missing values
print(paste("Number of missing values:", missing_values))

# Summary of missing values by column
print(summary(is.na(bank_data)))
