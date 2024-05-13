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

# Load necessary library
library(caTools)

# Set seed for reproducibility
set.seed(123)

# Split data into training and testing sets (70% training, 30% testing)
split <- sample.split(bank_data$fraud_label, SplitRatio = 0.7)
train_data <- subset(bank_data, split == TRUE)
test_data <- subset(bank_data, split == FALSE)

# Print dimensions of training and testing sets
print("Dimensions of Training Data:")
print(dim(train_data))
print("Dimensions of Testing Data:")
print(dim(test_data))
