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

# Define the number of bootstrap samples
num_bootstraps <- 1000

# Initialize a vector to store bootstrap statistics
bootstrap_statistics <- numeric(num_bootstraps)

# Set seed for reproducibility
set.seed(123)

# Perform bootstrapping
for (i in 1:num_bootstraps) {
  # Sample with replacement from the dataset
  bootstrap_sample <- bank_data[sample(nrow(bank_data), replace = TRUE), ]
  
  # Calculate the statistic of interest (e.g., mean transaction amount)
  # For example, let's calculate the mean transaction amount for each bootstrap sample
  bootstrap_statistics[i] <- mean(bootstrap_sample$transaction_amount)
}

# Calculate confidence interval (e.g., 95% confidence interval)
confidence_interval <- quantile(bootstrap_statistics, c(0.025, 0.975))

# Print confidence interval
print("95% Confidence Interval for Mean Transaction Amount:")
print(confidence_interval)
