# Create a directory named "models" if it doesn't exist
if (!file.exists("./models")) {
  dir.create("./models")
}

# Saving the decision trees model for the bank fraud dataset
saveRDS(decision_trees_model, file = "./models/bank_fraud_decision_trees_model.rds")

# Load the saved decision trees model
loaded_bank_fraud_decision_trees_model <- readRDS("./models/bank_fraud_decision_trees_model.rds")

# Prepare new data for prediction (replace with your actual new data)
new_bank_data <- data.frame(
  transaction_id = c(1001, 1002, 1003),  # Example transaction IDs
  transaction_amount = c(1500, 700, 3000),  # Example transaction amounts
  location = c("New York", "Chicago", "Los Angeles"),  # Example locations
  merchant = c("ABC Corp", "XYZ Inc", "ABC Corp"),  # Example merchants
  age = c(40, 35, 28),  # Example ages
  gender = c("M", "F", "M")  # Example genders
)

# Use the loaded model to make predictions for new bank fraud data
predictions_bank_loaded_model <- predict(loaded_bank_fraud_decision_trees_model, newdata = new_bank_data)

# Print predictions
print(predictions_bank_loaded_model)
