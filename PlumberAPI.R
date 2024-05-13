# Load necessary libraries
library(plumber)

# Load the saved decision trees model for bank fraud prediction
loaded_bank_fraud_decision_trees_model <- readRDS("./models/bank_fraud_decision_trees_model.rds")

#* @apiTitle Bank Fraud Prediction Model API
#* @apiDescription Used to predict whether a transaction is fraudulent.



#* @post /predict_bank_fraud
#* @param transaction_id Numeric: Transaction ID
#* @param transaction_amount Numeric: Transaction amount
#* @param location Character: Location of the transaction
#* @param merchant Character: Merchant of the transaction
#* @param age Numeric: Age of the customer
#* @param gender Factor: Gender of the customer (M/F)
predict_bank_fraud <- function(transaction_id, transaction_amount, location, merchant, age, gender) {
  # Create a data frame using the arguments
  new_bank_data <- data.frame(
    transaction_id = as.numeric(transaction_id),
    transaction_amount = as.numeric(transaction_amount),
    location = as.character(location),
    merchant = as.character(merchant),
    age = as.numeric(age),
    gender = as.factor(gender)
  )
  
  # Use the loaded model to make predictions
  prediction <- predict(loaded_bank_fraud_decision_trees_model, newdata = new_bank_data)
  
  # Return the prediction
  return(as.character(prediction))
}

