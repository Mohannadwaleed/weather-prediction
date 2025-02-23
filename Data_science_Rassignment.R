# Sample dataset
my_data <- data.frame(
  Outlook = c("Rainy", "Rainy", "Overcast", "Sunny", "Sunny", "Sunny", "Overcast", "Rainy", "Rainy", "Sunny", "Rainy", "Overcast", "Overcast", "Sunny"),
  Temperature = c("Hot", "Hot", "Hot", "Mild", "Cool", "Cool", "Cool", "Mild", "Cool", "Mild", "Mild", "Mild", "Hot", "Hot"),
  Humidity = c("High", "High", "High", "High", "Normal", "Normal", "Normal", "High", "Normal", "Normal", "Normal", "High", "Normal", "High"),
  Windy = c("False", "True", "False", "False", "False", "True", "True", "False", "False", "False", "True", "True", "False", "True"),
  Class = c("No", "No", "Yes", "Yes", "Yes", "No", "Yes", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "No")
)

# Function to calculate accuracy
calculate_accuracy <- function(predicted, actual) {
  mean(predicted == actual)
}

# Function to implement ZeroR algorithm
zero_r <- function(my_data) {
  # Choose the most frequent class as the prediction
  prediction <- names(sort(table(my_data$Class), decreasing = TRUE)[1])
  return(rep(prediction, nrow(my_data)))
}

# Function to implement OneR algorithm
one_r <- function(my_data) {
  predictor_names <- names(my_data)[1:(ncol(my_data)-1)]
  best_accuracy <- 0
  best_predictor <- ""
  
  for (predictor in predictor_names) {
    prediction <- rep("", nrow(my_data))
    
    for (i in seq_along(my_data[[predictor]])) {
      value <- my_data[[predictor]][i]
      prediction[i] <- names(sort(table(my_data[my_data[[predictor]] == value, "Class"]), decreasing = TRUE)[1])
    }
    
    accuracy <- calculate_accuracy(prediction, my_data$Class)
    
    if (accuracy > best_accuracy) {
      best_accuracy <- accuracy
      best_predictor <- predictor
    }
  }
  
  return(list(predictor = best_predictor, accuracy = best_accuracy, prediction = prediction))
}

# Use the ZeroR algorithm
zero_r_prediction <- zero_r(my_data)
zero_r_accuracy <- calculate_accuracy(zero_r_prediction, my_data$Class)
cat("ZeroR Accuracy:", zero_r_accuracy, "\n")

# Use the OneR algorithm
one_r_result <- one_r(my_data)
cat("Best Predictor for OneR:", one_r_result$predictor, "\n")
cat("OneR Accuracy:", one_r_result$accuracy, "\n")
