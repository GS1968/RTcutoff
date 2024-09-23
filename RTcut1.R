# Load necessary libraries
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

# Define constants
Chance <- 1/4  # the random guessing chance correct for an item with 4 options
ULimit <- 100  # the upper limit for threshold; user choice
LLimit <- 5    # the lower limit for threshold; user choice

# Define the function to calculate CUMP threshold value for item j
threshold.value <- function(j, x, y, mean_rt, sd_rt) {
  # Create contingency table for response time and response (0/1)
  item <- table(x[, j], y[, j])
  item <- as.matrix(item)
  
  # Extract response times
  S <- as.numeric(row.names(item))
  
  # Calculate P+ at each response time
  Pplus <- item[, 2] / apply(item, 1, sum)
  
  # Calculate cumulative P+
  cumP <- Pplus
  K <- length(item[, 2])
  for (k in 2:K) {
    cumP[k] <- sum(item[1:k, 2]) / sum(item[1:k, ])
  }
  
  # Identify indices where cumulative P+ is less than the Chance level
  ind <- (cumP < Chance)
  
  # Find the last index where cumP is less than Chance
  LL <- which(ind, arr.ind = TRUE)
  LL <- as.vector(LL)
  
  # Dynamically adjust ULimit based on mean and standard deviation of response times
  dynamic_ULimit <- mean_rt[j] + (1 * sd_rt[j])  # 1 SD above the mean
  
  if (length(LL) == 0) {
    # If no index where cumP < Chance, set threshold to dynamic_ULimit
    value.T <- dynamic_ULimit
  } else {
    # Otherwise, set the threshold to the last valid index
    value.T <- S[LL[length(LL)]]
  }
  
  # Ensure the threshold value is within the specified limits
  if (value.T > dynamic_ULimit) {
    value.T <- dynamic_ULimit
  } else if (value.T < LLimit) {
    value.T <- LLimit
  }
  
  # Return the threshold value
  return(drop(value.T))
}

# Function to read response time data from a .csv file and calculate CUMP thresholds for a given participant
calculate_thresholds_from_csv <- function(response_time_csv, response_data_csv, participant_row) {
  # Read response time data from .csv file
  x <- read.csv(response_time_csv, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  
  # Read response (0/1) data from another .csv file
  y <- read.csv(response_data_csv, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  
  # Convert data frames to matrices
  x <- as.matrix(x)
  y <- as.matrix(y)
  
  # Calculate mean and SD of response times for each item
  mean_rt <- apply(x, 2, mean, na.rm = TRUE)
  sd_rt <- apply(x, 2, sd, na.rm = TRUE)
  
  # Calculate the threshold values for all items
  num_items <- ncol(x)
  threshold_values <- sapply(1:num_items, function(j) threshold.value(j, x, y, mean_rt, sd_rt))
  print(threshold_values)
  
  # Select the specified participant's response times
  participant_response_times <- x[participant_row, ]
  
  # Create data frames for plotting
  threshold_data <- data.frame(Item = 1:num_items, Threshold = threshold_values)
  participant_data <- data.frame(Item = 1:num_items, ResponseTime = participant_response_times)
  mean_data <- data.frame(Item = 1:num_items, MeanRT = mean_rt)
  sd_plus_1_5_data <- data.frame(Item = 1:num_items, SDPlus1_5 = mean_rt + (1.5 * sd_rt))
  
  # Create x-axis labels for the items
  item_labels <- paste("Item", 1:num_items)
  
  # Plotting the participant's response times and CUMP thresholds along with mean and +1.5 SD lines
  ggplot() +
    geom_line(data = threshold_data, aes(x = Item, y = Threshold, color = "CUMP Threshold"), size = 1.5) +  # Thicker line
    geom_point(data = threshold_data, aes(x = Item, y = Threshold, color = "CUMP Threshold"), size = 5) +  # Larger circles
    geom_line(data = participant_data, aes(x = Item, y = ResponseTime, color = "Response Time"), linetype = "dotted", size = 1.5) +  # Thicker line
    geom_point(data = participant_data, aes(x = Item, y = ResponseTime, color = "Response Time"), shape = 17, size = 5) +  # Larger triangles
    geom_line(data = mean_data, aes(x = Item, y = MeanRT, color = "Mean RT"), size = 1.5) +
    geom_line(data = sd_plus_1_5_data, aes(x = Item, y = SDPlus1_5, color = "+1.5 SD RT"), linetype = "dashed", size = 1.5) +
    xlab("Item") + ylab("Response Time (Seconds)") +
    scale_x_continuous(breaks = 1:num_items, labels = item_labels) +
    scale_color_manual(values = c("CUMP Threshold" = "blue", "Response Time" = "red", "Mean RT" = "black", "+1.5 SD RT" = "green")) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.title = element_blank(),  # Removed "Legend"
      legend.text = element_text(size = 14),
      axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_line(color = "grey", linetype = "dotted")
    )
}

# Example usage:
# Call the function to calculate and plot CUMP thresholds for a given participant (e.g., participant row 5) with the first csv file being
associated with introduction of response times, followed by the data file (response vectors).
calculate_thresholds_from_csv("C:\\Users\\Georg\\Desktop\\RE41rts.csv", "C:\\Users\\Georg\\Desktop\\RE41data.csv", participant_row = 216)
