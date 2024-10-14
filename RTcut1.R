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

# Function to read response time data from a .csv file and calculate CUMP thresholds for all participants
calculate_flags_and_plot <- function(response_time_csv, response_data_csv, selected_participant_row) {
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
  
  # Get the number of participants and items
  num_participants <- nrow(x)
  num_items <- ncol(x)
  
  # Calculate the threshold values for all items
  threshold_values <- sapply(1:num_items, function(j) threshold.value(j, x, y, mean_rt, sd_rt))
  
  # Calculate the mean and standard deviation of the CUMP thresholds
  mean_cump <- mean(threshold_values, na.rm = TRUE)
  sd_cump <- sd(threshold_values, na.rm = TRUE)
  
  # Initialize a vector to store the flag status for each participant
  flag_vector <- numeric(num_participants)
  
  # Loop through each participant and calculate flags based on the mean and SD of the CUMP thresholds
  for (i in 1:num_participants) {
    # Select the participant's response times
    participant_response_times <- x[i, ]
    
    # Create flag variable: identify if participant's response time is + or - 1 SD from the mean of the CUMP thresholds
    flags <- (participant_response_times < (mean_cump - 1 * sd_cump)) | 
             (participant_response_times > (mean_cump + 1 * sd_cump))
    
    # Calculate the percentage of flagged items
    flagged_percentage <- sum(flags, na.rm = TRUE) / num_items
    
    # Flag participant if more than 50% of their items are flagged
    flag_vector[i] <- ifelse(flagged_percentage > 0.5, 1, 0)
  }
  
  # Create a data frame with participant row numbers and their flag status
  flag_data <- data.frame(Participant = 1:num_participants, Flagged = flag_vector)
  
  # Print flag data for all participants
  print(flag_data)
  
  # Now plot for the selected participant
  participant_response_times <- x[selected_participant_row, ]
  
  # Create data frames for plotting
  threshold_data <- data.frame(Item = 1:num_items, Threshold = threshold_values)
  participant_data <- data.frame(Item = 1:num_items, ResponseTime = participant_response_times)
  mean_data <- data.frame(Item = 1:num_items, MeanRT = mean_rt)
  sd_plus_1_5_data <- data.frame(Item = 1:num_items, SDPlus1_5 = threshold_values + (1.5 * sd_rt))  # +1.5 SD from CUMP threshold
  
  # Create x-axis labels for the items
  item_labels <- paste("Item", 1:num_items)
  
  # Plotting the selected participant's response times, CUMP thresholds, and +1.5 SD line
  plot <- ggplot() +
    geom_line(data = threshold_data, aes(x = Item, y = Threshold, color = "CUMP Threshold"), size = 1.5) +  # Thicker line
    geom_point(data = threshold_data, aes(x = Item, y = Threshold, color = "CUMP Threshold"), size = 5) +  # Larger circles
    geom_line(data = participant_data, aes(x = Item, y = ResponseTime, color = "Response Time"), linetype = "dotted", size = 1.5) +  # Thicker line
    geom_point(data = participant_data, aes(x = Item, y = ResponseTime, color = "Response Time"), shape = 17, size = 5) +  # Larger triangles
    geom_line(data = mean_data, aes(x = Item, y = MeanRT, color = "Mean RT"), size = 1.5) +
    geom_line(data = sd_plus_1_5_data, aes(x = Item, y = SDPlus1_5, color = "+1.5 SD RT"), linetype = "dashed", size = 1.5) +  # +1.5 SD from CUMP threshold
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
  
  # Print and return the plot for the selected participant
  print(plot)
  
  # Return the flag data for all participants
  return(flag_data)
}

# Example usage:
# Calculate and print flag data for all participants and plot for a selected participant (e.g., participant row 5)
# flag_data <- calculate_flags_and_plot("C:\\Users\\Georg\\Desktop\\RE41rts.csv", "C:\\Users\\Georg\\Desktop\\RE41data.csv", selected_participant_row = 5)
