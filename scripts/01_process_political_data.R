# 01_process_political_data.R (County-level Data)
# Purpose: Process multiple county-level voting datasets to get state-level political leaning

# Load required libraries
library(tidyverse)

# Create directories if they don't exist
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# Configuration parameters
# Options: "sum" (add up votes across years) or "average" (average votes across years)
combine_method <- "average"  

# Define file paths for the election datasets
election_files <- c(
  "data/political/election_data_2020.csv",
  "data/political/election_data_2024.csv"
)

# Function to read and standardize each file
read_election_data <- function(file_path) {
  if (file.exists(file_path)) {
    cat("Reading file:", file_path, "\n")
    data <- read_csv(file_path, show_col_types = FALSE)
    
    # Standardize data structure
    # First, identify which format the dataset is in
    if ("state_abbr" %in% names(data) && !("state_name" %in% names(data))) {
      cat("Converting 2016-style dataset format...\n")
      
      # This appears to be 2016 data format
      data <- data %>%
        # Remove any index column if it exists
        select(-any_of(c("...1"))) %>%
        # Rename state_abbr to state_name if needed
        rename_with(~ "state_name", .cols = any_of(c("state_abbr"))) %>%
        # Rename combined_fips to county_fips if needed
        rename_with(~ "county_fips", .cols = any_of(c("combined_fips")))
    }
    
    # Ensure all required columns exist and have correct data types
    data <- data %>%
      # Convert character numerical columns to numeric
      mutate(across(c("per_point_diff", "diff", "per_gop", "per_dem"), 
                    ~ as.numeric(as.character(.x))),
             # Ensure vote columns are numeric
             across(c("votes_gop", "votes_dem", "total_votes"), 
                    ~ as.numeric(as.character(.x))))
    
    # Keep only the needed columns
    required_cols <- c("state_name", "county_fips", "county_name", 
                       "votes_gop", "votes_dem", "total_votes", 
                       "diff", "per_gop", "per_dem", "per_point_diff")
    
    # Check which required columns are present
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      warning(paste("Missing columns in", file_path, ":", paste(missing_cols, collapse = ", ")))
    }
    
    # Select only required columns that exist in the dataset
    data <- data %>% select(any_of(required_cols))
    
    return(data)
  } else {
    warning(paste("File not found:", file_path))
    return(NULL)
  }
}

# Read and standardize all available election data files
election_data_list <- lapply(election_files, read_election_data)
# Remove any NULL entries (files that couldn't be read)
election_data_list <- election_data_list[!sapply(election_data_list, is.null)]

if (length(election_data_list) == 0) {
  stop("No valid election data files found.")
}

# Print info about the loaded datasets
cat("Loaded", length(election_data_list), "election datasets.\n")

# Check the column names in each dataset for debugging
for (i in seq_along(election_data_list)) {
  cat("Dataset", i, "columns:", paste(names(election_data_list[[i]]), collapse = ", "), "\n")
}

# Function to combine datasets based on the chosen method
combine_datasets <- function(data_list, method = "average") {
  # If only one dataset is available, return it as is
  if (length(data_list) == 1) {
    return(data_list[[1]])
  }
  
  # Combine all datasets into one dataframe
  cat("Combining datasets...\n")
  combined_data <- bind_rows(data_list)
  
  # Group by state and county to combine data across years
  result <- combined_data %>%
    group_by(state_name, county_fips, county_name) %>%
    summarize(
      # Either sum or average the votes based on the chosen method
      votes_gop = if(method == "sum") sum(votes_gop, na.rm = TRUE) else mean(votes_gop, na.rm = TRUE),
      votes_dem = if(method == "sum") sum(votes_dem, na.rm = TRUE) else mean(votes_dem, na.rm = TRUE),
      total_votes = if(method == "sum") sum(total_votes, na.rm = TRUE) else mean(total_votes, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Recalculate derived metrics based on the combined data
    mutate(
      diff = votes_gop - votes_dem,
      per_gop = votes_gop / total_votes,
      per_dem = votes_dem / total_votes,
      per_point_diff = per_gop - per_dem
    )
  
  return(result)
}

# Combine datasets using the specified method
combined_election_data <- combine_datasets(election_data_list, combine_method)
cat("Combined data using method:", combine_method, "\n")

# Check how many states we have in the combined dataset
state_count <- length(unique(combined_election_data$state_name))
cat("Number of unique states in combined dataset:", state_count, "\n")

# Display some of the states to check coverage
unique_states <- unique(combined_election_data$state_name)
cat("First few states:", paste(head(sort(unique_states)), collapse=", "), "...\n")

# Aggregate votes by state
state_votes <- combined_election_data %>%
  # Group by state name
  group_by(state_name) %>%
  # Sum votes by party for each state
  summarize(
    total_rep_votes = sum(votes_gop, na.rm = TRUE),
    total_dem_votes = sum(votes_dem, na.rm = TRUE),
    total_votes = sum(total_votes, na.rm = TRUE)
  ) %>%
  # Calculate percentage metrics
  mutate(
    pct_rep = total_rep_votes / total_votes,
    pct_dem = total_dem_votes / total_votes,
    political_leaning = pct_rep - pct_dem  # Positive values = more Republican
  )

# Check how many states we have after aggregation
cat("Number of states after aggregation:", nrow(state_votes), "\n")

# Print a summary to check the results
print(head(state_votes))

# Final dataset for saving
final_state_votes <- state_votes %>%
  # Rename state_name to state for consistency with other datasets
  rename(state = state_name)

# Save the processed data
write_csv(final_state_votes, "data/processed/state_political_leaning.csv")

cat("Political data processing complete.\n")