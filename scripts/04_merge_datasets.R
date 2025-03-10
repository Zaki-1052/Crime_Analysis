# 04_merge_datasets.R - Simple percentage-based approach
# Purpose: Create a complete master dataset using simple percentages

# Load required libraries
library(tidyverse)
library(stringr)

# Create standard state name function for consistency across datasets
standardize_state_name <- function(state_name) {
  # Convert to title case
  name <- str_to_title(state_name)
  
  # Handle common variations
  name <- case_when(
    name == "District Of Columbia" ~ "District of Columbia",
    name == "D.c." ~ "District of Columbia",
    name == "D.C." ~ "District of Columbia",
    name == "Washington Dc" ~ "District of Columbia",
    TRUE ~ name
  )
  
  return(name)
}

# Create directories if they don't exist
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# Load and standardize datasets
read_and_standardize <- function(file_path) {
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }
  
  # Read the file with more robust error handling
  tryCatch({
    df <- read_csv(file_path, col_types = cols(.default = "c"), show_col_types = FALSE)
    # Convert all column types appropriately after reading as character
    df <- type_convert(df)
    # Standardize state names
    if ("state" %in% names(df)) {
      df <- df %>% mutate(state = standardize_state_name(state))
    }
    return(df)
  }, error = function(e) {
    message(paste("Error reading", file_path, ":", e$message))
    return(NULL)
  })
}

# Load all processed datasets
political_data <- read_and_standardize("data/processed/state_political_leaning.csv")
incarceration_data <- read_and_standardize("data/processed/state_incarceration_rates.csv")
crime_data <- read_and_standardize("data/processed/state_crime_rates.csv")

# Print detailed information about each dataset
print_dataset_info <- function(name, data) {
  cat("\n", name, "Dataset:\n")
  cat("  Columns:", paste(colnames(data), collapse=", "), "\n")
  cat("  Rows:", nrow(data), "\n")
  cat("  States:", paste(sort(data$state), collapse=", "), "\n")
  cat("\n")
}

print_dataset_info("Political", political_data)
print_dataset_info("Incarceration", incarceration_data)
print_dataset_info("Crime", crime_data)

# Merge datasets
merged_data <- tibble(state = political_data$state) %>%
  left_join(political_data, by = "state") %>%
  left_join(incarceration_data, by = "state") %>%
  left_join(crime_data, by = "state")

# Handle missing values if any
if (any(is.na(merged_data))) {
  cat("\nHandling missing values...\n")
  
  merged_data <- merged_data %>%
    mutate(across(everything(), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
}

# Create derived columns with simple percentage conversions
merged_data <- merged_data %>%
  mutate(
    # Convert imprisonment rate to percentage (divide by 1000)
    incarceration_intensity = imprisonment_rate_2022 / 1000,
    
    # Already percentages based on your example, but let's scale to 0-100 range for visualization
    crime_rate_person_scaled = crime_rate_person * 100,
    crime_rate_property_scaled = crime_rate_property * 100,
    crime_rate_society_scaled = crime_rate_society * 100,
    
    # Create categorical political leaning
    political_category = case_when(
      political_leaning < -0.2 ~ "Strong Democrat",
      political_leaning < -0.1 ~ "Lean Democrat",
      political_leaning < 0.1 ~ "Swing",
      political_leaning < 0.2 ~ "Lean Republican",
      TRUE ~ "Strong Republican"
    )
  )

# Print a sample to verify
cat("\nSample of processed data:\n")
merged_data %>%
  select(state, imprisonment_rate_2022, incarceration_intensity, 
         crime_rate_person, crime_rate_person_scaled) %>%
  head(5) %>%
  print()

# Sort by state name for consistency
merged_data <- merged_data %>% arrange(state)

# Save the final merged dataset
write_csv(merged_data, "data/processed/master_dataset.csv")

# Also save a version with state as row names for easier use in visualization
merged_data_matrix <- as.data.frame(merged_data)
rownames(merged_data_matrix) <- merged_data_matrix$state
write_csv(merged_data, "data/processed/master_dataset_indexed.csv")

cat("\nData merging complete! Final dataset has", nrow(merged_data), 
    "rows and", ncol(merged_data), "columns.\n")