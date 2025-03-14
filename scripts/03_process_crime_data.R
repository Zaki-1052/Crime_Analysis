# 03_process_crime_data.R
# Purpose: Process crime data from FBI files to get crime rates by state

# Load required libraries
library(tidyverse)

# Create directories if they don't exist
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# Function to process each crime file
process_crime_file <- function(file_path, crime_type) {
  # Read the crime data
  crime_data <- read_csv(file_path)
  
  # Clean the data by removing commas and converting to numeric
  crime_data_clean <- crime_data %>%
    # Skip the "Total" row
    filter(State != "Total") %>%
    # Remove commas and convert to numeric
    mutate(
      Population = as.numeric(gsub(",", "", Population)),
      `Total Offenses` = as.numeric(gsub(",", "", `Total Offenses`))
    ) %>%
    # Calculate crime rate per 100,000 people
    mutate(
      crime_rate = (`Total Offenses` / Population)
    ) %>%
    # Select only state and crime rate
    select(state = State, crime_rate)
  
  # Add crime type identifier
  crime_data_clean$crime_type <- crime_type
  
  return(crime_data_clean)
}

# Process each crime file
person_crimes <- process_crime_file("data/crime/person_crimes.csv", "person")
property_crimes <- process_crime_file("data/crime/property_crimes.csv", "property")
society_crimes <- process_crime_file("data/crime/society_crimes.csv", "society")

# Check the first few rows of each
print(head(person_crimes))
print(head(property_crimes))
print(head(society_crimes))

# Combine into a single dataset in long format
crime_data_long <- bind_rows(person_crimes, property_crimes, society_crimes)

# Reshape to wide format for easier visualization
crime_data_wide <- crime_data_long %>%
  pivot_wider(
    names_from = crime_type,
    values_from = crime_rate,
    names_prefix = "crime_rate_"
  )

# Check the combined dataset
print(head(crime_data_wide))

# Save both formats of the processed data
write_csv(crime_data_wide, "data/processed/state_crime_rates.csv")
write_csv(crime_data_long, "data/processed/state_crime_rates_long.csv") # Useful for some visualization approaches

cat("Crime data processing complete.\n")