# 02_process_incarceration_data.R
# Purpose: Extract state-level incarceration rates from BJS data

# Load required libraries
library(tidyverse)

# Create directories if they don't exist
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# Read the simplified imprisonment rates data
incarceration_data <- read_csv("data/incarceration/p22stt07.csv", skip = 1) 
# Skip=1 skips the category header row

# Clean up the data
incarceration_rates <- incarceration_data %>%
  # Remove the total rows and other non-state entries
  filter(!Jurisdiction %in% c("U.S. total", "Federal/a", "State")) %>%
  # Rename columns for clarity
  rename(
    state = Jurisdiction,
    imprisonment_rate_2022 = `Age 18 or older`
  ) %>%
  # Select only the columns we need
  select(state, imprisonment_rate_2022)

# Check the results
print(head(incarceration_rates))
print(summary(incarceration_rates))

# Save the processed data
write_csv(incarceration_rates, "data/processed/state_incarceration_rates.csv")

cat("Incarceration data processing complete.\n")