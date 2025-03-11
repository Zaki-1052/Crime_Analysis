# create_visualization_final.R
# Purpose: Create an comprehensive visualization showing the relationship between
# political leaning, incarceration rates, and crime rates across states

# ================================================
# CONFIGURATION PARAMETERS - ADJUST AS NEEDED
# ================================================

# Crime bar scaling parameters
CRIME_BAR_SCALE_FACTOR = 0.66     # Controls overall height of crime bars (lower = shorter bars)
CRIME_BAR_WIDTH = 5            # Controls thickness of crime bars
CRIME_BAR_ALPHA = 0.9            # Controls opacity of crime bars (0-1)

# Incarceration stripe parameters
INCARCERATION_STRIPE_ALPHA = 0.4 # Controls opacity of incarceration stripes (0-1)
INCARCERATION_DENSITY_MIN = 0.03 # Minimum density for stripes (lower = fewer stripes)
INCARCERATION_DENSITY_MAX = 0.4  # Maximum density for stripes (higher = more stripes)

# Color scheme
PERSON_CRIME_COLOR = "#FF6B00"   # Orange for person crimes
PROPERTY_CRIME_COLOR = "#00DAFF" # Cyan for property crimes
SOCIETY_CRIME_COLOR = "#FF00FF"  # Magenta for society crimes

# Political color scheme
DEM_COLOR = "#0047AB"            # Strong blue for Democratic
SWING_COLOR = "#9370DB"          # Light purple for swing states
REP_COLOR = "#B22222"            # Strong red for Republican

# ================================================
# Load required libraries
# ================================================

library(tidyverse)
library(sf)
library(tigris)
library(cowplot)
library(ggpattern) # For stripe patterns to represent incarceration
library(shadowtext) # For adding outlined text to bars

# Set options
options(tigris_use_cache = TRUE)

# ================================================
# Load and prepare data
# ================================================

# Load master dataset
master_data <- read_csv("data/processed/master_dataset.csv", show_col_types = FALSE) %>%
  # Ensure numeric columns
  mutate(across(where(is.numeric), as.numeric))

# Get US state boundaries
us_states <- states(cb = TRUE) %>%
  filter(!STUSPS %in% c("PR", "VI", "GU", "AS", "MP")) %>%
  select(GEOID, NAME, geometry) %>%
  rename(state = NAME)

# Handle state name standardization
us_states$state <- case_when(
  us_states$state == "District Of Columbia" ~ "District of Columbia",
  TRUE ~ us_states$state
)

# Join datasets
map_data <- us_states %>%
  left_join(master_data, by = "state")

# Transform political leaning to create stronger red-blue distinction
map_data <- map_data %>%
  mutate(
    # Use power transformation to amplify differences while preserving sign
    enhanced_political = sign(political_leaning) * (abs(political_leaning))^0.3,
    
    # Apply additional boost to ensure clear red/blue distinction
    final_political = case_when(
      political_leaning > 0.15 ~ enhanced_political * 2,  # Stronger red for Republican states
      political_leaning < -0.15 ~ enhanced_political * 2, # Stronger blue for Democratic states
      abs(political_leaning) > 0.05 ~ enhanced_political * 2, # Moderate boost for leaning states
      TRUE ~ enhanced_political * 1.5  # Slight boost for true swing states
    )
  )

# Debug: Print transformed values for Alaska and Hawaii
cat("Alaska final_political:", map_data$final_political[map_data$state == "Alaska"], "\n")
cat("Hawaii final_political:", map_data$final_political[map_data$state == "Hawaii"], "\n")

# Split data for continental US and insets
continental_states <- map_data %>% filter(!state %in% c("Alaska", "Hawaii"))
alaska <- map_data %>% filter(state == "Alaska")
hawaii <- map_data %>% filter(state == "Hawaii")

# ================================================
# Calculate centroids for bar placement
# ================================================

calculate_centroids <- function(sf_data) {
  centroids <- st_centroid(sf_data)
  coords <- st_coordinates(centroids)
  
  df <- tibble(
    state = sf_data$state,
    centroid_x = coords[, "X"],
    centroid_y = coords[, "Y"]
  )
  
  return(df)
}

# Calculate centroids with error handling
all_centroids <- tryCatch({
  calculate_centroids(map_data)
}, error = function(e) {
  message("Error calculating centroids: ", e$message)
  centroids <- st_centroid(map_data$geometry)
  coords <- st_coordinates(centroids)
  return(tibble(
    state = map_data$state,
    centroid_x = coords[, "X"],
    centroid_y = coords[, "Y"]
  ))
})

# ================================================
# Prepare crime bars with 3D-like appearance
# ================================================

prepare_crime_bars <- function(map_data, centroids) {
  # Extract crime data and join with centroids
  bars_data <- map_data %>%
    st_drop_geometry() %>%
    select(state, crime_rate_person_scaled, crime_rate_property_scaled, crime_rate_society_scaled) %>%
    left_join(centroids, by = "state") %>%
    mutate(
      # Apply configured scaling factor
      scale_factor = CRIME_BAR_SCALE_FACTOR,
      
      # Calculate segment positions for stacked bar
      person_y_start = centroid_y,
      person_y_end = person_y_start + (crime_rate_person_scaled * scale_factor),
      
      property_y_start = person_y_end,
      property_y_end = property_y_start + (crime_rate_property_scaled * scale_factor),
      
      society_y_start = property_y_end,
      society_y_end = society_y_start + (crime_rate_society_scaled * scale_factor),
      
      # Calculate offset for 3D effect (slight horizontal shift)
      x_offset = 0.02 * (max(centroids$centroid_x) - min(centroids$centroid_x)) / 50,
      
      # Total crime height (for labels)
      total_height = (crime_rate_person_scaled + crime_rate_property_scaled + crime_rate_society_scaled) * scale_factor
    )
  
  return(bars_data)
}

crime_bars <- prepare_crime_bars(map_data, all_centroids)

# ================================================
# Create map with improved elements
# ================================================

create_map <- function(sf_data, bars_data, title = NULL, show_legend = TRUE) {
  # Filter bars for this map
  map_bars <- bars_data %>% filter(state %in% sf_data$state)
  
  # Create base map with striped pattern for incarceration
  p <- ggplot() +
    # Use geom_sf_pattern for stripe density to represent incarceration
    geom_sf_pattern(
      data = sf_data,
      aes(
        fill = final_political,           # Political leaning (transformed)
        pattern_density = incarceration_intensity  # Incarceration rate
      ),
      pattern = 'stripe',              # Use diagonal stripes pattern
      pattern_color = "black",         # Black stripes
      pattern_alpha = INCARCERATION_STRIPE_ALPHA, # Configurable stripe opacity
      pattern_angle = 45,              # Diagonal stripes
      color = "white",                 # White borders between states
      size = 0.3                       # Border thickness
    )
  
  # Different legend settings based on map type
  if (show_legend) {
    # Main map with legends
    p <- p +
      # Red-blue gradient for political leaning
      scale_fill_gradient2(
        low = DEM_COLOR,          # Strong blue for Democratic
        mid = SWING_COLOR,        # Light purple for swing states (not white/grey)
        high = REP_COLOR,         # Strong red for Republican
        midpoint = 0,
        space = "Lab",            # Perceptually uniform color space
        name = "Political Leaning",
        breaks = c(-0.3, 0, 0.3),  # Add breaks to match labels
        labels = c("Democratic", "Swing", "Republican"),
        guide = guide_colorbar(
          title.position = "top",
          barwidth = 10,
          barheight = 0.5,
          title.hjust = 0.5
        )
      ) +
      # Stripe density for incarceration
      scale_pattern_density_continuous(
        range = c(INCARCERATION_DENSITY_MIN, INCARCERATION_DENSITY_MAX),
        name = "Incarceration Rate",
        breaks = c(0.1, 0.25, 0.4),
        labels = c("Low", "Medium", "High"),
        guide = guide_legend(
          override.aes = list(pattern_alpha = 0.8, fill = "gray50"),
          title.position = "top",
          title.hjust = 0.5
        )
      )
  } else {
    # Inset maps without legends
    p <- p +
      scale_fill_gradient2(
        low = DEM_COLOR,
        mid = SWING_COLOR,
        high = REP_COLOR,
        midpoint = 0,
        space = "Lab",
        guide = "none"
      ) +
      scale_pattern_density_continuous(
        range = c(INCARCERATION_DENSITY_MIN, INCARCERATION_DENSITY_MAX),
        guide = "none"
      )
  }
  
  # Add crime bars with 3D-like effect
  # First add shadow/outline
  p <- p +
    # Shadow effects for person crime bars
    geom_segment(
      data = map_bars,
      aes(x = centroid_x + x_offset, y = person_y_start, 
          xend = centroid_x + x_offset, yend = person_y_end),
      color = "black",
      alpha = 0.3,
      size = CRIME_BAR_WIDTH + 0.5
    ) +
    # Shadow effects for property crime bars
    geom_segment(
      data = map_bars,
      aes(x = centroid_x + x_offset, y = property_y_start, 
          xend = centroid_x + x_offset, yend = property_y_end),
      color = "black",
      alpha = 0.3,
      size = CRIME_BAR_WIDTH + 0.5
    ) +
    # Shadow effects for society crime bars
    geom_segment(
      data = map_bars,
      aes(x = centroid_x + x_offset, y = society_y_start, 
          xend = centroid_x + x_offset, yend = society_y_end),
      color = "black",
      alpha = 0.3,
      size = CRIME_BAR_WIDTH + 0.5
    )
  
  # Then add main colored bars
  p <- p +
    # Person crimes (bottom segment)
    geom_segment(
      data = map_bars,
      aes(x = centroid_x, y = person_y_start, xend = centroid_x, yend = person_y_end),
      color = PERSON_CRIME_COLOR,
      alpha = CRIME_BAR_ALPHA,
      size = CRIME_BAR_WIDTH
    ) +
    # Property crimes (middle segment)
    geom_segment(
      data = map_bars,
      aes(x = centroid_x, y = property_y_start, xend = centroid_x, yend = property_y_end),
      color = PROPERTY_CRIME_COLOR,
      alpha = CRIME_BAR_ALPHA,
      size = CRIME_BAR_WIDTH
    ) +
    # Society crimes (top segment)
    geom_segment(
      data = map_bars,
      aes(x = centroid_x, y = society_y_start, xend = centroid_x, yend = society_y_end),
      color = SOCIETY_CRIME_COLOR,
      alpha = CRIME_BAR_ALPHA,
      size = CRIME_BAR_WIDTH
    )
  
  # Add overall theming
  p <- p + theme_void() +
    theme(
      plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
      legend.position = if(show_legend) "bottom" else "none",
      legend.box = "horizontal",
      legend.margin = margin(10, 10, 10, 10),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
    )
  
  # Add title if provided
  if(!is.null(title)) {
    p <- p + ggtitle(title)
  }
  
  return(p)
}

# ================================================
# Create maps for main visualization and insets
# ================================================
# 
# main_map <- create_map(
#   continental_states,
#   crime_bars %>% filter(!state %in% c("Alaska", "Hawaii")),
#   show_legend = TRUE
# )
# 
# # Create Alaska and Hawaii maps without backgrounds
# alaska_map <- create_map(
#   alaska,
#   crime_bars %>% filter(state == "Alaska"),
#   "Alaska",
#   show_legend = FALSE
# )
# 
# hawaii_map <- create_map(
#   hawaii,
#   crime_bars %>% filter(state == "Hawaii"),
#   "Hawaii",
#   show_legend = FALSE
# )

# ================================================
# Create maps with fixed legend handling
# ================================================

# First create the main map with legends intact
main_map_with_legends <- create_map(
  continental_states,
  crime_bars %>% filter(!state %in% c("Alaska", "Hawaii")),
  show_legend = TRUE
)

# Extract all legends BEFORE removing them from the plot
map_legends <- cowplot::get_legend(main_map_with_legends)

# Then create versions without legends for plotting
main_map <- main_map_with_legends + theme(legend.position = "none")

# Debug Alaska/Hawaii political values more clearly
print(alaska$political_leaning)
print(alaska$final_political)
print(hawaii$political_leaning) 
print(hawaii$final_political)

# # Create inset maps with fixed coloring
# alaska_map <- create_map(
#   alaska,
#   crime_bars %>% filter(state == "Alaska"),
#   "Alaska",
#   show_legend = FALSE
# ) 
# 
# hawaii_map <- create_map(
#   hawaii,
#   crime_bars %>% filter(state == "Hawaii"),
#   "Hawaii",
#   show_legend = FALSE
# )

# ================================================
# Create explicit political leaning legend
# ================================================

political_legend <- ggplot() +
  # Create color squares for each political category
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = DEM_COLOR) +
  annotate("rect", xmin = 1, xmax = 2, ymin = 0, ymax = 1, fill = SWING_COLOR) +
  annotate("rect", xmin = 2, xmax = 3, ymin = 0, ymax = 1, fill = REP_COLOR) +
  
  # Add labels
  annotate("text", x = 0.5, y = 1.3, label = "Democratic", hjust = 0.5) +
  annotate("text", x = 1.5, y = 1.3, label = "Swing", hjust = 0.5) +
  annotate("text", x = 2.5, y = 1.3, label = "Republican", hjust = 0.5) +
  
  # Add title
  annotate("text", x = 1.5, y = 2, label = "Political Leaning", fontface = "bold") +
  
  # Theming
  theme_void() +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = NA, color = NA)
  ) +
  xlim(-0.5, 3.5) + ylim(0, 2.5)

# ================================================
# Create explicit incarceration rate legend
# ================================================

# ================================================
# Create improved incarceration rate legend
# ================================================

# Create a data frame with different incarceration rate levels
incarceration_levels <- data.frame(
  category = factor(c("Low", "Medium", "High"), 
                    levels = c("Low", "Medium", "High")),
  value = c(0.1, 0.25, 0.4),  # Matches your breaks in the main visualization
  x = c(1, 2, 3),
  y = 1
)

# Create a legend using the actual pattern functionality
incarceration_legend <- ggplot(incarceration_levels, aes(x = x, y = y)) +
  # Create rectangles with the exact same pattern settings as the main map
  geom_rect_pattern(
    aes(
      xmin = x - 0.4, xmax = x + 0.4,
      ymin = y - 0.4, ymax = y + 0.4,
      pattern_density = value
    ),
    pattern = 'stripe',
    pattern_color = "black",
    pattern_angle = 45,
    pattern_alpha = INCARCERATION_STRIPE_ALPHA,
    fill = "gray90",
    color = "black",
    size = 0.2
  ) +
  # Apply the same density scaling as in the main map
  scale_pattern_density_continuous(
    range = c(INCARCERATION_DENSITY_MIN, INCARCERATION_DENSITY_MAX),
    guide = "none"
  ) +
  # Add labels
  geom_text(
    aes(label = category),
    y = 0.3,
    vjust = 1
  ) +
  # Add title
  annotate("text", x = 2, y = 1.8, label = "Incarceration Rate", fontface = "bold") +
  # Clean styling
  theme_void() +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = NA, color = NA)
  ) +
  xlim(0, 4) + ylim(0, 2) +
  coord_fixed()

# ================================================
# Fix Alaska and Hawaii with direct color assignment
# ================================================

# Function to create state insets with correct colors
create_state_inset <- function(state_data, crime_bar_data, title) {
  # Get political value for determining color
  pol_val <- state_data$political_leaning
  
  # Determine appropriate color directly
  state_color <- if(pol_val < 0.05) REP_COLOR else 
    if(pol_val > -0.05) DEM_COLOR else
      SWING_COLOR
  
  # Filter bar data for this state
  state_bars <- crime_bar_data %>% filter(state == state_data$state)
  
  # Create map with direct fill color
  p <- ggplot() +
    # Create base map with correct political color
    geom_sf(
      data = state_data,
      fill = state_color,
      color = "white",
      size = 0.3
    ) +
    # Add stripe pattern for incarceration
    geom_sf_pattern(
      data = state_data,
      aes(pattern_density = incarceration_intensity),
      pattern = 'stripe',
      pattern_color = "black", 
      pattern_alpha = INCARCERATION_STRIPE_ALPHA,
      pattern_angle = 45,
      fill = state_color,  # Use direct color
      color = "white",
      size = 0.3
    ) +
    scale_pattern_density_continuous(
      range = c(INCARCERATION_DENSITY_MIN, INCARCERATION_DENSITY_MAX),
      guide = "none"
    ) +
    # Add crime bars (same as main function)
    # First shadows
    geom_segment(
      data = state_bars,
      aes(x = centroid_x + x_offset, y = person_y_start, 
          xend = centroid_x + x_offset, yend = person_y_end),
      color = "black", alpha = 0.3, size = CRIME_BAR_WIDTH + 0.5
    ) +
    geom_segment(
      data = state_bars,
      aes(x = centroid_x + x_offset, y = property_y_start, 
          xend = centroid_x + x_offset, yend = property_y_end),
      color = "black", alpha = 0.3, size = CRIME_BAR_WIDTH + 0.5
    ) +
    geom_segment(
      data = state_bars,
      aes(x = centroid_x + x_offset, y = society_y_start, 
          xend = centroid_x + x_offset, yend = society_y_end),
      color = "black", alpha = 0.3, size = CRIME_BAR_WIDTH + 0.5
    ) +
    # Then main colored bars
    geom_segment(
      data = state_bars,
      aes(x = centroid_x, y = person_y_start, xend = centroid_x, yend = person_y_end),
      color = PERSON_CRIME_COLOR, alpha = CRIME_BAR_ALPHA, size = CRIME_BAR_WIDTH
    ) +
    geom_segment(
      data = state_bars,
      aes(x = centroid_x, y = property_y_start, xend = centroid_x, yend = property_y_end),
      color = PROPERTY_CRIME_COLOR, alpha = CRIME_BAR_ALPHA, size = CRIME_BAR_WIDTH
    ) +
    geom_segment(
      data = state_bars,
      aes(x = centroid_x, y = society_y_start, xend = centroid_x, yend = society_y_end),
      color = SOCIETY_CRIME_COLOR, alpha = CRIME_BAR_ALPHA, size = CRIME_BAR_WIDTH
    ) +
    theme_void() +
    ggtitle(title)
  
  return(p)
}

# Create Alaska and Hawaii with correct colors
alaska_map <- create_state_inset(
  alaska, 
  crime_bars %>% filter(state == "Alaska"),
  NULL
)

hawaii_map <- create_state_inset(
  hawaii, 
  crime_bars %>% filter(state == "Hawaii"),
  "Hawaii"
)

# ================================================
# Convert all legends to grobs
# ================================================

#crime_legend_grob <- ggplotGrob(crime_legend)
political_legend_grob <- ggplotGrob(political_legend)
incarceration_legend_grob <- ggplotGrob(incarceration_legend)

# ================================================
# Create explicit crime bars legend
# ================================================

# Create a clear crime bars legend - using annotate instead of geom_segment
crime_legend <- ggplot() +
  # Person segment
  annotate("segment", x = 1, y = 0, xend = 1, yend = 1,
           color = PERSON_CRIME_COLOR, size = 10) +
  # Property segment
  annotate("segment", x = 1, y = 1, xend = 1, yend = 2,
           color = PROPERTY_CRIME_COLOR, size = 10) +
  # Society segment
  annotate("segment", x = 1, y = 2, xend = 1, yend = 3,
           color = SOCIETY_CRIME_COLOR, size = 10) +
  # Add labels
  annotate("text", x = 1.6, y = 0.5, label = "Person Crimes", hjust = 0) +
  annotate("text", x = 1.6, y = 1.5, label = "Property Crimes", hjust = 0) +
  annotate("text", x = 1.6, y = 2.5, label = "Society Crimes", hjust = 0) +
  # Title
  annotate("text", x = 1, y = 3.5, label = "Crime Categories", fontface = "bold") +
  # Theming
  theme_void() +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = NA, color = NA)
  ) +
  xlim(0, 4) + ylim(0, 4) +
  coord_fixed()

# Extract legends
crime_legend_grob <- ggplotGrob(crime_legend)
#political_legend_grob <- get_legend(main_map)

# ================================================
# Combine components into final visualization
# ================================================

final_viz <- ggdraw() +
  # Main map
  draw_plot(main_map, x = 0, y = 0.01, width = 1, height = 1) +
  
  # # Properly sized insets without backgrounds
  draw_plot(alaska_map, x = 0.75, y = 0.4, width = 1, height = 1) +
  draw_plot(hawaii_map, x = -0.75, y = -0.077, width = 1, height = 1) +
  
  # Add distinct legends with clear labels
  draw_plot(crime_legend_grob, 0.7, 0.1, 0.35, 0.25) +
  draw_plot(political_legend_grob, x = 0, y = 0, width = 1, height = 0.15) +
  draw_plot(incarceration_legend_grob, x = 0.75, y = 0.33, width = 0.33, height = 0.22) +
  
  # Title
  draw_label(
    "The Disconnect Between Political Rhetoric and Crime Outcomes",
    x = 0.5, y = 0.97,
    hjust = 0.5, size = 18, fontface = "bold"
  ) +
  
  # Subtitle
  draw_label(
    "Political leaning (red-blue), incarceration rates (stripe density), and crime rates (stacked bars)",
    x = 0.5, y = 0.93,
    hjust = 0.5, size = 12
  )

# Save the final visualization
dir.create("output", showWarnings = FALSE)
ggsave(
  "output/crime_politics_final.png",
  final_viz,
  width = 18,
  height = 12,
  dpi = 300
)

print("Final visualization saved to output/crime_politics_final.png")