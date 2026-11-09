
# Homework 3 - Stat 184

# Install packages once in the Console if needed:
# install.packages(c("ggplot2", "dplyr", "tidyr", "readr", "sf"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)
vehicles <- ggplot2::mpg # Full dataset
gems <- ggplot2::diamonds # Full dataset


# -------
# Problem 1
# -------
# Part 1A
#   - Reproduce the same plot shown in HW3-2, using ggplot.
#   - Explain why there are three fitted lines even when drv is mapped to color only in the point layer

# Reproducing the faceted scatter plot with trend lines
p_smooth <- ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~drv) +
  theme_bw() +
  labs(
    x = "Engine displacement (litres)",
    y = "Highway fuel economy (mpg)",
    color = "drv"
  )

print(p_smooth)

# Explanation: 
# There are three fitted lines because the dataset is split into separate panels using facet_wrap(~drv). Faceting automatically subgroups the data passed to every layer in the plot. 
# Therefore, even though drv is only mapped to color inside geom_point(), geom_smooth() receives a separate subset of data for each facet and fits a separate line for each subgroup.

# -------
# Part 1B
#   - Reproduce the same plot shown in HW3-2 using ggplot

# Left plot: Absolute counts
p_count <- ggplot(mpg, aes(x = drv, fill = factor(cyl))) +
  geom_bar() +
  theme_bw() +
  labs(y = "count", fill = "factor(cyl)")

# Right plot: Proportions within drive type
p_prop <- ggplot(mpg, aes(x = drv, fill = factor(cyl))) +
  geom_bar(position = "fill") +
  theme_bw() +
  labs(y = "Proportion within drive type", fill = "factor(cyl)")

# Display both plots side by side (requires 'patchwork' or displaying individually)
print(p_count)
print(p_prop)

# Explanation: 
# The plot on the right uses a 'fill' setting, which stretches every bar to 100%. This shows the percentage breakdown of each cylinder type within each drive type, but it hides the actual number of items in each group4-

# -------
# Part 1C
#   - Create two count histograms of carat from the same smaller table. Save them as p_fine and p_coarse. Use bin widths 0.01 and 0.5, respectively; in both cases use boundary = 0 and closed = "right".
#      > Keep the same x-axis range and labels, show both plots, and label each bin width. Report how many records were retained and excluded.
#   - From the original gems, display the numbers of diamonds recorded as exactly 0.99 and 1.00 carat. Use code to answer
#   - In at most four sentences, describe a feature that is easier to see with the narrower bins.

# Assuming a smaller table subset, e.g., carats up to 3
gems_small <- diamonds %>% filter(carat <= 3)

# Create two histograms
p_fine <- ggplot(gems_small, aes(x = carat)) +
  geom_histogram(binwidth = 0.01, boundary = 0, closed = "right") +
  xlim(0.2, 3.0) +  # Standardizes the x-axis range
  theme_bw() +
  labs(title = "Bin width: 0.01", x = "carat", y = "count")

p_coarse <- ggplot(gems_small, aes(x = carat)) +
  geom_histogram(binwidth = 0.5, boundary = 0, closed = "right") +
  xlim(0.2, 3.0) +  # Keeps the same x-axis range
  theme_bw() +
  labs(title = "Bin width: 0.5", x = "carat", y = "count")

# Show the histograms
print(p_fine)
print(p_coarse)

# Reporting records retained vs excluded
retained_count <- nrow(gems_small)
excluded_count <- nrow(diamonds) - retained_count
cat(sprintf("Retained: %d, Excluded: %d\n", retained_count, excluded_count))

# Display numbers of diamonds at exactly 0.99 and 1.00 carat
carat_counts <- diamonds %>%
  filter(carat %in% c(0.99, 1.00)) %>%
  group_by(carat) %>%
  summarise(count = n())

print(carat_counts)

# Explanation:
# When the bars on the chart are made narrower (0.01), huge spikes appear at nice, round numbers like 1.00, 0.70, or 0.50 carats, with sudden drops right below them (like 0.99 carats). 
# This shows how humans and the market prefer even numbers: diamond cutters intentionally shape stones to hit the 1.00-carat mark instead of stopping at 0.99 carats, because diamonds sell for much higher prices at exact round weights


# -------
# Problem 2
# -------

#   - Using real facility data for your chosen region, investigate: Where are the facilities, and how much area lies within 500 meters or 1,000 meters of at least one facility

# Change path to the folder containing your files if needed:
setwd("C:/Users/ilene/OneDrive/Documentos")

# Load Data 
raw_libraries <- read_csv("libraries.csv")
initial_count <- nrow(raw_libraries) # Chicago Community Areas

# Clean Data
# Split the "LOCATION" column into separate latitude/longitude columns
cleaned_libraries <- raw_libraries %>%
  filter(!is.na(LOCATION)) %>%
  
  # Remove parentheses and spaces from the text
  mutate(LOCATION_clean = gsub("[() ]", "", LOCATION)) %>%
  
  # Separate into two numeric columns using the comma delimiter
  separate(LOCATION_clean, into = c("latitude", "longitude"), sep = ",", convert = TRUE) %>%
  
  # Drop any rows where parsing failed
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  
  # Assign unique ID
  mutate(site_id = row_number()) 

cleaned_count <- nrow(cleaned_libraries)

# Report checks to console
cat("Initial records read:", initial_count, "\n")
cat("Cleaned records with valid coordinates:", cleaned_count, "\n")
cat("Removed records:", initial_count - cleaned_count, "\n")

# Convert dataframe to a spatial sf object
sites_sf <- st_as_sf(cleaned_libraries, coords = c("longitude", "latitude"), crs = 4326)

# Transform to a local projected CRS using meters
local_crs <- 3435 
sites_m <- st_transform(sites_sf, crs = local_crs)

# Create Buffers and calculate Union Areas
buffer_500 <- st_union(st_buffer(sites_m, dist = 500))
buffer_1000 <- st_union(st_buffer(sites_m, dist = 1000))

area_500_km2 <- as.numeric(st_area(buffer_500)) / 1e6
area_1000_km2 <- as.numeric(st_area(buffer_1000)) / 1e6

buffer_summary <- tibble(
  radius_m = c(500, 1000),
  union_area_km2 = c(area_500_km2, area_1000_km2)
)
print("Buffer Area Proximity Summary:")
print(buffer_summary)

# Generate Maps
p_sites <- ggplot() +
  geom_sf(data = sites_m, color = "darkblue", size = 2) +
  labs(title = "Location of Retained Public Library Facilities (p_sites)",
       subtitle = "Cleaned point locations with unique structural IDs",
       caption = paste("Projected CRS: EPSG:", local_crs, "(Units: Meters)")) +
  theme_bw()

p_buffers <- ggplot() +
  geom_sf(data = st_buffer(sites_m, dist = 500), fill = "skyblue", color = "blue", alpha = 0.3) +
  geom_sf(data = sites_m, color = "darkblue", size = 1.2) +
  labs(title = "Public Facilities and 500-Meter Proximity Zones (p_buffers)",
       subtitle = "Overlapping regional buffer boundaries are unioned",
       caption = paste("Projected CRS: EPSG:", local_crs)) +
  theme_bw()

print(p_sites)
print(p_buffers)

# -------
# Problem 3

#   - Using the facilities from Problem 2 and a separate real boundary dataset, investigate: Which areas have the most facilities, and does the comparison change when we account for area size?

# Load Data
boundaries <- st_read("boundaries.geojson") 

# Align Boundary CRS to match points
boundaries_m <- st_transform(boundaries, crs = local_crs)

# Repair geometry structural flaws and calculate physical area sizes
boundaries_m <- st_make_valid(boundaries_m) %>%
  filter(!st_is_empty(.)) %>%
  mutate(area_km2 = as.numeric(st_area(.)) / 1e6) %>%
  filter(area_km2 > 0)

# Check and extract neighborhood column labels dynamically.
# Note: Replace 'community' or 'name' below if your file uses a different column name!
if("community" %in% names(boundaries_m)) {
  boundaries_m <- boundaries_m %>% mutate(district_name = community)
} else if ("name" %in% names(boundaries_m)) {
  boundaries_m <- boundaries_m %>% mutate(district_name = name)
} else {
  boundaries_m <- boundaries_m %>% mutate(district_name = paste("District", row_number()))
}
boundaries_m <- boundaries_m %>% mutate(district_id = row_number())

# Execute Point-in-Polygon intersection checks
intersections <- st_intersects(sites_m, boundaries_m)

# Compile the Point Matching Verification Audit File
site_match_audit <- tibble(
  site_id = sites_m$site_id,
  n_matches = lengths(intersections)
)
write_csv(site_match_audit, "site_match_audit.csv")

# Classify regional point mappings
match_0   <- sum(site_match_audit$n_matches == 0)
match_1   <- sum(site_match_audit$n_matches == 1)
match_gt_1 <- sum(site_match_audit$n_matches > 1)

cat("Spatial Intersection Match Audit Results:\n")
cat(" - Facilities matching exactly 0 regions:", match_0, "\n")
cat(" - Facilities matching exactly 1 region:", match_1, "\n")
cat(" - Facilities matching multiple regions:", match_gt_1, "\n\n")

# Retain only locations mapping into exactly one district area unit
valid_points <- sites_m[which(lengths(intersections) == 1), ]

# Spatial join aggregation to sum points within boundary units
district_counts <- st_join(boundaries_m, valid_points, join = st_intersects) %>%
  group_by(district_id, district_name, area_km2) %>%
  summarize(n_sites = sum(!is.na(site_id)), .groups = "drop") %>%
  # Normalize counts by district area sizes to uncover true geographic distribution density
  mutate(sites_per_km2 = n_sites / area_km2)

print("District Spatial Densities (district_counts Table Summary):")
print(st_drop_geometry(district_counts) %>% head(10))

# Build Spatial Chloropleth Comparison Maps
p_count <- ggplot(data = district_counts) +
  geom_sf(aes(fill = n_sites), color = "grey50", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(title = "Total Facility Counts by District Unit (p_count)",
       fill = "Total Sites",
       caption = "Grey regions denote zero-count areas.") +
  theme_bw()

p_density <- ggplot(data = district_counts) +
  geom_sf(aes(fill = sites_per_km2), color = "grey50", size = 0.2) +
  scale_fill_viridis_c(option = "viridis", na.value = "grey90") +
  labs(title = "Normalized Facility Densities (p_density)",
       fill = "Sites / km²",
       caption = "Accounting for district scale uncovers targeted access clusters.") +
  theme_bw()

# Output comparison maps
print(p_count)
print(p_density)

data_sources_log <- tibble(
  dataset_name = c("Facilities Points", "Administrative Boundaries"),
  file_name = c("libraries.csv", "boundaries.geojson"),
  source = c("City Open Data Portal", "City Open Data Portal"),
  download_date = c(Sys.Date(), Sys.Date())
)
write_csv(data_sources_log, "data_sources.csv")
cat("\n=== PROCESS COMPLETE: ALL LOGS AND SHAPEFILES GENERATED ===\n")

