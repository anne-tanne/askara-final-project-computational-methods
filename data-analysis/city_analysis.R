# Load necessary libraries
library(dplyr)
library(readr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(maps)
library(hrbrthemes)

hrbrthemes::import_roboto_condensed()


# Load map data for Switzerland
switzerland_map <- map_data("world", region = "Switzerland")

# Set the output directory for saving visuals
output_dir <- "output/visuals/location-analysis/city-analysis"

# Load datasets
df_city_counts_german <- read_csv('output/data/df_city_count_data_german.csv')
df_city_counts_french <- read_csv('output/data/df_city_count_data_french.csv')

# Function to calculate proportions by region
calc_proportions <- function(df) {
  df |>
    group_by(Region) |>
    summarise(Total_Count = sum(Count)) |>
    mutate(Proportion = Total_Count / sum(Total_Count))
}

# Calculate proportions for German and French city data
prop_german <- calc_proportions(df_city_counts_german)
prop_french <- calc_proportions(df_city_counts_french)

# Output proportions for German and French city data
prop_german
prop_french

# Function for performing a Chi-squared test
chi_test <- function(df1, df2) {
  # Creating a contingency table
  table <- rbind(df1$Total_Count, df2$Total_Count)
  colnames(table) <- c("German Region", "French Region", "Italian Region")
  rownames(table) <- c("German Dataset", "French Dataset")
  
  # Performing Chi-squared test
  chisq.test(table)
}

# Perform the test
chi_test_result <- chi_test(prop_german, prop_french)
chi_test_result

# Add 'Edition' column to proportions data
prop_german$Edition <- "German"
prop_french$Edition <- "French"


# Function to update region names
update_region_names <- function(df) {
  df$Region <- factor(df$Region,
                      levels = c(1, 2, 3),
                      labels = c("German Switzerland", "French Switzerland", "Ticino"))
  return(df)
}

# Update region names in proportions data
prop_german <- update_region_names(prop_german)
prop_french <- update_region_names(prop_french)

# Combine proportions data with updated region names
combined_props <- rbind(prop_german, prop_french)

# Create a bar plot for German and French city mentions by region

german_french_bar_plot <- ggplot(combined_props, aes(x = Region, y = Proportion, fill = Edition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("German" = "lightblue", "French" = "lightcoral")) +
  labs(title = "Proportion of City Mentions by Region in German and French Editions",
       x = "Region",
       y = "Proportion") +
  theme_ipsum()  # Apply the theme_ipsum() from hrbrthemes


# Save the bar plot as an image
ggsave(file.path(output_dir, "german_french_bar_plot.png"), german_french_bar_plot, height = 6, width = 10)

# Processing, plotting, and saving bar charts for the top 10 cities mentioned in German and French Editions
# German Edition
top_10_cities_german <- df_city_counts_german |>
  arrange(desc(Count)) |>
  head(10)

# Create a bar plot with cities ordered from high to low counts
german_bar_plot <- ggplot(top_10_cities_german, aes(x = reorder(City, -Count), y = Count, fill = City)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rep('skyblue', 10)) +
  labs(title = 'Top 10 Cities Mentioned in "20 Minuten"', x = "City", y = "Count") +
  theme_ipsum() +
  theme(legend.position = "none") +
  geom_text(aes(label = Count), vjust = -0.3)


# Save the German bar plot as an image
ggsave(file.path(output_dir, "top_10_cities_german.png"), german_bar_plot)

# French Edition
top_10_cities_french <- df_city_counts_french |>
  arrange(desc(Count)) |>
  head(10)

# Create a bar plot with cities ordered from high to low counts
french_bar_plot <- ggplot(top_10_cities_french, aes(x = reorder(City, -Count), y = Count, fill = City)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rep('lightcoral', 10)) +
  labs(title = 'Top 10 Cities Mentioned in "20 Minutes"', x = "City", y = "Count") +
  theme_ipsum() +
  theme(legend.position = "none") +
  geom_text(aes(label = Count), vjust = -0.3)

# Save the French bar plot as an image
ggsave(file.path(output_dir, "top_10_cities_french.png"), french_bar_plot)

# Plotting and saving scatter plots for city counts in German and French Editions
# German Edition
german_scatter_plot <- ggplot(df_city_counts_german, aes(x = Longitude, y = Latitude, size = Count)) +
  geom_point(alpha = 0.7, color = "skyblue") +
  theme_ipsum() +
  labs(title = "Frequency of Cities Mentioned in German Edition", x = "Longitude", y = "Latitude") +
  coord_fixed(ratio = 1.3, xlim = c(5, 10), ylim = c(45, 48))

# Save the scatter plot as an image
ggsave(file.path(output_dir, "scatter_plot_german.png"), german_scatter_plot)

german_scatter_plot_map <- ggplot() +
  geom_polygon(data = switzerland_map, aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  geom_point(data = df_city_counts_german, aes(x = Longitude, y = Latitude, size = Count), alpha = 0.7, color = "skyblue") +
  theme_ipsum() +
  labs(title = "Frequency of Cities Mentioned in German Edition", x = "Longitude", y = "Latitude") +
  coord_fixed(ratio = 1.3, xlim = c(5.8, 10.5), ylim = c(45.8, 47.8)) # Further narrowed xlim and ylim

# Save the plot
ggsave(file.path(output_dir, "scatter_plot_german_map.png"), german_scatter_plot_map, width = 10, height = 8)


# French Edition
french_scatter_plot <- ggplot(df_city_counts_french, aes(x = Longitude, y = Latitude, size = Count)) +
  geom_point(alpha = 0.7, color = "lightcoral") +
  theme_ipsum() +
  labs(title = "Frequency of Cities Mentioned in French Edition", x = "Longitude", y = "Latitude") +
  coord_fixed(ratio = 1.3, xlim = c(5, 10), ylim = c(45, 48))

# Save the scatter plot as an image
ggsave(file.path(output_dir, "scatter_plot_french.png"), french_scatter_plot)

french_scatter_plot_map <- ggplot() +
  geom_polygon(data = switzerland_map, aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  geom_point(data = df_city_counts_french, aes(x = Longitude, y = Latitude, size = Count), alpha = 0.7, color = "lightcoral") +
  theme_minimal() +
  labs(title = "Frequency of Cities Mentioned in French Edition", x = "Longitude", y = "Latitude") +
  coord_fixed(ratio = 1.3, xlim = c(5.8, 10.5), ylim = c(45.8, 47.8)) # Further narrowed xlim and ylim

# Save the plot
ggsave(file.path(output_dir, "scatter_plot_french_map.png"), french_scatter_plot_map, width = 10, height = 8)


# Creating and saving interactive maps with leaflet and plotly

# German Edition
switzerland_map_german <- leaflet(df_city_count_data_german) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  setView(lng = 8.2275, lat = 46.8182, zoom = 7) |>
  addCircleMarkers(~Longitude, ~Latitude, radius = ~Count / 5, color = 'skyblue', fill = TRUE, popup = ~paste(City, Count))

# Save the interactive map as an HTML file
saveWidget(switzerland_map_german, file.path(output_dir, "html/switzerland_map_german.html"), selfcontained = TRUE)

# French Edition
switzerland_map_french <- leaflet(df_city_count_data_french) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  setView(lng = 8.2275, lat = 46.8182, zoom = 7) |>
  addCircleMarkers(~Longitude, ~Latitude, radius = ~Count / 5, color = 'lightcoral', fill = TRUE, popup = ~paste(City, Count))

# Save the interactive map as an HTML file
saveWidget(switzerland_map_french, file.path(output_dir, "html/switzerland_map_french.html"), selfcontained = TRUE)
