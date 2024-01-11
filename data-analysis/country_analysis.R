# Load necessary libraries
library(dplyr)
library(readr)
library(stringr)
library(rnaturalearth)   # For accessing map data
library(rnaturalearthdata)  # For additional map data
library(hrbrthemes)  # Load the hrbrthemes package


output_dir <- "output/visuals/location-analysis/country-analysis"



# Load datasets
df_country_counts_german <- read_csv('output/data/df_country_count_data_german.csv')
df_country_counts_french <- read_csv('output/data/df_country_count_data_french.csv')

df_country_counts_german


# Getting world countries data in 'sf' (spatial) format
world <- ne_countries(scale = "medium", returnclass = "sf")

# Aggregating French data to ensure each country has a unique count
df_country_count_data_french_aggregated <- df_country_counts_french |>
  group_by(ISO2) |>
  summarise(Count = sum(Count))

df_country_count_data_german_aggregated <- df_country_counts_german |>
  group_by(ISO2) |>
  summarise(Count = sum(Count))


# Joining the German data with world spatial data
german_data <- world |> 
  left_join(df_country_count_data_german_aggregated, by = c("iso_a2" = "ISO2"))

# Joining the aggregated French data with world spatial data
french_data <- world |>
  left_join(df_country_count_data_french_aggregated, by = c("iso_a2" = "ISO2"))

german_choropleth <- ggplot(german_data) +
  geom_sf(aes(fill = Count)) +
  scale_fill_viridis_c() +
  labs(title = "Frequency of Countries Mentioned in German Edition") +
  theme_ipsum()  # Apply the theme_ipsum() from hrbrthemes
# Save the choropleth map
ggsave(file.path(output_dir, "german_edition_count_map.png"), german_choropleth)


# Plotting and saving choropleth map for French Edition
french_choropleth <- ggplot(french_data) +
  geom_sf(aes(fill = Count)) +
  scale_fill_viridis_c() +
  labs(title = "Frequency of Countries Mentioned in German Edition") +
  theme_ipsum()  # Apply the theme_ipsum() from hrbrthemes
# Save the choropleth map
ggsave(file.path(output_dir, "french_edition_count_map.png"), french_choropleth)

# Selecting top ten countries for the French edition
top_french_countries <- df_country_counts_french |>
  filter(!is.na(Count) & !is.na(CountryName)) |>  # Exclude rows with NA in Count or CountryName
  arrange(desc(Count)) |>
  top_n(10, Count)

# Selecting top ten countries for the German edition
top_german_countries <- df_country_counts_german |>
  arrange(desc(Count)) |>
  top_n(10, Count)

# Bar chart for the French edition

country_name_mapping_french <- c(
  "suisse" = "Switzerland",
  "etats-unis" = "United States",
  "france" = "France",
  "royaume-uni" = "United Kingdom",
  "russie" = "Russia",
  "allemagne" = "Germany",
  "italie" = "Italy",
  "ukraine" = "Ukraine",
  "belgique" = "Belgium",
  "espagne" = "Spain"
)


french_bar_chart <- ggplot(top_french_countries, aes(x = reorder(CountryName, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  scale_x_discrete(labels = country_name_mapping_french) +  # Apply translated labels here
  labs(title = "Top 10 Countries in French Edition", x = "Country", y = "Count") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for vertical bars

# Save the French bar chart with English labels
ggsave(file.path(output_dir, "french_edition_top_countries_bar_chart.png"), french_bar_chart)

country_name_mapping_german <- c(
  "schweiz" = "Switzerland",
  "vereinigte staaten von amerika" = "United States",
  "frankreich" = "France",
  "vereinigtes konigreich" = "United Kingdom",
  "russland" = "Russia",
  "deutschland" = "Germany",
  "italien" = "Italy",
  "ukraine" = "Ukraine",
  "brasilien" = "Brasil",
  "china" = "China", 
  "israel" = "Israel"
)

german_bar_chart <- ggplot(top_german_countries, aes(x = reorder(CountryName, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  scale_x_discrete(labels = country_name_mapping_german) +  # Apply translated labels here
  labs(title = "Top 10 Countries in German Edition", x = "Country", y = "Count") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for vertical bars

# Save the German bar chart with English labels
ggsave(file.path(output_dir, "german_edition_top_countries_bar_chart.png"), german_bar_chart)




# Define constants for analysis
total_articles_german <- 894
total_articles_french <- 1060


neighbouring_countries_iso <- c("DE", "FR", "IT", "AT", "LI") # ISO2 codes
neighbours_german <- c("deutschland", "oesterreich", "frankreich", "italien", "liechtenstein")
neighbours_french <- c("allemagne", "autriche", "france", "italie", "liechtenstein")

# Function to analyze country mentions in articles
analyze_country_mentions <- function(df, switzerland_name, neighbours) {
  data <- list(
    NoCountry = sum(df$Mentioned_Countries == "set()"),
    OnlySwitzerland = sum(df$Mentioned_Countries == paste0("{'", switzerland_name, "'}")),
    OnlyNonSwitzerland = sum(df$Mentioned_Countries != "set()" & !grepl(switzerland_name, df$Mentioned_Countries)),
    OnlyNeighbours = sum(grepl(paste(neighbours, collapse = "|"), df$Mentioned_Countries) & !grepl(switzerland_name, df$Mentioned_Countries)),
    NonSwitzerlandNonNeighbours = sum(!grepl(paste(c(switzerland_name, neighbours), collapse = "|"), df$Mentioned_Countries)),
    SwitzerlandOrNeighbours = sum(grepl(paste(c(switzerland_name, neighbours), collapse = "|"), df$Mentioned_Countries))
  )
  return(data)
}

# Apply analysis of country mentions in datasets
results_german <- analyze_country_mentions(df_german, "schweiz", neighbours_german)
results_french <- analyze_country_mentions(df_french, "suisse", neighbours_french)


results_german
