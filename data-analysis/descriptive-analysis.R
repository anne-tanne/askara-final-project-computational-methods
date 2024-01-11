# Load necessary libraries
library(readr)
library(ggplot2)

# Reading the CSV files
# Reading the CSV file for the German data into df_german dataframe
df_german <- read_csv('../scraping/data/extractor_all_articles_20minuten.csv')
# Reading the CSV file for the French data into df_french dataframe
df_french <- read_csv('../scraping/data/extractor_all_articles_20minutes.csv')

# Calculating the number of articles in each dataframe
# Get the number of rows in the df_german dataframe, which represents the number of German articles
german_articles_count <- nrow(df_german)
# Get the number of rows in the df_french dataframe, which represents the number of French articles
french_articles_count <- nrow(df_french)

# Printing the number of articles in each dataframe
print(paste("Number of articles in df_german:", german_articles_count))
print(paste("Number of articles in df_french:", french_articles_count))

# Creating a bar plot with ggplot2
# Create a new dataframe 'data' to store the language and the number of articles
data <- data.frame(
  Languages = c('German', 'French'),
  Number_of_Articles = c(german_articles_count, french_articles_count)
)

# Create a bar plot using ggplot2 to visualize the number of articles in each language
article_plot <- ggplot(data, aes(x = Languages, y = Number_of_Articles, fill = Languages)) +
  geom_bar(stat = "identity", show.legend = FALSE) + # Create a bar plot with identity mapping
  scale_fill_manual(values = c('skyblue', 'lightcoral')) + # Set fill colors for the bars
  labs(title = 'Number of Articles in German and French DataFrames',
       x = 'Languages',
       y = 'Number of Articles') + # Set plot titles and labels
  theme_minimal() + # Use a minimal theme for the plot
  geom_text(aes(label = Number_of_Articles), vjust = -0.3) # Add labels on top of the bars

# Save the plot as an image file
ggsave("output/visuals/descriptive-analysis/distribution_articles_bar_plot.png", article_plot, width = 10, height = 6)
