# Load necessary libraries
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(rvest)

# Define the path to the directory containing the HTML files
directory_path <- "HTML/20minuti/"

# List all HTML files in the directory
file_paths <- list.files(directory_path, pattern = "\\.html$", full.names = TRUE)

file_paths

# Define a function to process each HTML file
process_file <- function(file_path) {
  # Provide feedback that the file is being processed
  message("Processing file: ", basename(file_path))
  
  html_content <- read_html(file_path)
  
  # Extract title, excluding content from any nested elements like 'span'
  title <- html_content |>
    html_nodes(xpath = '//h1/text()') |>
    html_text(trim = TRUE) |>
    str_squish()
  
  # Extract subtitle from the 'span' tag within the 'h1' tag
  subtitle <- html_content |>
    html_nodes('h1 > span') |>
    html_text(trim = TRUE) |>
    str_squish()
  
  # Extract and combine main content
  main_content <- html_content |>
    html_nodes('.block_html.flex-wrap p') |>
    html_text(trim = TRUE) |>
    str_squish()
  main_content_combined <- paste(main_content, collapse=" ")
  
  # Return a dataframe
  tibble(Title = title, Header = subtitle, Content = main_content_combined)
}


# Process all files and combine the results into a single dataframe
all_articles_df <- map_df(file_paths, process_file)

# Print the combined dataframe
print(all_articles_df)

view(all_articles_df)

# Write the dataframe to a CSV file in the same directory as the script
csv_path <- "data/extractor_all_articles_20minuti.csv"
write.csv(all_articles_df, csv_path, row.names = FALSE)

# Return the path to the CSV file as output
csv_path
