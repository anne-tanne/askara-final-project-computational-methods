library(readr)
library(dplyr)
library(stringr)
library(purrr)

# Define the path to the directory containing the HTML files
directory_path_fr <- "HTML/20minutes/"
directory_path_de <- "HTML/20minuten/"

# List all HTML files in the directory
file_paths_fr <- list.files(directory_path_fr, pattern = "\\.html$", full.names = TRUE)
file_paths_de <- list.files(directory_path_de, pattern = "\\.html$", full.names = TRUE)

# Define a function to process each HTML file
process_file <- function(file_path) {
  # Provide feedback that the file is being processed
  message("Processing file: ", basename(file_path))
  
  # Use tryCatch to handle errors and continue processing other files
  tryCatch({
    html_content <- read_html(file_path)
    
    # Extract the genre from the <span> within the <h2> tag and remove the colon
    genre <- html_content|>
      html_nodes(xpath = '//h2[not(@class)]/span[not(@class)]')|>
      html_text(trim = TRUE)|>
      str_squish()|>
      str_remove(":") # Remove the colon
    genre <- ifelse(length(genre) > 0, genre, NA)
    
    # Extract the title from the <h2> tag, excluding any nested <span> tags
    title <- html_content|>
      html_nodes(xpath = '//h2[not(@class)]/text()')|>
      html_text(trim = TRUE)|>
      str_squish()
    title <- ifelse(length(title) > 0, title, NA)
    
    # Extract the main content from the <p> tags within the class 'Article_elementTextblockarray__1gb_B'
    main_content <- html_content|>
      html_nodes('.Article_elementTextblockarray__1gb_B p')|>
      html_text(trim = TRUE)|>
      str_squish()
    main_content_combined <- ifelse(length(main_content) > 0, paste(main_content, collapse=" "), NA)
    
    # Check if the content is "NA" or "Page non trouvée" and filter it out
    if (main_content_combined == "NA" || main_content_combined == "Page non trouvée") {
      return(NULL) # Skip this entry
    }
    
    # Create a dataframe with the extracted information
    tibble(Title = title, Header = genre, Content = main_content_combined)
  }, error = function(e) {
    # If an error occurs, print a message and return NULL to skip this entry
    message("Error processing file: ", basename(file_path), "\nError: ", e)
    return(NULL)
  })
}

# Process all files and combine the results into a single dataframe
articles_20_minutes <- map_df(file_paths_fr, process_file)
articles_20_minuten <- map_df(file_paths_de, process_file)


# Write the dataframe to a CSV file in the same directory as the script
csv_path_fr <- "data/extractor_all_articles_20minutes.csv"
csv_path_de <- "data/extractor_all_articles_20minuten.csv"

write.csv(articles_20_minutes, csv_path_fr, row.names = FALSE)
write.csv(articles_20_minuten, csv_path_de, row.names = FALSE)
