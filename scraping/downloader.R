library(tidyverse)
library(httr)

# Function to sanitize a title to create a safe filename
sanitize_filename <- function(string) {
  # Transliterate German, French, and Italian characters
  string <- gsub("ä", "ae", string)
  string <- gsub("ö", "oe", string)
  string <- gsub("ü", "ue", string)
  string <- gsub("Ä", "Ae", string)
  string <- gsub("Ö", "Oe", string)
  string <- gsub("Ü", "Ue", string)
  string <- gsub("ß", "ss", string)
  string <- gsub("é|è|ê|ë", "e", string)
  string <- gsub("à|á|â|ã|ä", "a", string)
  string <- gsub("ì|í|î|ï", "i", string)
  string <- gsub("ò|ó|ô|õ|ö", "o", string)
  string <- gsub("ù|ú|û|ü", "u", string)
  string <- gsub("ç", "c", string)
  string <- gsub("ñ", "n", string)
  
  # Convert all other special characters to ASCII
  string <- iconv(string, to = "ASCII//TRANSLIT")
  
  # Replace apostrophes with hyphens and remove quotation marks
  string <- gsub("'", "-", string)
  string <- gsub("\"", "", string)
  
  # Replace colons with hyphens and remove commas
  string <- gsub(":", "", string)
  string <- gsub(",", "", string)
  
  # Replace spaces with hyphens
  string <- gsub(" ", "-", string)
  
  # Convert to lower case
  string <- tolower(string)
  
  # Remove remaining non-alphanumeric characters (except for hyphens)
  string <- gsub("[^A-Za-z0-9\\-]", "", string)
  
  return(string)
}

# Function to download HTML files using sanitized filenames
download_html_files <- function(csv_file_path, target_dir, start_date, end_date) {
  # Read the CSV file
  df <- read_csv(csv_file_path, show_col_types = FALSE)
  
  # Convert dates to Date type for comparison
  df$date <- as.Date(df$date)
  
  # Filter the dataframe for the specified date range
  df <- df |> filter(date >= as.Date(start_date) & date <= as.Date(end_date))
  
  # Iterate over each row in the dataframe
  for(i in 1:nrow(df)) {
    # Sanitize the title to create a filename
    title <- sanitize_filename(df$title[i])
    date <- gsub("-", "", df$date[i]) # Remove hyphens from date
    filename <- paste0(date, "_", title, ".html")
    
    # Specify the path for the file
    file_path <- file.path(target_dir, filename)
    
    # Print message to console
    message("Downloading: ", filename)
    
    # Download the file and handle errors silently
    try({
      GET(df$loc[i], write_disk(file_path, overwrite = TRUE))
      message("Downloaded: ", filename)
      Sys.sleep(2) # Sleep for 2 seconds between downloads
    }, silent = TRUE)
  }
}


# Define base directory and subdirectories
base_dir <- getwd()
csv_final_dir <- file.path(base_dir, "csv_files", "final")
html_dir <- file.path(base_dir, "HTML")

# Create the 'html' directory and subdirectories for each publication if they don't exist
dir.create(html_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(html_dir, "20minuti"), showWarnings = FALSE)
dir.create(file.path(html_dir, "20minuten"), showWarnings = FALSE)
dir.create(file.path(html_dir, "20minutes"), showWarnings = FALSE)

# List of publication names
publications <- c("20minuti", "20minuten", "20minutes")
start_date <- "2023-11-10"
end_date <- "2023-11-24"

# Process each publication's CSV files
for (publication in publications) {
  # Define the path to the current publication's CSV file
  csv_file_path <- file.path(csv_final_dir, paste0("processed_", publication, ".csv"))
  
  # Define the target directory for the HTML files
  target_dir <- file.path(html_dir, publication)
  
  # Print message to console
  message("Starting downloads for ", publication)
  
  # Download the HTML files for the current publication within the date range
  download_html_files(csv_file_path, target_dir, start_date, end_date)
  
  # Print message to console
  message("Completed downloads for ", publication)
}