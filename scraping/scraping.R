# Load required libraries
library(xml2)
library(tidyverse)

# Function to parse sitemap and return a dataframe of URLs and other metadata
parse_sitemap <- function(xml_url, publication_name, language, sitemap_dir, csv_dir) {
  # Construct file paths
  sitemap_file_path <- file.path(sitemap_dir, paste0(publication_name, "_sitemap.xml"))
  csv_file_path <- file.path(csv_dir, paste0(Sys.Date(), "_", publication_name, ".csv"))
  
  # Create directories if they don't exist
  dir.create(sitemap_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(csv_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Download the sitemap file
  download.file(xml_url, sitemap_file_path, method = "libcurl")
  
  # Read the XML sitemap
  xml_data <- read_xml(sitemap_file_path)
  
  # Namespace handling
  ns <- xml_ns_rename(xml_ns(xml_data), d1 = "default")
  
  # Extract URLs
  urls <- xml_find_all(xml_data, ".//default:url", ns)
  
  # Extract details for each URL and compile into a dataframe
  df <- map_df(urls, ~{
    loc <- xml_find_first(.x, ".//default:loc", ns) |> xml_text()
    publication_date <- xml_find_first(.x, ".//news:publication_date", ns) |> xml_text()
    title <- xml_find_first(.x, ".//news:title", ns) |> xml_text()
    keywords <- xml_find_first(.x, ".//news:keywords", ns) |> xml_text()
    
    # Optional: Extract image location
    image_node <- xml_find_first(.x, ".//image:loc", ns)
    image_loc <- if (!is.na(image_node)) xml_text(image_node) else NA_character_
    
    # Return a tibble
    tibble(loc, publication_name, language, publication_date, title, keywords, image_loc)
  })
  
  # Write the dataframe to a CSV file
  write.csv(df, file = csv_file_path)
  
  return(df)
}

# Define directory paths
sitemap_dir <- "sitemaps"
csv_dir <- "csv_files"

# Parse sitemaps and write to CSV files in separate folders
df_20minuti <- parse_sitemap("https://www.tio.ch/sitemap.xml", "20minuti", "it", sitemap_dir, csv_dir)
df_20minuten <- parse_sitemap("https://www.20min.ch/sitemaps/de/news.xml", "20minuten", "de", sitemap_dir, csv_dir)
df_20minutes <- parse_sitemap("https://www.20min.ch/sitemaps/fr/news.xml", "20minutes", "fr", sitemap_dir, csv_dir)

# Optional: print out the dataframes to check
print(df_20minuti)
print(df_20minuti)
print(df_20minutes)
