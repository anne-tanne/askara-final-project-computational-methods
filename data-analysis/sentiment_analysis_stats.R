# Load necessary libraries
library(dplyr)
library(readr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(maps)
library(hrbrthemes)

hrbrthemes::import_roboto_condensed()


# Set the output directory for saving visuals
output_dir <- "output/visuals/sentiment-analysis"

# Load datasets
df_french <- read_csv('output/data/df_french_sentiment_analysis.csv')
df_german <- read_csv('output/data/df_german_sentiment_analysis.csv')

df_german_summary <- df_german %>%
  count(sentiment) %>%
  mutate(Proportion = n / sum(n))

df_french_summary <- df_french %>%
  count(sentiment) %>%
  mutate(Proportion = n / sum(n))

# Combine summaries for plotting
df_summary <- bind_rows(
  mutate(df_german_summary, Language = "German"),
  mutate(df_french_summary, Language = "French")
)

sentiment_count <- ggplot(df_summary, aes(x = Language, y = Proportion, fill = sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("POSITIVE" = "#191970", "NEGATIVE" = "#dc143c")) +
  theme_ipsum() +
  labs(title = "Proportion of Article Sentiments by Language", y = "Proportion", x = "")

ggsave(file.path(output_dir, "sentiment_count.png"), sentiment_count, height = 6, width = 10)



# Negative Z-Test 
prop_neg_german <- sum(df_german$sentiment == "NEGATIVE") / nrow(df_german)
prop_neg_french <- sum(df_french$sentiment == "NEGATIVE") / nrow(df_french)

# Perform the two-proportion z-test
z_test_negative <- prop.test(x = c(sum(df_german$sentiment == "NEGATIVE"), sum(df_french$sentiment == "NEGATIVE")),
                             n = c(nrow(df_german), nrow(df_french)))

# Output the result
z_test_negative

# Positive Z-Test 
prop_pos_german <- sum(df_german$sentiment == "POSITIVE") / nrow(df_german)
prop_pos_french <- sum(df_french$sentiment == "POSITIVE") / nrow(df_french)

# Perform the two-proportion z-test
z_test_positive <- prop.test(x = c(sum(df_german$sentiment == "POSITIVE"), sum(df_french$sentiment == "POSITIVE")),
                             n = c(nrow(df_german), nrow(df_french)))

# Output the result
z_test_positive


# For the German dataset
sentiment_table_german <- table(df_german$sentiment)
chi_squared_german <- chisq.test(sentiment_table_german)

# For the French dataset
sentiment_table_french <- table(df_french$sentiment)
chi_squared_french <- chisq.test(sentiment_table_french)

# Output the results
chi_squared_german
chi_squared_french

