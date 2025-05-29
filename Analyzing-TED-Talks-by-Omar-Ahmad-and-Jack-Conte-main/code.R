if(!is.null(dev.list())) dev.off()  # clear out the past 
rm(list = ls())
cat("\014")

# Load necessary packages
# =======================
library(dsEssex)  # load the dsEssex package
library(tidyverse)  # load the tidyverse package
library(tidytext)  # load the tidytext package
library(ggrepel)  # load the ggrepel package
library(cowplot)  # load the cowplot package


# Load TED Talks dataset and display the first few rows
# =====================================================
data(ted_talks)  # load the ted_talks dataset
head(ted_talks)  # display the first few rows of the dataset


# Tokenize the text column by word and tidy the resulting data frame
# ==================================================================
tokened_ted_talks <- ted_talks %>% # select the data frame to tidy and tokenize
  tidytext::unnest_tokens(word, text)  # tidy the data frame and tokenize the text column by word

# Remove stop words from the tokenized data frame
# ===============================================
nonstop_ted_talks <- tokened_ted_talks %>% # select the tidied and tokenized data frame to remove stopwords
  dplyr::anti_join(get_stopwords()) # remove stop words using the anti_join function from the dplyr package


# Count the frequency of each word spoken by Omar Ahmad in the non-stop words data frame
# ======================================================================================
oma_ahmad_talk <- nonstop_ted_talks %>% # select the data frame with stop words to create a Omar Ahmad df
  dplyr::filter(speaker == "Omar Ahmad") %>% # filter by speaker
  dplyr::count(speaker, word, sort = TRUE) # count the frequency of each word


# Count the frequency of each word spoken by Jack Conte in the non-stop words data frame
# ======================================================================================
jack_conte_talk <- nonstop_ted_talks %>% # select the data frame with stop words to create a Jack Conte df
  dplyr::filter(speaker == "Jack Conte") %>% # filter by speaker
  dplyr::count(speaker, word, sort = TRUE) # count the frequency of each word

head(oma_ahmad_talk) 
head(jack_conte_talk) # Display the first few rows of the resulting data frame


# Combine the word count data frames for each speaker and select the top 10 most frequent words for each speaker
# ======================================================================================
omar_and_jack_top_words <- dplyr::bind_rows(oma_ahmad_talk, jack_conte_talk) %>% # select the filtered word by speaker and frequency
  group_by(speaker) %>% # Group the filtered data by speaker
  top_n(20, n) %>% # select top 20 words by frequency
  arrange(speaker, desc(n)) # Arranged selected words in descending order

head(omar_and_jack_top_words) # Display the first few rows of the resulting top words data frame


# Display the top 20 most frequent words for each speaker in a faceted bar plot
# =============================================================================
oma_ahmad_plot <- oma_ahmad_talk %>% 
  filter(speaker == "Omar Ahmad") %>% # filters data to include only
  slice_max(n, n = 20) %>% # selects top 20 words based on frequency text
  mutate(word = reorder(word, n)) %>% # reorder words to plotted in order of frequency
  ggplot(aes(n, word, fill = speaker)) + # create ggplot() with the x-axis frequency count, y-axis word
  geom_col(show.legend = FALSE) + # Add a column chart to the plot
  scale_fill_manual(values = c("purple")) + # fill bar color
  labs(title = "Top 20 Words for Omar Ahmad", x = "Count", y = "Word") + # sets title and axis labels
  theme_minimal() # apply minimal theme to the plot

jack_conte_plot <- jack_conte_talk %>% 
  filter(speaker == "Jack Conte") %>% # filters data to include only
  slice_max(n, n = 20) %>% # selects top 20 words based on frequency text
  mutate(word = reorder(word, n)) %>% # reorder words to plotted in order of frequency
  ggplot(aes(n, word, fill = speaker)) + # create ggplot() with the x-axis frequency count, y-axis word
  geom_col(show.legend = FALSE) + # Add a column chart to the plot
  scale_fill_manual(values = c("orange")) + # fill bar color
  labs(title = "Top 20 Words for Jack Conte", x = "Count", y = "Word") + # sets title and axis labels
  theme_minimal()  # apply minimal theme to the plot

plot_grid(oma_ahmad_plot, jack_conte_plot, ncol = 2) # use the "cowplot" package to arrange plots side by side 


# Compare speaker words using visualisation
# ============================================
compare_speakers_words <- dplyr::bind_rows(oma_ahmad_talk, jack_conte_talk) %>% # combine Jack & Omar data frames into a single data frame.
  group_by(word) %>% # data frame grouped by the word
  filter(sum(n) > 10) %>% # filter rows with number of words greater than 10
  ungroup() %>% # ungroup the data
  pivot_wider(names_from = "speaker", values_from = "n", values_fill = 0) %>% # reshape the data from long to wide and replace any missing value
  ggplot(aes(`Omar Ahmad`, `Jack Conte`)) + # specify the x and y for Omar Ahmad and Jack Conte
  geom_abline(color = "blue", size = 1.2, alpha = 0.75, lty = 3) + # draw diagonal line on the plot
  geom_text_repel(aes(label = word), max.overlaps = 15) + # add labels to the plot and maximum overlapping
  coord_fixed() # to plot equal aspect ratio.


omar_and_jack_talk <- nonstop_ted_talks %>% # select the data frame to filter
  filter(speaker %in% c("Omar Ahmad", "Jack Conte")) # filter by speakers Omar Ahmad and Jack Conte


# Identify the 10 most common words spoken by both Omar Ahmad and Jack Conte
# ===========================================================================
omar_and_jack_common_words <- omar_and_jack_talk %>% # select the tokenized and non stop data
  group_by(word) %>% # group the common words data frame by words
  filter(n_distinct(speaker) == 2) %>% # filter the grouped data frame to keep only the words that are spoken by both speakers
  count(word) %>% # count the number of times each word appears in the filtered data frame
  ungroup() %>% # Remove grouping
  arrange(desc(n)) %>% # arrange in descending order
  top_n(10, n) # select top 10 words by frequency

head(omar_and_jack_common_words) # Display the first few rows common words by speakers


# Display the top 10 most common words spoken by both speakers in a bar plot
# ==========================================================================
common_word_plot <- ggplot(omar_and_jack_common_words, aes(x = fct_reorder(word, n), y = n)) + # create ggplot object for common words by speakers
  geom_col(fill = "black") + # adds column chart to the plot with black fill color
  labs(title = "Top 10 Most Frequently Used Common Words",
       x = "Word", y = "Count") + # set the title and labels for the plot
  theme_minimal() # apply the minimal theme to the plot


# Sentiment analysis
# =========================================
sentiment_lexicon <- get_sentiments("bing") # Get sentiment lexicon from tidytext
omar_and_jack_sentiment <- omar_and_jack_talk %>%
  inner_join(sentiment_lexicon) %>% # join dataset with sentiment lexicon using the inner_join() function
  count(speaker, word, sentiment) %>% # group dataset by speaker, word and sentiment count
  group_by(speaker, sentiment) %>% # group by speaker and sentiment
  mutate(total_words = sum(n), percent = n/total_words) %>% # create column with sum of word counts for each speaker-sentiment combination
  arrange(desc(percent)) #arrange % in descending order

# Get top words for each sentiment
# ================================
sentiment_top_words <- omar_and_jack_sentiment %>% # create dataframe from sentiment data
  group_by(speaker, sentiment) %>% # group 
  slice_max(n, n = 7) %>% # extract top 10 highest percentage for each sentiment category
  ungroup() # ungroup the extracted data

# Plot top words
# ==============
sentiment_plot <- ggplot(sentiment_top_words, aes(percent, word, fill = sentiment)) + # create ggplot object with sentiment_top_words
  geom_col(show.legend = FALSE) + # Add a column chart to the plot
  facet_wrap(speaker ~ sentiment, scales = "free_y") + # creates a panel of plots arranged in a grid
  labs(x = "Percentage", y = "") + # sets labels for x-axis and y-axis
  theme(axis.text.y = element_text(size = 9)) + # sets the appearance of the plot
  scale_fill_manual(values = c("positive" = "#1B9E77", "negative" = "#D95F02", "neutral" = "#7570B3"))


# Calculate the sentiment score for each speaker by summing the AFINN sentiment values for each word spoken
# ======================================================================================
my_speakers_sentiment_score <- omar_and_jack_talk %>% # select the tidy non stop data
  inner_join(get_sentiments("afinn"), by = "word") %>% # Join tokenized TED talk data with AFINN sentiment lexicon by matching words
  group_by(speaker) %>% # Group the data by speaker
  summarize(sentiment = sum(value)) %>% # Sum up the sentiment values for each speaker
  ungroup() # Remove the grouping structure


my_speakers_sentiment_score # display the sentiment score of both speakers

plot_grid(oma_ahmad_plot, jack_conte_plot, ncol = 2)
