
# Lab 10: text analysis ---------------------------------------------------

library(gutenbergr)
library(dplyr)
library(stringr)
library(tidytext)
library(textdata)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(igraph)
library(ggraph)
library(widyr)
library(scales)


# Get data ----------------------------------------------------------------

## explore gutenberg project text data
authorlist <- gutenberg_authors
metadata <- gutenberg_metadata

## query an author
author_titles <- gutenberg_metadata %>%
  filter(author == "Hemingway, Ernest" & language == "en") 

author_ids <- author_titles$gutenberg_id
author_ids

## load data
text <- gutenberg_download(gutenberg_id = author_ids)



# Q1: frequency analysis of common words ----------------------------------

## load stop words
data("stop_words")

## tokenize by word
tokens <- text %>%
  unnest_tokens(output = word, input = text, token = "words") %>%
  anti_join(stop_words) %>%
  mutate(linenumber = row_number())

book_words <- tokens %>%
  left_join(metadata %>% select(gutenberg_id, title), by = "gutenberg_id") %>%
  count(title, word, sort = TRUE)

## frequency counts
total_words <- book_words %>%
  group_by(title) %>%
  summarize(book_total = sum(n))
book_words <- left_join(book_words, total_words)

top_ten <- book_words %>%
  group_by(title) %>%
  slice_max(n, n = 10) %>%
  ungroup()

## and plot
ggplot(data = top_ten, 
       aes(x = reorder_within(x = word, by = n, within = title), # this sorts word count in order
           y = n, 
           fill = title)) +
  geom_col(show.legend = FALSE) + 
  scale_x_reordered() +  # recognize that the x is reordered (and remove reordering labels)
  coord_flip() + # stack bars vertically
  facet_wrap(~title, ncol = 2, scales = "free") + 
  labs(title = "Top 10 Words in Selected Works of Ernest Hemingway") +
  ylab("Counts") +
  xlab("Words") +
  theme_minimal()



# Q2: sentiment analysis --------------------------------------------------

## structure data for line by line analysis
books <- text %>%
  group_by(gutenberg_id) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(output = word, input = text) %>%
  left_join(metadata %>% select(gutenberg_id, title), by = "gutenberg_id")

## AFINN lexicon
afinn <- books %>%
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  group_by(title, index = linenumber %/% 50) %>%  ## group by 50 lines at a time
  summarise(sentiment = sum(value), .groups = "drop")

ggplot(afinn, aes(x = index, y = sentiment, fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ title, ncol = 2, scales = "free_x") +
  labs(
    title = "Sentiment Analysis of Author's Works",
    subtitle = "Sentiment score across each text, based on AFINN Lexicon") +
  theme_minimal()


## bing lexicon
bing <- books %>%
  inner_join(get_sentiments("bing")) %>%
  count(title, index = linenumber %/% 50, sentiment) %>%
  pivot_wider(names_from = "sentiment", values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(bing, aes(x = index, y = sentiment, fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free_x") +
  labs(title = "Sentiment Analysis of Author's Works", 
       subtitle = "Sentiment score across each text, based on Bing Lexicon")

## NRC lexicon
nrc <- books %>%
  inner_join(get_sentiments("nrc")) %>%
  count(title, index = linenumber %/% 50, sentiment)

ggplot(nrc, aes(x = index, y = n, fill = sentiment)) +
  geom_col() +
  facet_wrap(~ title, ncol = 2, scales = "free_x") +
  labs(
    title = "Sentiment Analysis of Author's Works",
    subtitle = "Counts of emotion words per 50-line segment, based on NRC lexicon")



# Q3: ngram analysis ------------------------------------------------------

## load data, unnesting by bigrams instead of words
bigrams <- text %>%
  unnest_tokens(output = bigram, input = text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>%
  mutate(pair = row_number())

## remove stop words
bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


# Markov chains
## count pairs
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

## filter original data for common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 5) %>%
  graph_from_data_frame()

## network visualization
set.seed(2017)
ggraph(graph = bigram_graph, layout = "fr") +
  geom_edge_link() + 
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

## add some fancy stuff to make a Markov chain...
set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  labs(title = "Mapping word relationships in Corpus",
       subtitle = "Markov chains based on bigrams from the corpus") +
  theme_void()


# correlation analysis

## tokenize once more, and drop words
words <- text %>%
  mutate(line = row_number()) %>%
  unnest_tokens(output = word, input = text, token = "words") %>%
  filter(!word %in% stop_words$word) 

## get common words
words_count <- words %>%
  filter(!str_detect(word, "’")) %>%  ## remove elisions
  count(word, sort = TRUE) %>%  ## count and sort
  slice_max(n, n = 10)   ## get the top n
  
top_10_words <- words_count$word  ## save words as list
top_10_words

## run correlations
correlations <- words %>%
  filter(word %in% top_10_words) %>%
  pairwise_cor(item = word, ## variable for correlating
               feature = line, ## pairing structure
               method = "pearson", ## correlation method
               upper = FALSE)


# and visualize some results with bar charts
correlations %>%
  filter(item1 %in% top_10_words) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 10) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() +
  labs(title = "Correlated words within Corpus") +
  ylab("Phi Coefficient") +
  xlab("word pairs")
