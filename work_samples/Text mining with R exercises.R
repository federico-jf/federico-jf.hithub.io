# Text mining exercises following Text Mining with R (Silge and Robinson, 2017)
# Federico Ferrero
# 5/20/2020

# clear environment 
rm(list=ls())

# enter text, an Emily Dickinson's poem
text<-c("Because I could not stop for Death-", 
        "He kindly stopped for me-",
        "The carriage held but just Ourselves-",
        "and Immortality")

# test output
text

# turn the previous text into a tidy text dataset
install.packages('dplyr')
library('dplyr')
text_df <- data_frame(line = 1:4, text=text)

# test the new dataset
text_df

# install tidytext library
install.packages('tidytext')

# TOKENIZATION: split text = it separes one by one the words from our database and indicates in which lines they are
library(tidytext)
text_df %>%
  unnest_tokens (word, text)

# tidying the works of Jane Austen: see pag 4 in Silge abd Robinson'book 
library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
  group_by(book)%>%
  mutate(linenumber = row_number(),
         chapter = cumsum (str_detect(text, regex("^chapter[\\divxlc]",
                                                  ignore_case = TRUE)))) %>%
  ungroup()

original_books

# tokenization the previous works of Jane Austen
library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books

# remove stop words
data("stop_words")

tidy_books  <- tidy_books %>%
  anti_join(stop_words)

# now when I check the output I have 217599 words (before, with the stops words included I had aboout 700000)
tidy_books 

# count words
tidy_books %>%
  count(word, sort = TRUE)
 
#visualization of count of words (sometimes it does not read %>%, try again after run libraries)
install.packages("ggplot2")
library(ggplot2)

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate (word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Gutenberg library: has thousands of books uploaded and its metadata
install.packages("gutenbergr")
library(gutenbergr)

# download H G Wells' 4 books using IDs (35, 36...), tokenization, and  remove stop words
hgwells <- gutenberg_download(c(35, 36, 5230, 159))
tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# test output
tidy_hgwells

# count words
tidy_hgwells %>%
  count(word, sort = TRUE)

# download Bronte sisters' 5 books using IDs (1260, 768...), tokenization, and  remove stop words
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# test output
tidy_bronte

# count words
tidy_bronte %>%
  count(word, sort = TRUE)

# binding the 3 data frames together (Austen, Wells and Bronte) in order to plot the comparison
library(tidyr)

frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"),
                       mutate(tidy_books, author = "Jane Austen")) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)

# plot the previous frequency and proportion comparison among Austen, Bronte sisters and Wells
install.packages("scales")
library(scales)
library(ggplot2)
# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Jane Austen`,
                      color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

# calculate how correlated are the word frequencies between Austen and the Bronte sisters.
cor.test(data = frequency[frequency$author == "Brontë Sisters",],
         ~ proportion + `Jane Austen`)
# correlation = 0.7609938

# calculate how correlated are the word frequencies between Austen and the Wells.
cor.test(data = frequency[frequency$author == "H.G. Wells",],
         ~ proportion + `Jane Austen`)
# correlation = 0.4241601 
# the word frequencies are more correlated between the Austen and Bronte novels than between Austen and Wells.

### SENTIMENT ANALYSIS
# the tidytext package contains several sentiment lexicons in the sentiments dataset
library(tidytext)
sentiments

# get specific sentiment lexicons: NRC describes word emotions 
get_sentiments("nrc")
# it returned an error, so I google the solution:

library(remotes)
install_github("EmilHvitfeldt/textdata")
install_github("juliasilge/tidytext")

# now it works
get_sentiments("nrc")

# what are the most common joy words in Emma (by Jane Austen): tokenization of the words
library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
  group_by(book)%>%
  mutate(linenumber = row_number(),
         chapter = cumsum (str_detect(text, regex("^chapter[\\divxlc]",
                                                  ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

original_books

# use the NRC lexicon and filter for the joy words. Next filter the joy words in the Emma book. Finally count words
nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

tidy_books %>%
  filter(book== "Emma") %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)

# examine how sentiment changes throughout each novel (pag 18)
library(tidyr)
janeautensentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index= linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# now plot previous calculation: we can see how each novel changes toward + or - sentiment across the story 
library(ggplot2)
ggplot(janeautensentiment, aes(index, sentiment, fill = book)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

# comparing the 3 sentiment dicionaries or lexicons using "Pride and Prejudice"

pride_prejudice <- tidy_books %>%
  filter(book == "Pride & Prejudice")

#test output
pride_prejudice

# estimate the net sentiment (positive-negative) in each chunk of the novel for each lexicon (pag 20)
afinn <- pride_prejudice %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = linenumber %/% 80) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  pride_prejudice %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  pride_prejudice %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# plot the previous estimation: wesee similar picks and downs in the same novel places using different lexicons
bind_rows(afinn,
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# most common positive and negative words

bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# test output
bing_word_counts

# plot previous calculation

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder (word,n)) %>%
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", 
       x = NULL) +
  coord_flip()

# add "miss" (because has been included as a "negative word") to a custom stop-words

custom_stop_words <- bind_rows(data_frame(word = c("miss"),
                                          lexicon = c ("custom")),
                               stop_words)

# wordcloud: most common words in Jane Austen's novels
install.packages("wordcloud")
library(wordcloud)
tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# generate a wordcloud showing sentiment analysis

install.packages("reshape2")
library("reshape2")
tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"), 
                   max.words = 100)