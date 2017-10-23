# loading libraries
library(qdap)
library(magrittr)
library(tm)
library(dplyr)
library(metricsgraphics)
library(qdapDictionaries)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(radarchart)

# creating a dataframe
person <- c("Ajay", "Hari", "Ram", "Rahim", "Ajay", "Hari", "Ram", "Rahim")
text <- c("Momos are the best",
          "I like to eat Momos",
          "I don't like other street foods but Momos",
          "What is for lunch?",
          "Dee Lazeez has a variety of food items",
          "I am very excited about the food festival",
          "Suimui momos are hard to prepare and difficult to garnish",
          "I think the food here is good")
text_df <- as.data.frame(cbind(person, text))

# overall polarity score
text_df %$% polarity(text)

# polarity score by person
(conversation <- text_df %$% polarity(text, person))

# counting table from conversation
counts(conversation)

# plotting conversation polarity
plot(conversation)

# cleaning corpus function
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "momos"))
  return(corpus)
}

# creating text vector
tm_define <- as.vector(text_df$text)

# creating a vector source
tm_vector <- VectorSource(tm_define)

# creating corpus from VectorSource
tm_corpus <- VCorpus(tm_vector)
content(tm_corpus[[1]])

# cleaning the corpus
tm_clean <- clean_corpus(tm_corpus)
content(tm_clean[[1]])

# creating document term matrix
tf_dtm <- DocumentTermMatrix(tm_clean)
tf_dtm_m <- as.matrix(tf_dtm)
dim(tf_dtm_m)
tf_dtm_m

# reading kos blog dataset
vocab <- read.csv("vocab.kos.txt", header = FALSE, col.names = c("word")) %>% mutate(wordID = 1:6906)
words <- read.csv("docword.kos.txt", header = FALSE, sep = " ", col.names = c("docID", "wordID", "count"),
                  skip = 3)

kos_words <- merge(words, vocab, by = "wordID") %>% select(docID, word, count) %>% group_by(word) %>% 
  summarize(freq=sum(count)) %>% mutate(rank = rank(desc(freq), ties.method=c("random")), 
                                        expectations = freq/rank)

# plotting frequency and expectation as per Zipf's Law
kos_plot <- mjs_plot(kos_words, x=rank, y=freq, show_rollover_text = FALSE)
kos_plot <- mjs_line(kos_plot)
kos_plot <- mjs_add_line(kos_plot, expectations)
kos_plot <- mjs_add_legend(kos_plot, legend = c("Frequency", "Expectation"))
kos_plot

# plotting conversation and lexicon
conversation
plot(conversation)

lexicon <- polarity(
  text.var = text_df$text,
  grouping.var = text_df$person,
  polarity.frame = key.pol,
  negators = negation.words,
  amplifiers = amplification.words,
  deamplifiers = deamplification.words)

lexicon
plot(lexicon)

# creating the song lyrics
stressed_out <- "I wish I found some better sounds no ones ever heard\nI wish I had a better voice that 
sang some better words\nI wish I found some chords in an order that is new\nI wish I didnt have to rhyme 
every time I sang\nI was told when I get older all my fears would shrink\nBut now Im insecure and I care 
what people think\nMy names Blurryface and I care what you think\nMy names Blurryface and I care what you 
think\nWish we could turn back time, to the good old days\nWhen our momma sang us to sleep but now were 
stressed out\nWish we could turn back time to the good old days\nWhen our momma sang us to sleep but now 
were stressed out\nWere stressed out\nSometimes a certain smell will take me back to when I was young\n
How come Im never able to identify where its coming from\nId make a candle out of it if I ever found it
\nTry to sell it never sell out of it Id probably only sell one\nItd be to my brother, cause we have the 
same nose\nSame clothes homegrown a stones throw from a creek we used to roam\nBut it would remind us of 
when nothing really mattered\nOut of student loans and tree-house homes we all would take the latter\n
My names Blurryface and I care what you think\nMy names Blurryface and I care what you think\nWish we 
could turn back time, to the good old days\nWhen our momma sang us to sleep but now were stressed out\n
Wish we could turn back time, to the good old days\nWhen our momma sang us to sleep but now were 
stressed out\nWe used to play pretend, give each other different names\nWe would build a rocket ship and 
then wed fly it far away\nUsed to dream of outer space but now theyre laughing at our face #\nSaying, 
Wake up you need to make money\nYeah\nWe used to play pretend give each other different names\nWe would 
build a rocket ship and then wed fly it far away\nUsed to dream of outer space but now theyre laughing 
at our face\nSaying, Wake up, you need to make money\nYeah\nWish we could turn back time, to the good 
old days\nWhen our momma sang us to sleep but now were stressed out\nWish we could turn back time, to 
the good old days\nWhen our momma sang us to sleep but now were stressed out\nUsed to play pretend, used 
to play pretend bunny\nWe used to play pretend wake up, you need the money\nUsed to play pretend used to 
play pretend bunny\nWe used to play pretend, wake up, you need the money\nWe used to play pretend give 
each other different names\nWe would build a rocket ship and then wed fly it far away\nUsed to dream of 
outer space but now theyre laughing at our face\nSaying, Wake up, you need to make money\nYeah"

polarity(stressed_out)
key.pol[grep("stress", x)]

# New lexicon
custom_pol <- sentiment_frame(positive.words, c(negative.words, "stressed", "turn back"))
# Compare new score
polarity(stressed_out, polarity.frame = custom_pol)

# tidytext matrix
tf_tidy <- tidy(tf_dtm)
tf_tidy

# examining sentiments dataframe
afinn_lex <- get_sentiments("afinn")
afinn_lex %>% count(score)

nrc_lex <- get_sentiments("nrc")
nrc_lex
nrc_counts <- nrc_lex %>% count(sentiment)
ggplot(nrc_counts, aes(x = sentiment, y = n))+
  geom_bar(stat = "identity")

# bing tidy polarity
bing <- get_sentiments("bing")
kos_words$word <- as.character(kos_words$word)
text_bing_words <- inner_join(kos_words, bing, by = c("word" = "word")) %>% 
  count(sentiment, rank) %>% 
  spread(sentiment, n, fill=0) %>% 
  mutate(polarity = positive - negative)

# Plot polarity vs. index
ggplot(text_bing_words, aes(rank, polarity)) + 
  # Add a smooth trend curve
  geom_smooth(method = "lm")

# reading enron email dataset
enron_vocab <- read.csv("vocab.enron.txt", header = FALSE, col.names = c("word"), 
                        stringsAsFactors = FALSE) %>% 
  mutate(wordID = 1:28102)
enron_words <- read.csv("docword.enron.txt", header = FALSE, sep = " ", 
                        col.names = c("docID", "wordID", "count"), skip = 3)

enron_words <- merge(enron_words, enron_vocab, by = "wordID") %>% select(docID, word, count) 

afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

# enron sentiment based on afinn 
enron_afinn <- enron_words %>% 
  # Inner Join to AFINN lexicon
  inner_join(afinn, by = c("word" = "word")) %>%
  # Count by score and document ID
  count(score, docID)

enron_afinn_agg <- enron_afinn %>% 
  # Group by line
  group_by(docID) %>%
  # Sum scores by line
  summarize(total_score = sum(score))

ggplot(enron_afinn_agg, aes(docID, total_score)) +
  geom_smooth()


# enron sentiment based on nrc
enron_nrc <- inner_join(enron_words, nrc, by = c("word" = "word"))

# DataFrame of tally
enron_nrc <- enron_nrc %>% 
  # Group by sentiment
  group_by(sentiment) %>% 
  # Get total count by sentiment
  summarize(total_count = sum(count))

# Plot the counts
ggplot(enron_nrc, aes(x = sentiment, y = total_count)) +
  # Add a column geom
  geom_col()


# enron sentiment by bing

enron_bing <- enron_words %>%
  # Inner join to the lexicon
  inner_join(bing, by = c("word" = "word")) %>%
  # Count by sentiment, index
  count(sentiment, docID) %>%
  # Spread sentiments
  spread(sentiment, n, fill=0) %>%
  mutate(
    # Add polarity field
    polarity = positive - negative,
    # Add line number field
    docID = unique(docID)
  )

# Plot
ggplot(enron_bing, aes(docID, polarity)) + 
  geom_smooth() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("Enron Emails Chronological Polarity")


# Enron frequency analysis
enron_sents <- inner_join(enron_words, bing, by = c("word" = "word"))

# Tidy sentiment calculation
enron_tidy_sentiment <- enron_sents %>% 
  count(word, sentiment, wt = count) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative)

# Review
enron_tidy_sentiment

# Subset
enron_tidy_small <- enron_tidy_sentiment %>% 
  filter(abs(polarity) >= 1000)

# Add polarity
enron_tidy_pol <- enron_tidy_small %>% 
  mutate(
    pol = ifelse(polarity>0, "positive", "negative")
  )

# Plot
ggplot(
  enron_tidy_pol, 
  aes(reorder(word, polarity), polarity, fill = pol)
) +
  geom_bar(stat = "identity") + 
  ggtitle("Moby Dick: Sentiment Word Frequency") + 
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1))


# Enron emotional introspection
enron_sentiment <- inner_join(enron_words, nrc)

# Drop positive or negative
enron_pos_neg <- enron_sentiment %>%
  filter(!grepl("positive|negative", sentiment))

# Count terms by sentiment then spread 
enron_tidy <- enron_pos_neg %>% 
  count(sentiment, term = word) %>% 
  spread(sentiment, n, fill = 0) %>%
  as.data.frame()

# Set row names
rownames(enron_tidy) <- enron_tidy[, 1]

# Drop terms column
enron_tidy[, 1] <- NULL

# Examine
head(enron_tidy)

# Comparison cloud
comparison.cloud(enron_tidy, max.words = 50, title.size = 1.5)


## Enron radarchart
enron_sentiment <- inner_join(enron_words, nrc)

# Drop positive or negative
enron_pos_neg <- enron_sentiment %>%
  filter(!grepl("positive|negative", sentiment))

# Tidy tally
enron_tally <- enron_pos_neg %>%
  group_by(sentiment) %>%
  tally()

# JavaScript radar chart
chartJSRadar(enron_tally)


## treemaps
books_score <- all_books %>% 
  # Inner join with AFINN scores
  inner_join(afinn, by=c("term" = "word"))

book_length <- books_score %>% 
  # Count number of words per book
  count(book)

book_score <- books_score %>% 
  # Group by author, book
  group_by(author, book) %>%
  # Calculate mean book score
  summarize(mean_score = mean(score))

book_tree <- book_score %>% 
  # Inner join by book
  inner_join(book_length, by="book")

# Examine the results
book_tree

# Make the visual
treemap(book_tree,
        index = c("author", "book"),
        vSize = "n",
        vColor = "mean_score",
        type = "value",
        title = "Book Sentiment Scores",
        palette = c("red", "white", "green"))


## Wordcloud
# Matrix
all_tdm_m <- as.matrix(all_tdm)

# Column names
colnames(all_tdm_m) <- c("positive", "negative")

# Top pos words
order_by_pos <- order(all_tdm_m[, 1], decreasing = TRUE)

# Review top 10 pos words
all_tdm_m[order_by_pos, ] %>% head(n=10)

# Top neg words
order_by_neg <- order(all_tdm_m[, 2], decreasing = TRUE)

# Review top 10 neg words
all_tdm_m[order_by_neg, ] %>% head(n=10)

# Comparison cloud
comparison.cloud(
  all_tdm_m, 
  max.words = 20,
  colors = c("darkgreen","darkred")
)
