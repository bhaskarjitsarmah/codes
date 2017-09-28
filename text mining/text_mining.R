# loading data
coffee <- read.csv("coffee.csv", stringsAsFactors = FALSE)
chardonnay <- read.csv("chardonnay.csv", stringsAsFactors = FALSE)

str(coffee)
str(chardonnay)

# extracting tweets
coffee_tweets <- coffee$text
chardonnay_tweets <- chardonnay$text

# building corpus from text vector
library(tm)
coffee_source <- VectorSource(coffee_tweets)
coffee_corpus <- VCorpus(coffee_source)

chardonnay_source <- VectorSource(chardonnay_tweets)
chardonnay_corpus <- VCorpus(chardonnay_source)

# building corpus from text dataframe
library(dplyr)
coffee_text <- coffee %>% select(text, statusSource)
df_source <- DataframeSource(coffee_text)
df_corpus <- VCorpus(df_source)

# data cleaning
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "coffee", "mug", "RT",
                                          "amp", "chardonnay", "wine", "glass"))
  
  return(corpus)
}
coffee_clean <- clean_corpus(coffee_corpus)
print(coffee_clean[[227]][1])
print(coffee$text[227])
 
chardonnay_clean <- clean_corpus(chardonnay_corpus)
 
# creating DocumentTermMatrix
coffee_dtm <- DocumentTermMatrix(coffee_clean)
print(coffee_dtm) 
coffee_m <- as.matrix(coffee_dtm)
print(coffee_m[148:150, 2587:2590])

chardonnay_dtm <- DocumentTermMatrix(chardonnay_clean)
print(chardonnay_dtm)
chardonnay_m <- as.matrix(chardonnay_dtm)
print(chardonnay_m[148:150, 2587:2590])

# creating TermDocumentMatrix
coffee_tdm <- TermDocumentMatrix(coffee_clean)
coffee_m <- as.matrix(coffee_tdm)
print(coffee_m[2587:2590, 148:150])

chardonnay_tdm <- TermDocumentMatrix(chardonnay_clean)
chardonnay_m <- as.matrix(chardonnay_tdm)
print(chardonnay_m[2587:2590, 148:150])

# frequent terms with tm
term_frequency <- rowSums(coffee_m)
term_frequency <- sort(term_frequency, decreasing = TRUE)
barplot(term_frequency[1:10], col = 'tan', las = 2)

chardonnay_words <- rowSums(chardonnay_m)
chardonnay_words <- sort(chardonnay_words, decreasing = TRUE)
barplot(chardonnay_words[1:10], col = 'tan', las = 2)

# creating wordcloud
library(wordcloud)
library(RColorBrewer)

# Create purple_orange
purple_orange <- brewer.pal(10, 'PuOr')

# Drop 2 faintest colors
purple_orange <- purple_orange[-(1:2)]

word_freqs <- data.frame(term=names(term_frequency), num=term_frequency)
wordcloud(words = word_freqs$term, 
          freq = word_freqs$num, 
          max.words = 100, 
          colors = purple_orange)

# creating commonality cloud (common words)
all_coffee <- paste(coffee$text, collapse=" ")
all_chardonnay <- paste(chardonnay$text, collapse=" ")
all_tweets <- c(all_coffee, all_chardonnay)
all_tweets <- VectorSource(all_tweets)
all_corpus <- VCorpus(all_tweets)

all_clean <- clean_corpus(all_corpus)
all_tdm <- TermDocumentMatrix(all_clean)
all_m <- as.matrix(all_tdm)
commonality.cloud(all_m, colors="steelblue1", max.words=100)

# creating comparison cloud (dissimilar words)
colnames(all_tdm) <- c("coffee", "chardonnay")
all_m <- as.matrix(all_tdm)
comparison.cloud(all_m, colors=c("orange", "blue"), max.words = 50)

# creating pyramid plot
common_words <- subset(all_m, all_m[, 1] > 0 & all_m[, 2] > 0)
difference <- abs(common_words[, 1] - common_words[, 2])
common_words <- cbind(common_words, difference)
common_words <- common_words[order(common_words[, 3], decreasing = TRUE), ]

top25_df <- data.frame(x = common_words[1:25, 1], 
                       y = common_words[1:25, 2], 
                       labels = rownames(common_words[1:25, ]))

library(plotrix)

pyramid.plot(top25_df$x, top25_df$y,
             labels = top25_df$labels, gap = 8,
             top.labels = c("Chardonnay", "Words", "Coffee"),
             main = "Words in Common", laxlab = NULL, 
             raxlab = NULL, unit = NULL)

# creating word networks
word_associate(coffee$text, match.string = c("barista"), 
               stopwords = c(Top200Words, "coffee", "amp"), 
               network.plot = TRUE, cloud.colors = c("gray85", "darkred"))

title(main = "Barista Coffee Tweet Associations")

# creating word cluster
chardonnay_clust <- removeSparseTerms(chardonnay_tdm, sparse = 0.975) 
chardonnay_clust_m <- as.matrix(chardonnay_clust)
chardonnay_clust_df <- as.data.frame(chardonnay_clust_m)
chardonnay_clust_dist <- dist(chardonnay_clust_df)
chardonnay_clust_hc <- hclust(chardonnay_clust_dist)
plot(chardonnay_clust_hc)

# creating word cluster using dendextend
library(dendextend)

chardonnay_clust_hcd <- as.dendrogram(chardonnay_clust_hc)
labels(chardonnay_clust_hcd)
chardonnay_clust_hcd <- branches_attr_by_labels(chardonnay_clust_hcd, 
                                                c("marvin", "gaye"), 
                                                color="red")
plot(chardonnay_clust_hcd, main="Better Dendrogram")
rect.dendrogram(chardonnay_clust_hcd, k=4, border="grey50")

# association plot
library(ggplot2)
library(ggthemes)

associations <- findAssocs(coffee_tdm, "venti", 0.2)
associations
associations_df <- as.data.frame(associations)

ggplot(associations_df, aes(y = rownames(associations_df))) + 
  geom_point(aes(x = associations_df[, 1]), 
             data = associations_df, size = 3) +
  theme_gdocs()

# creating n-gram tokens
library(RWeka)

tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min=2, max=2))
}
unigram_dtm <- DocumentTermMatrix(coffee_corpus)
bigram_dtm <- DocumentTermMatrix(text_corp, control=list(tokenize = tokenizer))
unigram_dtm
bigram_dtm

# creating TfIdf terms
tf_tdm <- TermDocumentMatrix(coffee_corpus)
tfidf_tdm <- TermDocumentMatrix(coffee_corpus, control=list(weighting=weightTfIdf))
tf_tdm_m <- as.matrix(tf_tdm)
tfidf_tdm_m <- as.matrix(tfidf_tdm)
tf_tdm_m[508:509, 5:10]
tfidf_tdm_m[508:509, 5:10]

# adding metadata
custom_reader <- readTabular(mapping = list(content = "text", 
                                            id = "num",
                                            author = "screenName",
                                            date = "created"
))

text_corpus <- VCorpus(DataframeSource(coffee_source), readerControl = list(reader = custom_reader))
text_corpus <- clean_corpus(text_corpus)
text_corpus[[1]][1]
text_corpus[[1]][2]