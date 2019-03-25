library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# Carregando o dataset

df <- readLines('mlk.txt')
head(df)
dfCorpus <- Corpus(VectorSource(dfCorpus))
class(dfCorpus)


dfCorpus <- tm_map(dfCorpus, removePunctuation)

dfCorpus <- tm_map(dfCorpus, removeWords, stopwords('english'))

dfCorpus <- tm_map(dfCorpus, stemDocument)

dfCorpus <- tm_map(dfCorpus, tolower)

dtm <- DocumentTermMatrix(dfCorpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v), freq = v)
head(d, 10)

# wordcloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 2,
          max.words = 200, random.order = TRUE, 
          colors = brewer.pal(8, "Dark2"))




wordcloud(dfCorpus, max.words = 100, random.order = FALSE)
