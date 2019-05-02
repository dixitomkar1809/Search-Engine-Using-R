library(tm)
library(dplyr)
library(janeaustenr)
library(tidytext)
library(wordcloud)
library(SnowballC)

## Create corpus
df <- read.table("http://www.utdallas.edu/~ond170030/data/MovieSummaries/plot_summaries.txt", header = FALSE, sep="\t",stringsAsFactors = FALSE,quote="")
df_title <- data.frame(doc_id = df[,1], text = df[,2])

plot.text.list <- as.list(as.character(df_title$text))

names(plot.text.list) <- df_title$doc_id

searchString <- "Thriller Brad Pitt"

corpus <- VCorpus(c(plot.text.list, searchString))

cleanCorpus <- tm_map(corpus, stripWhitespace)
cleanCorpus <- tm_map(cleanCorpus, removeWords, stopwords("english"))
cleanCorpus <- tm_map(cleanCorpus, stemDocument)
cleanCorpus <- tm_map(cleanCorpus, removePunctuation)
cleanCorpus <- tm_map(cleanCorpus, removeNumbers)
