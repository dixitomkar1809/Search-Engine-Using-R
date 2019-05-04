library(tm)
library(dplyr)
library(tidytext)
library(wordcloud)
library(SnowballC)

# Read the Data
df <- read.table("http://www.utdallas.edu/~ond170030/data/MovieSummaries/plot_summaries_500.txt", header = FALSE, sep="\t",stringsAsFactors = FALSE,quote="")

# Getting titles of the df
df_title <- data.frame(doc_id = df[,1], text = df[,2])

# Converting from factors to text and then setting the name as the id
plotList <- as.list(as.character(df_title$text))
noPlotList <- length(plotList)
names(plotList) <- df_title$doc_id

# Search String
query <- "serial killer"

# Creating Corpus
# Here we are adding the query to the corpus or the plots itself
plots <- VectorSource(c(plotList, query))

# putting the name of the query as query rest others will be just the id
plots$Names <- c(names(plotList), "query")

corpus <- Corpus(plots)

# Cleaning Corpus 
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, stripWhitespace)

# Creating Term Document Matrix(corpus)
# this is a type of simple triplet matrix, (i, j, value) are stored for non zero values
tdmTriplet <- TermDocumentMatrix(corpus)

# Dense Matrix
tdm <- as.matrix(tdmTriplet)

# Calculates tdidf weights
get.tf.idf.weights <- function(tf.vec, df) {
  weight = rep(0, length(tf.vec))
  weight[tf.vec > 0] = (1 + log2(tf.vec[tf.vec > 0])) * log2(noPlotList/df)
  weight
}

get.weights.per.term.vec <- function(tfidf.row) {
  term.df <- sum(tfidf.row[1:noPlotList] > 0)
  tf.idf.vec <- get.tf.idf.weights(tfidf.row, term.df)
  return(tf.idf.vec)
}

# Generating TF_IDF Matrix
tf_idfMatrix <- t(apply(tdm, c(1), FUN = get.weights.per.term.vec ))
colnames(tf_idfMatrix) <- colnames(tdm)

# Normalizing the tfidf matrix
tf_idfMatrix <- scale(tf_idfMatrix, center = FALSE, scale = sqrt(colSums(tf_idfMatrix^2)))

# Query Vector
queryVector <- tf_idfMatrix[, (noPlotList+1)]
tf_idfMatrix <- tf_idfMatrix[, 1:noPlotList]

# Cosine Similarities are simple dot products as our vectors are normalized
scores <- t(queryVector) %*% tf_idfMatrix

# Ranking the plots with the cosine similarities with the query
results <- data.frame(doc=names(plotList), score = t(scores), text=unlist(plotList))

# Ordering in decreasin order of cosine score
results <- results[order(results$score, decreasing = TRUE), ]
View(results[1:10, ])


