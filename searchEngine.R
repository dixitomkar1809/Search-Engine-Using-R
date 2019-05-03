library(tm)
library(dplyr)
library(janeaustenr)
library(tidytext)
library(wordcloud)
library(SnowballC)

df <- read.table("http://www.utdallas.edu/~ond170030/data/MovieSummaries/plot_summaries_500.txt", header = FALSE, sep="\t",stringsAsFactors = FALSE,quote="")
df_title <- data.frame(doc_id = df[,1], text = df[,2])

plot.text.list <- as.list(as.character(df_title$text))
no.plot.text.list <- length(plot.text.list)
names(plot.text.list) <- df_title$doc_id

searchString <- "serial killer"

documents <- VectorSource(c(plot.text.list, searchString))
documents$Names <- c(names(plot.text.list), "searchString")

corpus <- Corpus(documents)

cleanCorpus <- tm_map(corpus, stripWhitespace)
cleanCorpus <- tm_map(cleanCorpus, tolower)
cleanCorpus <- tm_map(cleanCorpus, removeWords, stopwords("english"))
cleanCorpus <- tm_map(cleanCorpus, stemDocument)
cleanCorpus <- tm_map(cleanCorpus, removePunctuation)
cleanCorpus <- tm_map(cleanCorpus, removeNumbers)

dtm <- DocumentTermMatrix(cleanCorpus)

dtm.tf_idf <- weightTfIdf(dtm)

m<-(as.matrix(dtm.tf_idf))

Filt_m <-m[,m[8,]>0]

CosineCoeff<-0*(1:no.plot.text.list)

for (i in 1:no.plot.text.list) {
  CosineCoeff[i]<-sum(Filt_m[i,]*Filt_m[8,])/(sum(Filt_m[i,]^2)^0.5 *sum(Filt_m[8,]^2)^0.5)
}

PageRank<-CosineCoeff[order(CosineCoeff,decreasing=TRUE)]
RankedPages<-plot.text.list[order(CosineCoeff,decreasing=TRUE)]

EuclCoeff<-0*(1:no.plot.text.list)

for (i in 1:no.plot.text.list) {
  EuclCoeff[i]<-sum((Filt_m[i,]-Filt_m[8,])^2)^0.5
}

order(EuclCoeff)
PageRank<-EuclCoeff[order(EuclCoeff)]
RankedPages<-plot.text.list[order(EuclCoeff)]
PageRank
RankedPages