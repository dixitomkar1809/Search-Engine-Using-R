# Search Engine# Pretend this is Big Data:

doc1 <- "Stray cats are running all over the place. I see 10 a day!"
doc2 <- "Cats are killers. They kill billions of animals a year."
doc3 <- "The best food in Columbus, OH is   the North Market."
doc4 <- "Brand A is the best tasting cat food around. Your cat will love it."
doc5 <- "Buy Brand C cat food for your cat. Brand C makes healthy and happy cats."
doc6 <- "The Arnold Classic came to town this weekend. It reminds us to be healthy."
doc7 <- "I have nothing to say. In summary, I have told you nothing."

#and this is the Big File System:

doc.list <- list(doc1, doc2, doc3, doc4, doc5, doc6, doc7)
N.docs <- length(doc.list)
N.docs
names(doc.list) <- paste0("doc", c(1:N.docs))
query <- "Healthy cat food"

#Load the tm package into memory.
library(tm)

#Make  A Corpus

my.docs <- VectorSource(c(doc.list, query))
my.docs$Names <- c(names(doc.list), "query")
my.docs
my.corpus <- Corpus(my.docs)
my.corpus#Clean and Transform the cORPUS## Convert to Lower Case
my.corpus2<- tm_map(my.corpus, tolower)
my.corpus2

## Remove Stopwords
my.corpus3<- tm_map(my.corpus2, removeWords, stopwords("english"))

## Remove Punctuations
my.corpus4<- tm_map(my.corpus3, removePunctuation)

## Remove Numbers
my.corpus5<- tm_map(my.corpus4, removeNumbers)## Eliminating Extra White Spaces
my.corpus6<- tm_map(my.corpus5, stripWhitespace)#remove plurals
library(SnowballC)
my.corpus7 <- tm_map(my.corpus6, stemDocument)
dtm <- DocumentTermMatrix(my.corpus7)
dtm
inspect(dtm)## do tfxidf
dtm_tfxidf <- weightTfIdf(dtm)
inspect(dtm_tfxidf)

#Calculate pagerank
m<-(as.matrix(dtm_tfxidf))
Filt_m <-m[,m[8,]>0]
Filt_m
CosineCoeff<-0*(1:N.docs)
for (i in 1:N.docs) {
  CosineCoeff[i]<-sum(Filt_m[i,]*Filt_m[8,])/(sum(Filt_m[i,]^2)^0.5 *sum(Filt_m[8,]^2)^0.5)
}
PageRank<-CosineCoeff[order(CosineCoeff,decreasing=TRUE)]
RankedPages<-doc.list[order(CosineCoeff,decreasing=TRUE)]
EuclCoeff<-0*(1:N.docs)
for (i in 1:N.docs) {
  EuclCoeff[i]<-sum((Filt_m[i,]-Filt_m[8,])^2)^0.5
}
order(EuclCoeff)
PageRank<-EuclCoeff[order(EuclCoeff)]
RankedPages<-doc.list[order(EuclCoeff)]
PageRank
RankedPages
