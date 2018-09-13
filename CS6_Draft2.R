library(plyr)
library(sqldf)
library(cluster) 
library(dbscan)

N <- 1500

data <- read.csv('/Users/ramya/Documents/SMU/QTW/Week1/CaseStudy6_2/docword.nips.txt',sep = ' ', skip=3, header=F)
colnames(data) <- c("doc","word", "count")
head(data, n=10)

docs<-max(data[,1]) #same as above, but store for later
words=max(data[,2]) #find number of unique words
max_count=max(data[,3]) # find the max count, so we can normalize later
tfidf = data.frame(c(1:docs)) #create a dataframe with just the document numbers

head(tfidf)
head(data)

for (i in 1:words){
  label = paste("word_",toString(i),sep="")
  tfidf[label] <- 0}

head(tfidf[,20:30])

dim(tfidf)

for (i in 1:746315){
  row = data[i,1]
  col = data[i,2]
  val = data[i,3]
  tfidf[row,col+1] = val
  if (i %% 20000 == 0){print(toString(i))}
}

dim(tfidf[, colSums(tfidf != 0) > 0])

tfidf<-tfidf[, colSums(tfidf != 0) > 0]

dim(tfidf)

#head(tfidf, n=1)
tfidf2 <- tfidf
tfidf[1:5, 1:5]
dim(tfidf2)
tfidf2 <- apply(tfidf[2:12376],1:2, function(x) log(1+x))
#tfidf2$docs <- tfidf[, 1]
tfidf2[1:5, 1:5]
#tfidf2$c.1.docs. <- seq.int(from=1, to=1500, by=1)

idf<-colSums(tfidf2[-1] != 0)

idf[1]
idf[2]
idf[39]

idf = log(1+ (1/idf)*N)

max(idf)

head(idf)

new_words<-dim(tfidf)[2] - 1
new_words

for (i in 1:new_words){
  tfidf[,i+1] <- tfidf[,i+1] * idf[i]}