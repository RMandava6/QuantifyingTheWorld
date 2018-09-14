library(dplyr)
library(sqldf)
library(cluster) 
library(dbscan)
library(HSAUR)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(caroline)
library(plyr)

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

tfidf3 <- apply(tfidf[2:12376],1:2, function(x) log(1+x))

tfidf2[2:12376]<- tfidf3

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

new_words<-dim(tfidf2)[2] - 1
new_words

for (i in 1:new_words){
  tfidf2[,i+1] <- tfidf2[,i+1] * idf[i]}

tfidf2[1:5, 1:5]

#kmeans 5
set.seed(5)
clusters1 <- kmeans(tfidf2, 5)

tfidfFinal <- tfidf2[1]
head(tfidfFinal)
colnames(tfidfFinal) <- "doc"

tfidfFinal$kcluster <- as.factor(clusters1$cluster)
head(tfidfFinal, n=10)
tail(tfidfFinal, n=10)

clusplot(tfidfFinal, clusters1$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0, plotchar=TRUE, span=TRUE)

#kmeans 10
set.seed(10)
clusters2 <- kmeans(tfidf2, 10)

tfidfFinal2 <- tfidf2[1]
head(tfidfFinal2)
colnames(tfidfFinal2) <- "doc"

tfidfFinal2$kcluster <- as.factor(clusters2$cluster)
head(tfidfFinal2, n=10)
tail(tfidfFinal2, n=10)

clusplot(tfidfFinal2, clusters2$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0, plotchar=TRUE, span=TRUE)

hist(as.integer(tfidfFinal2$kcluster), 
     main="Hist", 
     xlab="Docs per clusters", 
     border="blue", 
     col="green",
     breaks=10)

#kmeans 20
set.seed(20)
clusters3 <- kmeans(tfidf2, 20)

tfidfFinal3 <- tfidf2[1]
head(tfidfFinal3)
colnames(tfidfFinal3) <- "doc"

tfidfFinal3$kcluster <- as.factor(clusters3$cluster)
head(tfidfFinal3, n=10)
tail(tfidfFinal3, n=10)

clusplot(tfidfFinal3, clusters3$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0, plotchar=TRUE, span=TRUE)

#DBSCAN
kNNdistplot(tfidf2, k = 16)
abline(h=.5, col = "red", lty=2)

set.seed(1234)
res <- dbscan(tfidf2, eps = 77, minPts =16)
hullplot(tfidf2, res$cluster)


#Reading vocab
dataTxt <- read.csv('/Users/ramya/Documents/SMU/QTW/Week1/CaseStudy6_2/vocab.nips.txt',header=F)
colnames(dataTxt) <- c("wordtext")
head(dataTxt, n=10)

dataTxt2 <-dataTxt
dataTxt2$word <- seq.int(from=1, to=12419, by=1)

data2<-data
dataTxtFinal <- merge(x = data2, y = tfidfFinal2, by = "doc", all = FALSE)
dim(dataTxtFinal)

dataTxtFinal2<- dataTxtFinal
dataTxtFinal2 <- merge(x = dataTxtFinal2, y = dataTxt2, by = "word", all = FALSE)
dim(dataTxtFinal2)

nCount <- count(data, "word")
head(nCount, n=10)


clouddata <- merge(x = dataTxtFinal2, y = nCount, by = "word", all = FALSE)
head(clouddata)

#Creating Data Cloud for 1st cluster
clouddata2<- clouddata[dataTxtFinal2$kcluster == 1,]
tail(clouddata2, n=10)
wordcloud1 <- sqldf("select max(freq) as freq, wordtext from clouddata2 group by wordtext order by freq desc")

set.seed(1234)
wordcloud(words = wordcloud1$wordtext, freq = wordcloud1$freq, min.freq = 200,
          max.words=500, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Data cloud for cluster =2
#Creating Data Cloud for 2nd cluster
clouddata2<- clouddata[dataTxtFinal2$kcluster == 2,]
tail(clouddata2, n=10)
wordcloud1 <- sqldf("select max(freq) as freq, wordtext from clouddata2 group by wordtext order by freq desc")

set.seed(1234)
wordcloud(words = wordcloud1$wordtext, freq = wordcloud1$freq, min.freq = 200,
          max.words=500, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Data cloud for cluster =3
#Creating Data Cloud for 2nd cluster
clouddata2<- clouddata[dataTxtFinal2$kcluster == 3,]
tail(clouddata2, n=10)
wordcloud1 <- sqldf("select max(freq) as freq, wordtext from clouddata2 group by wordtext order by freq desc")

set.seed(1234)
wordcloud(words = wordcloud1$wordtext, freq = wordcloud1$freq, min.freq = 200,
          max.words=500, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Data cloud for cluster =4
#Creating Data Cloud for 2nd cluster
clouddata2<- clouddata[dataTxtFinal2$kcluster == 4,]
tail(clouddata2, n=10)
wordcloud1 <- sqldf("select max(freq) as freq, wordtext from clouddata2 group by wordtext order by freq desc")

set.seed(1234)
wordcloud(words = wordcloud1$wordtext, freq = wordcloud1$freq, min.freq = 200,
          max.words=500, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Data cloud for cluster =5
#Creating Data Cloud for 2nd cluster
clouddata2<- clouddata[dataTxtFinal2$kcluster == 5,]
tail(clouddata2, n=10)
wordcloud1 <- sqldf("select max(freq) as freq, wordtext from clouddata2 group by wordtext order by freq desc")

set.seed(1234)
wordcloud(words = wordcloud1$wordtext, freq = wordcloud1$freq, min.freq = 200,
          max.words=500, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Data cloud for cluster =6
#Creating Data Cloud for 2nd cluster
clouddata2<- clouddata[dataTxtFinal2$kcluster == 6,]
tail(clouddata2, n=10)
wordcloud1 <- sqldf("select max(freq) as freq, wordtext from clouddata2 group by wordtext order by freq desc")

set.seed(1234)
wordcloud(words = wordcloud1$wordtext, freq = wordcloud1$freq, min.freq = 200,
          max.words=500, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Data cloud for cluster =7
#Creating Data Cloud for 2nd cluster
clouddata2<- clouddata[dataTxtFinal2$kcluster == 7,]
tail(clouddata2, n=10)
wordcloud1 <- sqldf("select max(freq) as freq, wordtext from clouddata2 group by wordtext order by freq desc")

set.seed(1234)
wordcloud(words = wordcloud1$wordtext, freq = wordcloud1$freq, min.freq = 200,
          max.words=500, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Data cloud for cluster =8
#Creating Data Cloud for 2nd cluster
clouddata2<- clouddata[dataTxtFinal2$kcluster == 8,]
tail(clouddata2, n=10)
wordcloud1 <- sqldf("select max(freq) as freq, wordtext from clouddata2 group by wordtext order by freq desc")

set.seed(1234)
wordcloud(words = wordcloud1$wordtext, freq = wordcloud1$freq, min.freq = 200,
          max.words=500, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


# Data cloud for cluster =9
#Creating Data Cloud for 2nd cluster
clouddata2<- clouddata[dataTxtFinal2$kcluster == 9,]
tail(clouddata2, n=10)
wordcloud1 <- sqldf("select max(freq) as freq, wordtext from clouddata2 group by wordtext order by freq desc")

set.seed(1234)
wordcloud(words = wordcloud1$wordtext, freq = wordcloud1$freq, min.freq = 200,
          max.words=500, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


# Data cloud for cluster =10
#Creating Data Cloud for 2nd cluster
clouddata2<- clouddata[dataTxtFinal2$kcluster == 10,]
tail(clouddata2, n=10)
wordcloud1 <- sqldf("select max(freq) as freq, wordtext from clouddata2 group by wordtext order by freq desc")

set.seed(1234)
wordcloud(words = wordcloud1$wordtext, freq = wordcloud1$freq, min.freq = 200,
          max.words=500, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
