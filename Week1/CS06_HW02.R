library(plyr)
library(sqldf)
library(cluster) 
library(dbscan)

N <- 1500

data <- read.csv('/Users/ramya/Documents/SMU/QTW/Week1/CaseStudy6_2/docword.nips.txt',sep = ' ', skip=3, header=F)
colnames(data) <- c("doc","word", "count")
head(data, n=10)

#Creating column tf and calculating the value for tf as ln(1+count)
data$tf <- log(1+data$count)
head(data, n=10)

#Calculate n value --> number of times a word appeared in all documents(across docs)
nCount <- count(data, "word")
head(nCount, n=10)

#Calculate idf for each word and assign it to new data frame 'idfData'
nCount$idf <- log(1 + (N/nCount$freq))
head(nCount, n=10)
idfData <- nCount
head(idfData, n=10)

tfidfdata <- merge(x = data, y = idfData, by = "word", all = TRUE)
head(tfidfdata, n=10)

#Calculate tf-idf scores per word per doc
tfidfdata$tfIdf <- tfidfdata$tf*tfidfdata$idf
head(tfidfdata,n=10)

#Assign final matrix to tfidfData
tfidfData2 <- tfidfdata[c(1,2,4,6,7)]
head(tfidfData2, n=10)

tfidfDatafinal <- tfidfData2[c(1,2,5)]
head(tfidfDatafinal, n=10)

#Reading vocab
dataTxt <- read.csv('/Users/ramya/Documents/SMU/QTW/Week1/CaseStudy6_2/vocab.nips.txt',header=F)
colnames(dataTxt) <- c("wordtext")
head(dataTxt, n=10)

dataTxt2 <-dataTxt

tfidfdata
inspect(dataTxt)

dataTxt2$word <- seq.int(from=1, to=12419, by=1)

#Running K-means
#word cloud on kmeans 10
set.seed(10)
clusters1 <- kmeans(tfidfDatafinal, 10)
tfidfDatafinal4 <- tfidfDatafinal


tfidfDatafinal4$kcluster <- as.factor(clusters1$cluster)
head(tfidfDatafinal4, n=10)
tail(tfidfDatafinal4, n=10)

#Merging kmeans results with vocab dataframe
dataTxtFinal <- merge(x = tfidfDatafinal4, y = dataTxt2, by = "word", all = FALSE)
head(dataTxtFinal)


tfidfDatafinal4$kcluster<- as.numeric(tfidfDatafinal4$kcluster)
hist(tfidfDatafinal4$kcluster, 
     main="Hist", 
     xlab="clusters", 
     border="blue", 
     col="green",
     breaks=10)

count1 <- count(tfidfDatafinal4, "word")
count1$freq2 <- count1$freq
count1<-count1[c(1,3)]
count1<- count1[order(-count1$freq),]
head(count1, n=10)

dataTxtFinal2 <- merge(x = count1, y = dataTxtFinal, by = "word", all = FALSE)
head(dataTxtFinal2, n=10)

clouddata <- dataTxtFinal2[dataTxtFinal2$kcluster == 1,]
tail(clouddata, n=10)
hist(clouddata$freq, 
     main="Histogram for words", 
     xlab="words", 
     border="blue", 
     col="green",
     breaks=20)

count2<-count(tfidfDatafinal4, "word")
head(count2, n=10)

dataTxtcloud <- merge(x = count2, y = dataTxt2, by = "word", all = FALSE)
head(dataTxtcloud, n=10)
dataTxtcloud<- dataTxtcloud[order(-dataTxtcloud$freq),]
head(dataTxtcloud, n=10)

set.seed(12345)
wordcloud(words = dataTxtcloud$wordtext, freq = dataTxtcloud$freq, min.freq = 100,
          max.words=1000, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
