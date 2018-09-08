library(plyr)
library(sqldf)

N <- 1500

data <- read.csv('/Users/ramya/Documents/SMU/QTW/Week1/CaseStudy6_2/docword.nips.txt',sep = ' ', skip=3, header=F)
colnames(data) <- c("doc","word", "count")
head(data, n=10)

#Creating column tf and calculating the value for tf as ln(1+count)
data2 <- data
data2$tf <- log(1+data2$count)

#Calculate n value --> number of times a word appeared in all documents(across docs)
data3 <-data
nCount <- count(data3, "word")
head(nCount, n=10)

#Calculate idf for each word and assign it to new data frame 'idfData'
nCount$idf <- log(1 + (N/nCount$freq))
head(nCount, n=10)
idfData <- nCount
head(idfData, n=10)

tfidfdata <- merge(x = data2, y = idfData, by = "word", all = TRUE)
head(tfidfdata, n=10)

#Calculate tf-idf scores per word per doc
tfidfdata$tfIdf <- tfidfdata$tf*tfidfdata$idf
head(tfidfdata,n=10)

#Assign final matrix to tfidfData
tfidfData <- tfidfdata[c(1,2,4,6,7)]
head(tfidfData, n=10)

tfidfDatafinal <- tfidfData[c(1,5)]
head(tfidfDatafinal, n=10)

set.seed(20)
clusters <- kmeans(tfidfDatafinal, 5)

# Save the cluster number in the dataset as column 'Borough'
tfidfDatafinal$kcluster <- as.factor(clusters$cluster)
head(tfidfDatafinal, n=10)

tfidfDatafinal2 <- tfidfDatafinal[c(1,2)]
str(clusters)

#Used
library(cluster)
library(fpc)

# Fig 01
plotcluster(tfidfDatafinal[100, c(1,3)], clusters$cluster)

# vary parameters for most readable graph
library(cluster) 
clusplot(tfidfDatafinal2, clusters$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

#future use
library(cluster)
library(HSAUR)
data(pottery)
km    <- kmeans(pottery,3)
dissE <- daisy(pottery) 
dE2   <- dissE^2
sk2   <- silhouette(km$cl, dE2)
plot(sk2)

