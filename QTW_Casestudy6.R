library(plyr)
library(sqldf)
library(cluster) 
library(dbscan)

N <- 1500

data <- read.csv('/Users/ramya/Documents/SMU/QTW/Week1/CaseStudy6_2/docword.nips.txt',sep = ' ', skip=3, header=F)
colnames(data) <- c("doc","word", "count")
head(data, n=10)

#Creating column tf and calculating the value for tf as ln(1+count)
data$tf <- log(1+data2$count)
head(data, n=10)

#Calculate n value --> number of times a word appeared in all documents(across docs)
nCount <- count(data, "word")
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

#kmeans with 5 clusters
set.seed(20)
clusters <- kmeans(tfidfDatafinal, 5)

# Save the cluster number in the dataset as column 'Borough'
tfidfDatafinal$kcluster <- as.factor(clusters$cluster)
head(tfidfDatafinal, n=10)

tfidfDatafinal2 <- tfidfDatafinal[c(1,2)]
head(tfidfDatafinal2, n=10)
str(clusters)

# vary parameters for most readable graph

clusplot(tfidfDatafinal2, clusters$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

#kmeans with 10 clusters
set.seed(10)
clusters1 <- kmeans(tfidfDatafinal, 5)
tfidfDatafinal2 <- tfidfDatafinal[c(1,2)]
head(tfidfDatafinal2, n=10)
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

#Running with test data
tfidfTestdata <- tfidfDatafinal[c(1,2)]
tail(tfidfTestdata, n=10)

#DBSCAN
## find suitable eps parameter using a k-NN plot for k = dim + 1
## Look for the knee!
#References: https://en.proft.me/2017/02/3/density-based-clustering-r/
#https://www.rdocumentation.org/packages/dbscan/versions/1.1-2/topics/dbscan
kNNdistplot(tfidfTestdata, k = 3)
abline(h=.5, col = "red", lty=2)

set.seed(1234)
res <- dbscan(tfidfTestdata, eps = 3, minPts = 100000)
hullplot(tfidfTestdata, res$cluster)

max(res$cluster)
pairs(iris, col = res$cluster + 1L)
