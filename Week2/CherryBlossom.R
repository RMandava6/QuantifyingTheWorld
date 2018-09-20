library(XML)
library (plyr)  
library(gdata)

ubase = "http://www.cherryblossom.org/"
womenURLs = 
  c("results/1999/cb99f.html", "results/2000/Cb003f.htm", "results/2001/oof_f.html",
    "results/2002/ooff.htm", "results/2003/CB03-F.HTM",
    "results/2004/women.htm", "results/2005/CB05-F.htm", 
    "results/2006/women.htm", "results/2007/women.htm", 
    "results/2008/women.htm", "results/2009/09cucb-F.htm",
    "results/2010/2010cucb10m-f.htm", 
    "results/2011/2011cucb10m-f.htm",
    "results/2012/2012cucb10m-f.htm")

# http://www.cherryblossom.org/results/2000/Cb003f.htm
#http://www.cherryblossom.org/results/2009/09cucb-F.htm

urls = paste(ubase, womenURLs, sep = "")

urls[1:3]

urls

extractResTable =
  # takes a list of websites from the cherry blossom race
  # a list of years corresponding to the year the result is for
  # and the gender of the participant
  # Retrieve data from web site, 
  # find the preformatted text,
  # and write lines or return as a character vector.
  # returns a list of strings corrsponding to lines in the web url
  function(url = "http://www.cherryblossom.org/results/2009/09cucb-F.htm",
           year = 1999, sex = "female", file = NULL)
  {
    doc = htmlParse(url)
    
    if (year == 2000) {
      # Get preformatted text from 4th font element
      # The top file is ill formed so the <pre> search doesn't work.
      ff = getNodeSet(doc, "//font")
      txt = xmlValue(ff[[4]])
      els = strsplit(txt, "\r\n")[[1]]
      print(year)
    }
    else if (year == 1999) {
      # Get preformatted text from <pre> elements
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\n")[[1]]   
      print(year)
    } 
    else {
      # Get preformatted text from <pre> elements
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\r\n")[[1]]   
      print(year)
    } 
    
    if (is.null(file)) return(els)
    # Write the lines as a text file.
      writeLines(els, con = file)
  }

years = 1999:2012
womenTables = mapply(extractResTable, url = urls, year = years)
names(womenTables) = years
sapply(womenTables, length)


save(womenTables, file = "/Users/ramya/Desktop/CBMenTextTables.rda")

capture.output(womenTables, file = "CBWomenTextTables4.xls")


df <- ldply (womenTables, data.frame)
df1 <- df

df1 <- df1[-c(0:4), ]

colnames(df1) <- c("YEAR","PLACE","DIV /TOT","NAME", "AGE", "HOMETOWN" , "TIME", "PACE")


myData = read.csv("/Users/ramya/Desktop/DataFinal.csv", header = TRUE, sep = ",")

dataFinal <- na.omit(myData)


dfData <- ldply (dataFinal, data.frame)
colnames(dfData) <- c("Year", "Age")

dfData$Year <- as.numeric(substring(dfData$Year, 2))

as.numeric()

#QQ Plot of Ages

#Box Plots
boxplot(Age~Year, data=dfData, main="Ages over Years - Running stats", 
        xlab="Years", ylab="Ages")

