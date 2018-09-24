library(XML)
library (plyr)  
library(gdata)
library(ggplot2)
library(gridExtra)

w2012 = read.table(file="/Users/ramya/Documents/GitHub/QuantifyingTheWorld_CS1/Week2/data/2012.txt", skip = 8)

els = readLines("/Users/ramya/Documents/GitHub/QuantifyingTheWorld_CS1/Week2/data/2001.txt")
els[1:20]

findColLocs = function(spacerRow) {
  
  spaceLocs = gregexpr(" ", spacerRow)[[1]]
  rowLength = nchar(spacerRow)
  
  if (substring(spacerRow, rowLength, rowLength) != " ")
    return( c(0, spaceLocs, rowLength + 1))
  else return(c(0, spaceLocs))
}

selectCols = 
  function(colNames, headerRow, searchLocs) 
  {
    sapply(colNames, 
           function(name, headerRow, searchLocs)
           {
             startPos = regexpr(name, headerRow)[[1]]
             if (startPos == -1) 
               return( c(NA, NA) )
             
             index = sum(startPos >= searchLocs)
             c(searchLocs[index] + 1, searchLocs[index + 1] - 1)
           },
           headerRow = headerRow, searchLocs = searchLocs )
  }


extractVariables = 
  function(file, varNames =c("name", "ag"))
  {
    # Find the index of the row with =s
    eqIndex = grep("^===", file)
    # Extract the two key rows and the data
    spacerRow = file[eqIndex] 
    headerRow = tolower(file[ eqIndex - 1 ])
    body = file[ -(1 : eqIndex) ]
    
    # Obtain the starting and ending positions of variables
    searchLocs = findColLocs(spacerRow)
    locCols = selectCols(varNames, headerRow, searchLocs)
    
    Values = mapply(substr, list(body), start = locCols[1, ], 
                    stop = locCols[2, ])
    colnames(Values) = varNames
    
    invisible(Values)
  }

wfilenames = paste("/Users/ramya/Documents/GitHub/QuantifyingTheWorld_CS1/Week2/data/", 1999:2002, ".txt", sep = "")
womenFiles = lapply(wfilenames, readLines)
names(womenFiles) = 1999:2002

womenResMat = lapply(womenFiles, extractVariables)
length(womenResMat)

sapply(womenResMat, nrow)
head(womenResMat)
