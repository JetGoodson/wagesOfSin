#################################
# lojack.R
# written by Jet Goodson 
# started on 7 March 2013
# contact at jetgoodson@gmail.com
#
# toolset for building a place name
# to lat/long coords look up table
# and other geospatial tools
##################################

##Contains Ordnance Survey data © Crown copyright and database right 2010
#Contains Royal Mail data © Royal Mail copyright and database right 2010
#Contains National Statistics data © Crown copyright and database right 2010


#native test function for testing other functions
lojack <- function() {

  coordinates <- createCoordinateTable()
  print(head(coordinates))
  locations <- createLocationTable()
  print(head(locations))
 
  for(i in 1:nrow(locations)){
     
    result <- coordinateCheck(locations[i,])
    cat("got results \n")
    if(!(result == "okay")){
     cat(c(locations[i,2], "~", locations[i,3], "~", locations[i,4], "~", locations[i,5], "~", locations[i,6], "~", locations[i,7], "~", locations[i,8], " ~~~ ", result, "\n"))
   }
  }
library(plyr)
  library(tm)
  
source("wordBag.R")
locWords <- wordBag(locations, "locWords")
locationTable <- read.table("data/Location_Tree.csv", header=FALSE, sep="~", quote="", stringsAsFactors=FALSE,fill=TRUE)
locationTable$V1 <- gsub('"', "", locationTable$V1)
locationTable$V4 <- gsub('"', "", locationTable$V4)
  
} #end of lojack test function


#creates a wordBag of location name
createLocationWordBag <- function() {
  library(plyr)
  library(tm)
 
  source("wordBag.R")
  locationTable <- read.table("data/Location_Tree.csv", header=FALSE, sep="~", quote="", stringsAsFactors=FALSE,fill=TRUE)
  locationTable$V1 <- gsub('"', "", locationTable$V1)
  locationTable$V4 <- gsub('"', "", locationTable$V4)
  
 
  wordBags <- vector()
  i <- 1
  while(i < nrow(locationTable)){
    high <- i + 100;
    if(high > nrow(locationTable)){
      high <- nrow(locationTable)
    }
    wBag <- wordBag(locationTable[i:high,], "locWords",doStem=FALSE)
    wordBags <- union(wordBags, wBag)
    i <- i + 101
    cat(c("Finished with ", i/nrow(locationTable), "%\n"))
  }
  
  print(wordBags)
  
  locWords <- wordBags
  
#locWords <- wordBag(locationTable, "locWords")
save(locWords, file="data/locationWordBag.rda")
}#end of createLocationWordBag


#creates the place name to coordinate look up table
createCoordinateTable <- function() {
coordTable <- read.table("data/STRATEGI_2010_GAZETTEER.TXT", header=FALSE, sep="*", quote="", stringsAsFactors=FALSE)
cat("Saving coordinate table\n")
save(coordTable, file="data/coordinateTable.rda")
#table gives place name, county, and northing and easting
return(coordTable)
}#end of createCoordinateTable

#create lookup table for location texts
createLocationTable <- function() {
locationTable <- read.table("data/Location_Tree.csv", header=FALSE, sep="~", quote="", stringsAsFactors=FALSE,fill=TRUE)
locationTable$V1 <- gsub('"', "", locationTable$V1)
locationTable$V2 <- gsub('"', "", locationTable$V2)
locationTable$V3 <- gsub('"', "", locationTable$V3)
locationTable$V4 <- gsub('"', "", locationTable$V4)
locationTable$V5 <- gsub('"', "", locationTable$V5)

cat("Saving location table\n")
save(locationTable, file="data/locationTable.rda")
return(locationTable)
}#end of createLocationTable


coordinateCheck <- function(input) {

  load("data/coordinateTable.rda")
  name <- ""
  j <- ncol(input)
  name <- input[,j]
  while(name == ""){
    j <- j - 1
    name <- input[,j]
  }
 
results <- coordTable[coordTable$V1 == "zzzz",]
  while(nrow(results) == 0) {
    name <- input[,j]
cat(c(name, "\n"))
    results <- coordTable[coordTable$V1 == name,]
    print(results)
    j <- j - 1
  }

  if(nrow(results) == 1) {
    return("okay");
  }
  return(name)
} #end of coordinate check
