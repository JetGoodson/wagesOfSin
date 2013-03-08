#################################
# wagesOfSin.R
# written by Jet Goodson 
# started on 7 March 2013
# contact at jetgoodson@gmail.com
#
# control file for wagesOfSin package
##################################

wagesOfSin <- function() {
  library("tm")        #data mining library for nlp
  source("toolbox.R")
  source("wordBag.R")
  
  loadSavedTrainData <- TRUE #load training data from saved instead of csv


  ###################################################### end config

  if(loadSavedTrainData == FALSE) {
    trainFrame <- read.csv("Train_rev1.csv", header=TRUE,skip=0,stringsAsFactors=FALSE)
    trainFrame <- trainFrame[,!(colnames(trainFrame) %in% c("Id", "Title", "SalaryRaw", "LocationRaw","SourceName"))] #not keen on these right now, maybe later
    trainFrame <- trainFrame[1:100,]   #only select  100 for now

    trainFrame$FullDescription <- gsub("www.*", "", trainFrame$FullDescription)     
    trainFrame$FullDescription <- gsub("*.com", "", trainFrame$FullDescription)     
   trainFrame$FullDescription <- gsub("*.co.uk", "", trainFrame$FullDescription)     #get rid of URLs in the descriptor, the one below should be better, but it doesn't work
#trainFrame$FullDescription <- gsub("(http://|)(www\\.)?([^\\.]+)\\.(\\w{2}|(com|net|org|edu|int|mil|gov|arpa|biz|aero|name|coop|info|pro|museum|co.uk))$", "", trainFrame$FullDescription)
#(http://|)(www\.)?([^\.]+)\.(\w{2}|(com|net|org|edu|int|mil|gov|arpa|biz|aero|name|coop|info|pro|museum))$
    
    save(trainFrame, file="table_train.rda")   #save so we can use the smaller version
    cat("Saved training data as R-object\n")    
  }
  if(loadSavedTrainData == TRUE) {   #load the saved bit
    cat("Loading:\n")
    print(load("table_train.rda"))
    cat("Loaded\n")
  }

  
 #gives a vector of words and their frequency in the corpus = all the FullDescription field combined
  descriptionBag <- wordBag(trainFrame[,!(colnames(trainFrame) %in% c("ContractType","ContractTime","LocationNormalized","SalarayNormalized"))], "descriptionBag")
#providing the word bag for analysis... I think doing the hashing trick will be better for constructing features


print(trainFrame$LocationNormalized)

  
  return("Stipendium peccati mors est.")
} #end of wages of sin
