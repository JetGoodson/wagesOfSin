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
  source("toolBox.R")
  source("wordBag.R")
  
  loadSavedTrainData <- FALSE #load training data from saved instead of csv


  ###################################################### end config

  if(loadSavedTrainData == FALSE) {
    trainFrame <- read.csv("data/Train_rev1.csv", header=TRUE, skip=0, stringsAsFactors=FALSE, nrows=2000)
    cat(c(nrow(trainFrame), " rows in this sucker.\n"))
    trainFrame <- trainFrame[,!(colnames(trainFrame) %in% c("Id", "SalaryRaw", "LocationRaw"))] #not keen on these right now, maybe later, 

    trainFrame <-cbind(trainFrame$SalaryNormalized, trainFrame[,!(colnames(trainFrame) %in% c("SalaryNormalized"))]) 
    
    trainFrame <- transmogrifyFrame(trainFrame)

    
    save(trainFrame, file="data/table_train.rda")   #save so we can use the smaller version
    cat("Saved training data as R-object\n")    
  }
  if(loadSavedTrainData == TRUE) {   #load the saved bit
    cat("Loading:\n")
    print(load("data/table_train.rda"))
    cat("Loaded\n")
  }



  
#print(head(trainFrame))
  
 #gives a vector of words and their frequency in the corpus = all the FullDescription field combined

  ##descriptionBag <- wordBag(trainFrame[,!(colnames(trainFrame) %in% c("ContractType","ContractTime","LocationNormalized","SalarayNormalized"))], "descriptionBag")
#providing the word bag for analysis... I think doing the hashing trick will be better for constructing features

#print(trainFrame$Category)
  
#empBag <-  wordBag(trainFrame[,!(colnames(trainFrame) %in% c("ContractType","LocationNormalized","FullDescription","Company","ContractTime"))], "descriptionBag")
#print(empBag)


  
#print(trainFrame$LocationNormalized)

#  
  return("Stipendium peccati mors est.")
} #end of wages of sin
