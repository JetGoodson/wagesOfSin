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
    trainFrame <- read.csv("data/Train_rev1.csv", header=TRUE, skip=0, stringsAsFactors=FALSE)#, nrows=20000)
    cat(c(nrow(trainFrame), " rows in this sucker.\n"))
    trainFrame <- trainFrame[,!(colnames(trainFrame) %in% c("Id", "SalaryRaw", "LocationRaw","SourceName"))] #not keen on these right now, maybe later, 
 
    trainFrame$FullDescription <- gsub("[_,\\*]", "", trainFrame$FullDescription)
    trainFrame$FullDescription <- gsub("(((file|gopher|news|nntp|telnet|http|ftp|https|ftps|sftp)://)|(www\\.))+(([a-zA-Z0-9\\._-]+\\.[a-zA-Z]{2,6})|([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}))(/[a-zA-Z0-9\\&amp;%_\\./-~-]*)?", "", trainFrame$FullDescription)
    trainFrame$FullDescription <- gsub("^.*(.com|.co.uk).*$", "", trainFrame$FullDescription)
    trainFrame$FullDescription <- gsub("/", " ", trainFrame$FullDescription)

     
    trainFrame$Category <- gsub("Accounting & Finance Jobs", "1", trainFrame$Category)
    trainFrame$Category <- gsub("Admin Jobs", "2", trainFrame$Category)
    trainFrame$Category <- gsub("Charity & Voluntary Jobs", "3", trainFrame$Category)
    trainFrame$Category <- gsub("Consultancy Jobs", "4", trainFrame$Category)
    trainFrame$Category <- gsub("Creative & Design Jobs", "5", trainFrame$Category)
    trainFrame$Category <- gsub("Customer Services Jobs", "6", trainFrame$Category)
    trainFrame$Category <- gsub("Domestic Help & Cleaning Jobs", "7", trainFrame$Category)
    trainFrame$Category <- gsub("Energy, Oil & Gas Jobs", "8", trainFrame$Category)
    trainFrame$Category <- gsub("Engineering Jobs", "9", trainFrame$Category)
    trainFrame$Category <- gsub("Graduate Jobs", "10", trainFrame$Category)
    trainFrame$Category <- gsub("Healthcare & Nursing Jobs", "11", trainFrame$Category)
    trainFrame$Category <- gsub("Hospitality & Catering Jobs", "12", trainFrame$Category)
    trainFrame$Category <- gsub("HR & Recruitment Jobs", "13", trainFrame$Category)
    trainFrame$Category <- gsub("IT Jobs", "14", trainFrame$Category)
    trainFrame$Category <- gsub("Legal Jobs", "15", trainFrame$Category)
    trainFrame$Category <- gsub("Logistics & Warehouse Jobs", "16", trainFrame$Category)
    trainFrame$Category <- gsub("Maintenance Jobs", "17", trainFrame$Category)
    trainFrame$Category <- gsub("Manufacturing Jobs", "18", trainFrame$Category)
    trainFrame$Category <- gsub("Other/General Jobs", "19", trainFrame$Category)
    trainFrame$Category <- gsub("PR, Advertising & Marketing Jobs", "20", trainFrame$Category)
    trainFrame$Category <- gsub("Property Jobs", "21", trainFrame$Category)
    trainFrame$Category <- gsub("Retail Jobs", "22", trainFrame$Category)
    trainFrame$Category <- gsub("Sales Jobs", "23", trainFrame$Category)
    trainFrame$Category <- gsub("Scientific & QA Jobs", "24", trainFrame$Category)
    trainFrame$Category <- gsub("Social Work Jobs", "25", trainFrame$Category)
    trainFrame$Category <- gsub("Teaching Jobs", "26", trainFrame$Category)
    trainFrame$Category <- gsub("Trade & Construction Jobs", "27", trainFrame$Category)
    trainFrame$Category <- gsub("Travel Jobs" , "28", trainFrame$Category)
    

    
    trainFrame$ContractTime <- gsub("contract" , "1", trainFrame$Category)
    trainFrame$ContractTime <- gsub("permanent" , "2", trainFrame$Category)
    
    trainFrame$ContractType <- gsub("part_time" , "2", trainFrame$Category)
    trainFrame$ContractType <- gsub("full_time" , "2", trainFrame$Category)
    




    
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
