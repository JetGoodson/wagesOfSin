#################################
# wagesOfSin.R
# written by Jet Goodson 
# started on 7 March 2013
# contact at jetgoodson@gmail.com
#
# control file for wagesOfSin package
##################################

wagesOfSin <- function() {

  ###########################################################configure
  nEntries         <- 250000  #number of training entries to run over 
  proportion       <- 80/100   #split for train and validation
  samplingSize     <- 10000    #how many events is each tree made of; can save processing memory and time
  
  analyzeData      <- TRUE   #run analyzer on data; produces histograms of salary distribution vs word
  
  outputLog        <- "logFile.dat"
   
  predictionFileRF   <- "test_RF_salaryOutput.csv"
  predictionFileSVM   <- "test_SVM_salaryOutput.csv"
  
  loadSavedTrainData <- FALSE #load training data from saved instead of csv
  loadSavedTestData <- FALSE #load training data from saved instead of csv
  
  ################################################end of configuration
  sink(outputLog, append=TRUE, split=TRUE)
  
  
  library("tm")        #data mining library for nlp
  library("caret")
  source("toolBox.R")
  source("wordBag.R")
  source("randomOrchard.R")
  source("cogInTheMachine.R")
   
 
  
  if(loadSavedTrainData == FALSE) {
    trainFrame <- read.csv("data/Train_rev1.csv", header=TRUE, skip=0, stringsAsFactors=FALSE, nrows=nEntries)
    cat(c(nrow(trainFrame), " rows in this sucker.\n"))
    
    
    trainFrame <- trainFrame[,!(colnames(trainFrame) %in% c("Id", "SalaryRaw", "LocationRaw"))] #not keen on these right now, maybe later, 
    
    trainFrame <- killDoppelgangers(trainFrame) #get rid of duplicate entries

    
    
    trainFrame <-cbind(trainFrame$SalaryNormalized, trainFrame[,!(colnames(trainFrame) %in% c("SalaryNormalized"))]) 
    
    trainFrame <- transmogrifyFrame(trainFrame)
     
    colnames(trainFrame)[1] <- "SalaryNormalized" #seems to get messed up


    row.has.na <- apply(trainFrame, 1, function(x){any(is.na(x))})
    row.has.na2 <- row.has.na[row.has.na == TRUE]
    cat(c(length(row.has.na2), " number of NA rows\n"))
    print(head(trainFrame[row.has.na, ]))

  
    
    save(trainFrame, file="data/table_train.rda")   #save so we can use the smaller version
    cat("Saved training data as R-object\n")    
  }
  if(loadSavedTrainData == TRUE) {   #load the saved bit
    cat("Loading:\n")
    print(load("data/table_train.rda"))
    cat("Loaded\n")
  }

  ####get and configure test data
  if(loadSavedTestData == FALSE){
  testFrame <- read.csv("data/Valid_rev1.csv", header=TRUE, skip=0, stringsAsFactors=FALSE)
  jobIdents <- testFrame[,1] #save job ids for later
  testFrame <- transmogrifyFrame(testFrame)
  save(testFrame, jobIdents, file="data/table_test.rda")
  }
  if(loadSavedTestData == TRUE) {   #load the saved bit
    cat("Loading:\n")
    print(load("data/table_test.rda"))
    cat("Loaded\n")
  }

   ###########################do some analysis
  
   if(analyzeData == TRUE){
     analyzer(trainFrame, "featurePlots")
   }

   ######################################split training data into a training set and a performance validation set
  set.seed(1)
  splitdex <- createDataPartition(trainFrame[,1], p=proportion, list=FALSE)
  validationFrame <- trainFrame[-splitdex,]
  trainFrame <- trainFrame[splitdex, ]
  cat(c("Training set partitioned into ", nrow(trainFrame), " for training and ", nrow(validationFrame), " for validation\n"))
  
   ################################################################## Random Forest 

  rfModel <- randomOrchard(trainFrame)

  validateOrchard(rfModel, validationFrame)

  orchardOutput <- orchardPredict(rfModel, testFrame, jobIdents)
  write.csv(orchardOutput, file=predictionFileRF, row.names=FALSE,quote=FALSE)
  cat(c("Wrote predictions to file ", predictionFileRF, "\n"))


  ########################################################### SVM

  #svmModel <- cogInTheMachine(trainFrame)

#  validateSVM(rfModel, validationFrame)

 # svmOutput <- svmPredict(rfModel, testFrame, jobIdents)
  #write.csv(svmOutput, file=predictionFileSVM, row.names=FALSE,quote=FALSE)
  #cat(c("Wrote predictions to file ", predictionFileSVM, "\n"))







  
  return("Stipendium peccati mors est.")
} #end of wages of sin
