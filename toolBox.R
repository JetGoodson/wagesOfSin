#################################
# toolbox.R
# written by Jet Goodson 
# started on 7 March 2013
# contact at jetgoodson@gmail.com
#
# general data handling tools
##################################

#simplify and manipulate the fields
transmogrifyFrame <- function(dataFrame) {

  source("wordBag.R")
  library(prettyR)
  library(plyr)
  library(tm)
  library(Snowball)
  require(stringr)
  stopWords <- stopwords("en")
  class(stopWords)
  
  load(file="data/locationWordBag.rda")#gives locWords

  #commented out, lets let frequency count handle this
 # redundantWordBag <- getWordList(dataFrame$Category, c("&", "Jobs", "/", "  "), c("", "", " ", " ")) #this must be before next line to be useful
 # redundantWordBag <- c(redundantWordBag, "permanent", "full", "part", "time", "contract", "engineer", "financial", "recruiter", "teacher")
  redundantWordBag <- c("contract", "permanent", "part", "full", "time")
  dataFrame$Category <- transmogrifyCategory(dataFrame$Category)
  #want to remove accounting, nursing from list

  ###do location generalized
  dataFrame$LocationNormalized <- locateThis(dataFrame$LocationNormalized)

  #better column names
  dataFrame$ContractType <- gsub("part_time" , "partTimer", dataFrame$ContractType)   
  dataFrame$ContractType <- gsub("full_time" , "fullTimer", dataFrame$ContractType)
  #leave as words the ContractTime column

 
  dataFrame <- cbind(dataFrame[,!(colnames(dataFrame) %in% c("Title", "Category", "ContractTime", "ContractType", "LocationNormalized"))], featureBag(dataFrame[,(colnames(dataFrame) %in% c("Title", "Category", "ContractTime", "ContractType", "LocationNormalized"))]))

  #this is where the hard part starts
  
  #dataFrame$Title <- textCleanup(dataFrame$Title, c(stopWords, locWords, redundantWordBag))
  dataFrame$FullDescription <- textCleanup(dataFrame$FullDescription, c(stopWords, locWords, redundantWordBag))

  dataFrame$Company <- killPunkSpaces(dataFrame$Company)
  dataFrame$SourceName <- killPunkSpaces(dataFrame$SourceName)
  
#  cat(c("The company  column has ", length(freq(dataFrame$Company)[[1]]), " unique entries\n"))
#  cat(c("The source name  column has ", length(freq(dataFrame$SourceName)[[1]]), " unique entries\n"))

  dataFrame <- dataFrame[,!(colnames(dataFrame) %in% c("FullDescription", "SourceName", "Company"))]

  dataFrame <- dataFrame[,!(colnames(dataFrame) %in% c("nana", "full", "part", "time", "jobs"))] #not sure why these stick around
  
  return(dataFrame)
}#end of transmogrify

#getCategories - word bag the categories
getWordList <- function(column, find = c(""), replace = c("")){
  if(length(find) > 0 && length(find) == length(replace)){
    for(i in 1:length(find)) {
      column <- gsub(find[i], replace[i], column)
    }}
  if(length(find) != length(replace)){
    cat("Find and replace lengths do not match\n")
  }
  column <- gsub("Nursing", "other", column) #for later redundancy, want to distinguish accountants from financiers, nurses from doctors
  column <- gsub("Accounting", "other", column)
  wordList <- tolower(unique(unlist(strsplit(column, " "))))
  return(wordList)
} #end of getCategories



#specifically alters 
transmogrifyCategory <- function(column) {
  #I'm turning this into [hopefully] unique identifiers that will make good feature names in columns. And being smartassed.
  column <- gsub("Accounting & Finance Jobs", "fatCats", column)  
  column <- gsub("Admin Jobs", "theMan", column)
  column <- gsub("Charity & Voluntary Jobs", "bleedingHearts", column)
  column <- gsub("Consultancy Jobs", "hiredGuns", column)
  column <- gsub("Creative & Design Jobs", "starvingArtists", column)
  column <- gsub("Customer Services Jobs", "helpDesk", column)
 #column <- gsub("Domestic Help & Cleaning Jobs", "custodialEngineering", column)
  column <- gsub("Domestic help & Cleaning Jobs", "custodialEngineering", column)
  column <- gsub("Energy, Oil & Gas Jobs", "goJuice", column)
  column <- gsub("Engineering Jobs", "beamMeUpScotty", column)
  column <- gsub("Graduate Jobs", "poorBastards", column)
  column <- gsub("Healthcare & Nursing Jobs", "bonesMcCoy", column)
  column <- gsub("Hospitality & Catering Jobs", "normanBates", column)
  column <- gsub("HR & Recruitment Jobs", "headHunters", column)
  column <- gsub("IT Jobs", "nerdHerd", column)
  column <- gsub("Legal Jobs", "legalEagles", column)
  column <- gsub("Logistics & Warehouse Jobs", "inTheRearWithTheGear", column)
  column <- gsub("Maintenance Jobs", "mrFixit", column)
  column <- gsub("Manufacturing Jobs", "assemblyLine", column)
  column <- gsub("Other/General Jobs", "oddJob", column)
  column <- gsub("PR, Advertising & Marketing Jobs", "prFlacks", column)
  column <- gsub("Property Jobs", "groundkeeperWillie", column)
  column <- gsub("Retail Jobs", "poshShopGirl", column)
  column <- gsub("Sales Jobs", "usedCarSalesmen", column)
  column <- gsub("Scientific & QA Jobs", "madScientists", column)
 #column <- gsub("Social Work Jobs", "busyBodies", column)
  column <- gsub("Social work Jobs", "busyBodies", column)
  column <- gsub("Teaching Jobs", "glorifiedBabysitters", column)
  column <- gsub("Trade & Construction Jobs", "doozers", column)
  column <- gsub("Travel Jobs" , "leavinOnAJetPlane", column)
  return(column)
}#end of transmogrifyCategory



#takes location to the second column to minimize features, should be about 30
locateThis <- function(column) {
  load(file="data/locationTable.rda")#gives locWords
  column <- sapply(column, function(x){
    newLoc <- which(locationTable$V4 == x)
    if(length(newLoc) == 0){
      newLoc <- which(locationTable$V3 == x)
      if(length(newLoc) == 0){
        newLoc <- x 
      }}
    if(length(newLoc) > 0){
     newLoc <- locationTable[newLoc[1], 2] 
    }
    #newLoc <- tolower(newLoc)
    newLoc <- strsplit(newLoc, " ")[[1]]
    newLoc <- paste(toupper(substring(newLoc, 1, 1)), substring(newLoc, 2), collapse = "")
  })
  column <- gsub("/" , "", column)
  column <- gsub(" " , "", column)
  column <- gsub("&" , "", column)
  column <- gsub("uk" , "", column) #duh. If there are any ads outside UK, this field won't tell us
  return(column) 
} #end of locateThis


#kill punctuation and spaces
killPunkSpaces <- function(column) {
  column <- gsub("[[:punct:]]", " ", column)  #kill punctuation, do before following to preserve urls in sourcename
  column <- sapply(column, function(x){
    t <- strsplit(x, " ")[[1]]
    t <- paste(toupper(substring(t, 1, 1)), substring(t, 2), sep = "", collapse = "")
  })
  column <- gsub("/\\s/", "", column) #strip excess white space
}#end of killPunkSpaces


#removes words from column that appear in other category, i.e. locations and job categories
wordRemover <- function(column, wordList) {
  '%nin%' <- Negate('%in%')
  column <- lapply(column, function(x) {   
    t <- unlist(strsplit(x, " "))
    t <- t[t %nin% wordList] ##this gets rid of stopwords i.e., common words like article, simple verbs
    t <- paste(t, sep=" ", collapse= " ") 
  })
  return(column)
}#end of word remover


#do all the things to the free text in title and fulldescription, saved from how it was around March 10th, 2013 for reference
textCleanup <- function(column, killWords = c("")) {
  source("wordBag.R")
  
  library(plyr)
  library(tm)
  library(Snowball)
  require(stringr)
  
  column <- tolower(column)   #make it lower case
  
  column <- gsub("[_,\\*]", "", column)  ##get rid of where they replaced salaries with asterisks.
                                        #below gets rid of websites, which I don't think we need to keep, they mess things up
  column <- gsub("(((file|gopher|news|nntp|telnet|http|ftp|https|ftps|sftp)://)|(www\\.))+(([a-zA-Z0-9\\._-]+\\.[a-zA-Z]{2,6})|([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}))(/[a-zA-Z0-9\\&amp;%_\\./-~-]*)?", "", column)
  column <- gsub("^.*(.com|.co.uk).*$", "", column)   #destroy british websites
  column <- gsub("/", " ", column)    #get rid of slashes, confuses things
  column <- gsub("\\.", " ", column)  #get rid of periods
  
  
  '%nin%' <- Negate('%in%')
  column <- lapply(column, function(x) {   
    t <- unlist(strsplit(x, " "))
    #  t <- t[t %nin% stopWords] ##this gets rid of stopwords i.e., common words like article, simple verbs
    t <- t[t %nin% killWords]#this gets rid of locations
    t <- SnowballStemmer(t)
    t <- paste(t, sep=" ", collapse= " ") 
  })

  column <- gsub("[[:punct:]]", " ", column)  #kill punctuation, do before following to preserve urls in sourcename
  column <- gsub("\\d", " ", column)  #get rid of digits
  column <- gsub("/\\s\\s+/", " ", column) #strip excess white space
  column <- str_trim(column)
  
  return(column)
}#end of text cleanup

#get rid of repeated entries in dataset
killDoppelgangers <- function(frame){
  clones <- duplicated(frame)
  clones <- which(clones == TRUE)
  if(length(clones) < 1){
    return(frame)
  }
  cat(c("There are ", nrow(frame[clones,]), " duplicate entries\n"))
  return(frame[-clones, ])
}#end of doppelganger function


#produce some plots
analyzer <- function(frame, folder){
  system(paste(c("mkdir -p ", folder), collapse=""))
  par(bg = "white")
  colNames <- colnames(frame)

  for(i in 2:length(colNames)){
    png(file=paste(c(folder, "/histogram_", colNames[1], "_for_", colNames[i], ".png"), collapse=""), width=800,height=400)
    hist(frame[(frame[,i]==1),1], main=paste(c(colNames[1], " for ", colNames[i]),collapse=""), xlab=paste(c(colNames[1], " [Pounds] "), collapse=""), ylab="entries",col="red3") 
  dev.off()
  }#end of histogram loop

  png(file="hist_salaryDist.png")
  hist(frame[,1], main="Salary Distribution", xlab=paste(c(colNames[1], " [Pounds] "), collapse=""), ylab="entries",col="cornflowerblue") 
  dev.off()
  
  graphics.off()
  
}#end of analyzer


#function to pre-process free text (title and fulldescription) for feature creation by eliminating least common words
cullFreeText <- function(column, wordQuota = 400, goodList = vector(), folder="wordPlots", doPlots=FALSE){

  system(paste(c("mkdir -p ", folder), collapse=""))
  
  library('plyr')
  library(Snowball)
  require(stringr)
  load(file="data/locationWordBag.rda")#gives locWords
  stopWords <- stopwords("en")
  class(stopWords)
  
  column <- tolower(column)   #make it lower case
  
  '%nin%' <- Negate('%in%')
  column <- sapply(column, function(x) {   
    t <- unlist(strsplit(x, " "))
    t <- t[t %nin% locWords]#this gets rid of locations
    t <- paste(t, sep=" ", collapse= " ") 
  })
  
  column <- gsub("\\+\\+", "plusplus", column)    #want to preserve C skills when 
  column <- gsub("\\#", "sharp", column)          #we kill special characters
  column <- gsub("c/c", "ceecee", column)    
  
  column <- gsub("/", " ", column)    #get rid of slashes, confuses things
  column <- gsub("\\.", " ", column)  #get rid of periods
  column <- gsub("\\d", "", column)
  column <- gsub("/\\s\\s+/", " ", column) #strip excess white space
  
  '%nin%' <- Negate('%in%')
  column <- sapply(column, function(x) {   
    t <- unlist(strsplit(x, " "))
    t <- t[t %nin% stopWords] ##this gets rid of stopwords i.e., common words like article, simple verbs
    t <- SnowballStemmer(t)
    t <- unique(t)
    t <- paste(t, sep=" ", collapse= " ") 
  })
  
  column <- gsub("[[:punct:]]", " ", column)  #kill punctuation, do before following to preserve urls in sourcename
  column <- gsub("\\b[a-zA-Z0-9]{1,2}\\b", "", column) #remove single characters
  column <- gsub("/\\s\\s+/", " ", column) #strip excess white space
  
  
  wordList <- sapply(column, function(x) {
    t <- unlist(strsplit(x, " ")) 
  })
  
  wordList <- unlist(wordList)
  freqWords <- sort(table(wordList), decreasing=TRUE)
  
  if(doPlots == TRUE){
  png(file=paste(c(folder, "/", "hist_unstructTextWordFrequencies_High.png"), collapse=""))
  hist(freqWords[freqWords > 5000], main = "freq > 5000 Occurances", col="cornflowerblue")
  dev.off()
  
  
  png(file=paste(c(folder, "/", "hist_unstructTextWordFrequencies_Low.png"), collapse=""))
  hist(freqWords[freqWords < 5000], main = "Freq < 5000 Occurances", col="cornflowerblue")
  dev.off()
  
  png(file=paste(c(folder, "/", "hist_unstructTextWordFrequencies_ReallyLow.png"), collapse=""))
  hist(freqWords[freqWords < 1000], main = "Freq < 1000 Occurances", col="cornflowerblue")
  dev.off()
  
  png(file=paste(c(folder, "/", "hist_unstructTextWordFrequencies_ReallyVeryLow.png"), collapse=""))
  hist(freqWords[freqWords < 100], main = "Freq < 100 Occurances", col="cornflowerblue")
  dev.off()
  
  png(file=paste(c(folder, "/", "hist_unstructTextWordFrequencies.png"), collapse=""))
  hist(freqWords[-1], main = "Word count, All Freq", col="cornflowerblue", breaks=c(0,1,10,100,1000,5000,10000,15000,20000,25000,50000,100000))
  dev.off()
  
  png(file=paste(c(folder, "/", "hist_unstructTextWordFrequencies_firstHundred.png"), collapse=""))
  hist(freqWords[2:(wordQuota+1)], main = "Freq of Quota Most Common Words", col="cornflowerblue")
  dev.off()
  
  png(file=paste(c(folder, "/", "hist_unstructTextWordFrequencies_ThousandBreaks.png"), collapse=""))
  hist(freqWords[-1], main = "Word Count, All Freq (1000 bins)", col="cornflowerblue", breaks=1000)
  dev.off()
  

  png(file=paste(c(folder, "/", "hist_unstructTextWordFrequencies_handfuls.png"), collapse=""))
  hist(freqWords[freqWords < 10], main = "Freq < 10 Occurances", col="cornflowerblue")
  dev.off()
}

 
  if(length(goodList) > 0){
    killList <- rownames(freqWords)
    killList <- setdiff(killList, goodList)
  }

  if(length(goodList) == 0){
    killList <- rownames(freqWords[(wordQuota+2):length(freqWords)])
    killList <- sort(unique(killList))
    goodList <- rownames(freqWords[2:(wordQuota+1)])
  }
  
  '%nin%' <- Negate('%in%')
  column <- sapply(column, function(x) {   
    t <- unlist(strsplit(x, " "))
    t <- t[t %nin% killList]#this gets rid of locations
    t <- paste(t, sep=" ", collapse= " ") 
  })

  column <- gsub("/\\s\\s+/", " ", column) #strip excess white space -- it's persistent

  return(list(column, goodList))
  
}#end of pretreat text


#gets a SparseM sparse matrix from a data frame
getSparse <- function(dataFrame) {
    tmpMat <- as.matrix(dataFrame)
    sparseMat <- as.matrix.csr(tmpMat)
    rm(tmpMat)
    return(sparseMat)
} #end of getSparse



