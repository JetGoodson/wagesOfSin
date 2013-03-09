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
  
  library(plyr)
  library(tm)
  library(Snowball)
  require(stringr)
  stopWords <- stopwords("en")
  class(stopWords)

  load(file="data/locationWordBag.rda")#gives locWords
  
  dataFrame$FullDescription <- paste(dataFrame$FullDescription, dataFrame$Title, sep=" ") #add title to fulldescription
  dataFrame$FullDescription <- tolower(dataFrame$FullDescription)   #make it lower case
  
  dataFrame$FullDescription <- gsub("[_,\\*]", "", dataFrame$FullDescription)  ##get rid of where they replaced salaries with asterisks.
  #below gets rid of websites, which I don't think we need to keep, they mess things up
  dataFrame$FullDescription <- gsub("(((file|gopher|news|nntp|telnet|http|ftp|https|ftps|sftp)://)|(www\\.))+(([a-zA-Z0-9\\._-]+\\.[a-zA-Z]{2,6})|([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}))(/[a-zA-Z0-9\\&amp;%_\\./-~-]*)?", "", dataFrame$FullDescription)
  dataFrame$FullDescription <- gsub("^.*(.com|.co.uk).*$", "", dataFrame$FullDescription)   #destroy british websites
  dataFrame$FullDescription <- gsub("/", " ", dataFrame$FullDescription)    #get rid of slashes, confuses things
  dataFrame$FullDescription <- gsub("\\.", " ", dataFrame$FullDescription)  #get rid of periods
  
  
  '%nin%' <- Negate('%in%')
  dataFrame$FullDescription <- lapply(dataFrame$FullDescription, function(x) {   
    t <- unlist(strsplit(x, " "))
    t <- t[t %nin% stopWords] ##this gets rid of stopwords i.e., common words like article, simple verbs
    t <- t[t %nin% locWords]#this gets rid of locations
    t <- SnowballStemmer(t)
    t <- paste(t, sep=" ", collapse= " ") 
  })

  ##Just stem with tm_map tool in lapply? Maybe even do same lapply
  ##bagOfHolding <- tm_map(bagOfHolding, stemDocument, language = "english")

  
  dataFrame$FullDescription <- gsub("[[:punct:]]", " ", dataFrame$FullDescription)  #kill punctuation, do before following to preserve urls in sourcename
  
  
  dataFrame$FullDescription <- paste(dataFrame$FullDescription, tolower(dataFrame$Company), tolower(dataFrame$SourceName), sep=" ")  #merge text  fields into 1
  dataFrame <- dataFrame[,!(colnames(dataFrame) %in% c("Title", "Company","SourceName"))]   #get rid of extra columns
  
  
  dataFrame$FullDescription <- gsub("\\d", " ", dataFrame$FullDescription)  #get rid of digits
  dataFrame$FullDescription <- gsub("/\\s\\s+/", " ", dataFrame$FullDescription) #strip excess white space
  dataFrame$FullDescription <- str_trim(dataFrame$FullDescription)
  
  
  dataFrame$Category <- gsub("Accounting & Finance Jobs", "1", dataFrame$Category)   #this changes this column to a number for each category
  dataFrame$Category <- gsub("Admin Jobs", "2", dataFrame$Category)
  dataFrame$Category <- gsub("Charity & Voluntary Jobs", "3", dataFrame$Category)
  dataFrame$Category <- gsub("Consultancy Jobs", "4", dataFrame$Category)
  dataFrame$Category <- gsub("Creative & Design Jobs", "5", dataFrame$Category)
  dataFrame$Category <- gsub("Customer Services Jobs", "6", dataFrame$Category)
  dataFrame$Category <- gsub("Domestic Help & Cleaning Jobs", "7", dataFrame$Category)
  dataFrame$Category <- gsub("Energy, Oil & Gas Jobs", "8", dataFrame$Category)
  dataFrame$Category <- gsub("Engineering Jobs", "9", dataFrame$Category)
  dataFrame$Category <- gsub("Graduate Jobs", "10", dataFrame$Category)
  dataFrame$Category <- gsub("Healthcare & Nursing Jobs", "11", dataFrame$Category)
  dataFrame$Category <- gsub("Hospitality & Catering Jobs", "12", dataFrame$Category)
  dataFrame$Category <- gsub("HR & Recruitment Jobs", "13", dataFrame$Category)
  dataFrame$Category <- gsub("IT Jobs", "14", dataFrame$Category)
  dataFrame$Category <- gsub("Legal Jobs", "15", dataFrame$Category)
  dataFrame$Category <- gsub("Logistics & Warehouse Jobs", "16", dataFrame$Category)
  dataFrame$Category <- gsub("Maintenance Jobs", "17", dataFrame$Category)
  dataFrame$Category <- gsub("Manufacturing Jobs", "18", dataFrame$Category)
  dataFrame$Category <- gsub("Other/General Jobs", "19", dataFrame$Category)
  dataFrame$Category <- gsub("PR, Advertising & Marketing Jobs", "20", dataFrame$Category)
  dataFrame$Category <- gsub("Property Jobs", "21", dataFrame$Category)
  dataFrame$Category <- gsub("Retail Jobs", "22", dataFrame$Category)
  dataFrame$Category <- gsub("Sales Jobs", "23", dataFrame$Category)
  dataFrame$Category <- gsub("Scientific & QA Jobs", "24", dataFrame$Category)
  dataFrame$Category <- gsub("Social Work Jobs", "25", dataFrame$Category)
  dataFrame$Category <- gsub("Teaching Jobs", "26", dataFrame$Category)
  dataFrame$Category <- gsub("Trade & Construction Jobs", "27", dataFrame$Category)
  dataFrame$Category <- gsub("Travel Jobs" , "28", dataFrame$Category)
  
   dataFrame$ContractTime <- gsub("contract" , "1", dataFrame$ContractTime)    #change to numbers
  dataFrame$ContractTime <- gsub("permanent" , "2", dataFrame$ContractTime)
  
 dataFrame$ContractType <- gsub("part_time" , "2", dataFrame$ContractType)   #change to numbers
  dataFrame$ContractType <- gsub("full_time" , "2", dataFrame$ContractType)
  
  
  return(dataFrame)
}#end of transmogrify
