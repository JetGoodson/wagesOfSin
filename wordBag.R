#################################
# wordBag.R
# written by Jet Goodson 
# started on 7 March 2013
# contact at jetgoodson@gmail.com
#
# makes a wordBag - intend mostly
# for analysis of common words
# not for vocab indexing
##################################


#makes a wordbag
wordBag <- function(input, bagName, doStem = TRUE){

  library("tm")

  bagOfHolding <- Corpus(DataframeSource(input))
  summary(bagOfHolding)
  bagOfHolding <- tm_map(bagOfHolding, removeNumbers)
  bagOfHolding <- tm_map(bagOfHolding, removePunctuation)
  bagOfHolding <- tm_map(bagOfHolding, stripWhitespace)
  bagOfHolding <- tm_map(bagOfHolding, tolower)
  bagOfHolding <- tm_map(bagOfHolding, removeWords, stopwords("english"))
  if(doStem==TRUE){
    bagOfHolding <- tm_map(bagOfHolding, stemDocument, language = "english")
  }
  terms <- TermDocumentMatrix(bagOfHolding)
  #matrixTerms <- as.matrix(terms)
  #matrixTerms <- rowSums(matrixTerms[,-1])
  return(findFreqTerms(terms, 1))
  
}#end of wordbag

#makes a text file for wordbagging
makeTextFile <- function(input, textFile){

  library("tm")
  write.table(input, file=textFile, row.names=FALSE, col.names=FALSE)

}#end of wordbag


#function to take a dataframe of key words and turn the key words into features.
featureBag <- function(frame) {
  library('tm') 
  library('doMC')
  library('plyr')
  
  if ( require("multicore", quietly = FALSE, warn.conflicts = TRUE) ) {
    cat(c(multicore:::detectCores(), " available cores\n"))
    registerDoMC(cores = multicore:::detectCores())
  }
  
  freqMat <- data.frame()
  
  split <- 50000
  max <- nrow(frame)
  lows <- seq(1, max, by=split)
  highs <- vector()
  for(m in 1:length(lows)){
  highs <- c(highs, lows[m] + split - 1)  
  }
  if(highs[length(highs)] > max){
    highs[length(highs)] <- max
  }
    
  maxIt <- 1 + as.integer((nrow(frame) - 1)/split)
  print(maxIt)
  print(lows)
  print(highs)
 
  freqMat <- foreach(v = 1:maxIt, .combine=rbind.fill) %dopar% {
    cat(c("Word bag", v, " starting on", lows[v], " to ", highs[v], "\n"))
    wordBag <- Corpus(DataframeSource(frame[lows[v]:highs[v],]))
    wordBag <- TermDocumentMatrix(wordBag)
    wordBag <- as.matrix(wordBag)
    cat(c("Word bag", v, " finished\n"))
    wordBag <- t(wordBag)
    wordBag <- as.data.frame(wordBag)
   # print(head(wordBag))
  }#end of foreach loop

  return(freqMat)
}#end of feature bag
