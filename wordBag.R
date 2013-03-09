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
