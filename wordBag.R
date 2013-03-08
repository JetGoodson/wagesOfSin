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
wordBag <- function(input, bagName){

  library("tm")

  bagOfHolding <- Corpus(DataframeSource(input))
  summary(bagOfHolding)
  bagOfHolding <- tm_map(bagOfHolding, removeNumbers)
  bagOfHolding <- tm_map(bagOfHolding, removePunctuation)
  bagOfHolding <- tm_map(bagOfHolding, stripWhitespace)
  bagOfHolding <- tm_map(bagOfHolding, tolower)
  bagOfHolding <- tm_map(bagOfHolding, removeWords, stopwords("english"))
  bagOfHolding <- tm_map(bagOfHolding, stemDocument, language = "english")

  terms <- TermDocumentMatrix(bagOfHolding)
  matrixTerms <- as.matrix(terms)
  matrixTerms <- rowSums(matrixTerms[,-1])
  print(matrixTerms)
  return(matrixTerms)
  
}#end of wordbag

#makes a wordbag
makeTextFile <- function(input, textFile){

  library("tm")
  write.table(input, file=textFile, row.names=FALSE, col.names=FALSE)

}#end of wordbag
