#################################
# toolbox.R
# written by Jet Goodson 
# started on 15 March 2013
# contact at jetgoodson@gmail.com
#
# handling of random forest alg on data
##################################

randomOrchard <- function(trainFrame) {

  library("randomForest")
  library("caret")
  library('doMC')
  
  samplingSize     <- 10000

  rfMethod         <- "parRF"         #parRF == parallel random forest, in line is just rf
  validationMethod <- "repeatedcv"    #validation method for model
  folds            <- 2              #number of folds in cv
  repetitions      <- 20              #repetition for repeatedcv
  tuneCount        <- 10              #number of tunings for mtry
  treeCount        <- 500

  trainController <- trainControl(
    method = validationMethod,
    number=folds,
    repeats=repetitions,
    returnResamp = "final",
    classProbs = FALSE,
    returnData = FALSE
    )

  if ( require("multicore", quietly = FALSE, warn.conflicts = TRUE) ) {
    trainController$workers <- multicore:::detectCores()
    trainController$computeFunction <- mclapply
    trainController$computeArgs <- list(mc.preschedule = FALSE, mc.set.seed = FALSE)
    cat(c(multicore:::detectCores(), " available cores\n"))
    
    registerDoMC(cores = multicore:::detectCores())
  }
  
  theOrchard <- train(trainFrame[,-1], trainFrame[,1], method = "parRF", tuneLength = tuneCount, trControl = trainController, scale = FALSE, keep.forest=TRUE, sampsize=samplingSize, nTree = treeCount)
 
  return(theOrchard)
}#end of random orchard

#run model on validationFrame and check accuracy
validateOrchard <- function(model, validationData) {
  library("randomForest")
  library("caret")

  validate <-predict(model, validationData[,-1])
  cat("Predicted head: \n")
  print(head(validate))

  cat("Actual head:\n")
  print(head(validationData[,1]))

  disparity <- valididation[,1] - validate

  cat(c("   Total Disparity = ", sum(abs(disparity)), "\n"))
  cat(c("   Average Disparity = ", sum(abs(disparity))/length(validate), "\n"))
  cat(c("   SD of Disparity = ", sd(abs(disparity)), "\n"))

  png(file="hist_salaryDisparity.png")
  hist(disparity, main="Disparity, Actual - Prediction", xlab="Disparity", ylab="entries", col="darkorchid4")
  dev.off()
  
  png(file="hist_salaryRelDisparity.png")
  hist(disparity/validFrame[,1], main="Relative Disparity, (Actual - Prediction)/Actual", xlab="Relative Disparity", ylab="entries", col="darkorange1")
  dev.off()
  
  png(file="scatter_predictionVSactual_salary.png")
  plot(validFrame[,1], validate, main="Predicted Salary versus Actual Salary", xlab="Actual Salary", ylab="Predicted Salary",col="cornflowerblue")
  dev.off()

  cat("Finished validation.\n")

}#end of validateOrchard




#run the model over test data to produce predition
orchardPredict <- function(model, testFrame, jobIds){

  testPredict <- predict(model, newdata=testFrame, type="raw")
  cat("Prediction finished\n")
  results <- cbind(jobIds, testPredict)
  colnames(results) <- c("Id", "SalaryNormalized")
 
}#end of orchard predict
