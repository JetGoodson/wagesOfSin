#################################
# randomOrchard.R
# written by Jet Goodson 
# started on 15 March 2013
# contact at jetgoodson@gmail.com
#
# handling of random forest alg on data
##################################

#set up and run random forest
randomOrchard <- function(trainFrame, samplingSize = 1000, folds = 2, repetitions = 5, tuneCount = 10, treeCount = 500) {

  library("randomForest")
  library("caret")
  library('doMC')
  
  #samplingSize     <- 1000    # these arguments are commented out because I moved them to function arguments

  rfMethod         <- "parRF"         #parRF == parallel random forest, in line is just rf
  validationMethod <- "repeatedcv"    #validation method for model
 # folds            <- 2              #number of folds in cv
 # repetitions      <- 1              #repetition for repeatedcv
 # tuneCount        <- 3              #number of tunings for mtry
 # treeCount        <- 500

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
  
  cat("Training the orchard\n")
  
  theOrchard <- train(trainFrame[,-1], trainFrame[,1], method = "parRF", tuneLength = tuneCount, trControl = trainController, scale = FALSE, keep.forest=TRUE, sampsize=samplingSize, nTree = treeCount, na.action=na.omit)

  cat("Orchard trained\n")
  
  return(theOrchard)
}#end of random orchard

#run model on validationFrame and check accuracy
validateOrchard <- function(model, validationData) {
  library("randomForest")
  library("caret")

  cat("Validating RF:\n")
  
  validate <-predict(model, validationData[,-1])
  cat("RF Predicted head: \n")
  print(head(validate))

  cat("RF Actual head:\n")
  print(head(validationData[,1]))

  disparity <- validationData[,1] - validate

  cat(c("   RF Total Disparity = ", sum(abs(disparity)), "\n"))
  cat(c("   RF Average Disparity = ", sum(abs(disparity))/length(validate), "\n"))
  cat(c("   RF SD of Disparity = ", sd(abs(disparity)), "\n"))

  png(file="hist_salaryDisparity_RF.png")
  hist(disparity, main="RF Disparity, Actual - Prediction", xlab="Disparity", ylab="entries", col="darkorchid4")
  dev.off()
  
  png(file="hist_salaryRelDisparity_RF.png")
  hist(disparity/validationData[,1], main="RF Relative Disparity, (Actual - Prediction)/Actual", xlab="Relative Disparity", ylab="entries", col="darkorange1")
  dev.off()
  
  png(file="scatter_predictionVSactual_salary_RF.png")
  plot(validationData[,1], validate, main="RF Predicted Salary versus Actual Salary", xlab="Actual Salary", ylab="Predicted Salary",col="cornflowerblue")
  dev.off()

  cat("Finished RF validation.\n")

}#end of validateOrchard




#run the model over test data to produce predition
orchardPredict <- function(model, testFrame, jobIds){

  testPredict <- predict(model, newdata=testFrame, type="raw")
  cat("RF Prediction finished\n")
  results <- cbind(jobIds, testPredict)
  colnames(results) <- c("Id", "SalaryNormalized")
 
}#end of orchard predict
