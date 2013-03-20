#################################
# cogInTheMachine.R
# written by Jet Goodson 
# started on 18 March 2013
# contact at jetgoodson@gmail.com
#
# handling of SVM-regression alg on data
##################################


#setup and run SVM
cogInTheMachine <- function(dataset){
 library(e1071)
 library(sprint)
 
 if ( require("multicore", quietly = FALSE, warn.conflicts = TRUE) ) {
  # trainController$workers <- multicore:::detectCores()
  # trainController$computeFunction <- mclapply
  # trainController$computeArgs <- list(mc.preschedule = FALSE, mc.set.seed = FALSE)
  # cat(c(multicore:::detectCores(), " available cores\n"))
   
   registerDoMC(cores = multicore:::detectCores())
 }

 bestPoly = 2
 bestGamma = 0.5
 bestCost = 5


 tuned <- tuneMachine(dataset, gams=10^{-2:0}, costs=10^{-1:2}, degs=2:4)
 print(tuned$best.parameters)
 bestPoly <- 2
 bestGamma <- tuned$best.parameters[[1]]
 bestCost <- tuned$best.parameters[[2]]
 bestCoef <- bestGamma
 
 responseVector <- as.vector(as.matrix(dataset[,1]))
 model <- psvm(x = dataset[,-1], y = responseVector, scale=FALSE, type = "eps-regression",  kernel="polynomial", degree = bestPoly, coef0 = bestCoef, gamma = bestGamma, cost = bestCost, cross = 10, probability = TRUE)
 rm(responseVector)
 print(summary(model))
 return(model)
}#end of cogInTheMachine


#sets up model to be tuned in terms of cost and gamma (if needed)
tuneMachine <-function(dataset, gams, costs, degs) {
  library(e1071)
  library(sprint)

  cat("Tuning on ", nrow(dataset), " events\n")
  
  responseVector <- as.vector(as.matrix(dataset[,1]))
  tuned <- tune.svm(x = dataset[,-1], y = responseVector, type = "eps-regression", kernel="polynomial", scale=FALSE, coef0 = 0.5, gamma = gams, cost = costs, degree=degs)
  rm(responseVector)
  print(summary(tuned))
  
  return(tuned)
}#end of tuneMachine


#run model on validationFrame and check accuracy
validateSVM <- function(model, validationData) {
  library("randomForest")
  library("caret")

  validate <-predict(model, validationData[,-1])
  cat("SVM Predicted head: \n")
  print(head(validate))

  cat("SVM Actual head:\n")
  print(head(validationData[,1]))

  disparity <- validationData[,1] - validate

  cat(c("   SVM Total Disparity = ", sum(abs(disparity)), "\n"))
  cat(c("   SVM Average Disparity = ", sum(abs(disparity))/length(validate), "\n"))
  cat(c("   SVM SD of Disparity = ", sd(abs(disparity)), "\n"))

  png(file="hist_salaryDisparity_SVM.png")
  hist(disparity, main="SVM Disparity, Actual - Prediction", xlab="Disparity", ylab="entries", col="darkorchid4")
  dev.off()
  
  png(file="hist_salaryRelDisparity_SVM.png")
  hist(disparity/validationData[,1], main="SVM Relative Disparity, (Actual - Prediction)/Actual", xlab="Relative Disparity", ylab="entries", col="darkorange1")
  dev.off()
  
  png(file="scatter_predictionVSactual_salary_SVM.png")
  plot(validationData[,1], validate, main="SVM Predicted Salary versus Actual Salary", xlab="Actual Salary", ylab="Predicted Salary",col="cornflowerblue")
  dev.off()

  cat("Finished SVM validation.\n")

}#end of validateOrchard




#run the model over test data to produce predition
svmPredict <- function(model, testFrame, jobIds){

  testPredict <- predict(model, newdata=testFrame, type="raw")
  cat("SVM Prediction finished\n")
  results <- cbind(jobIds, testPredict)
  colnames(results) <- c("Id", "SalaryNormalized")
 
}#end of orchard predict
