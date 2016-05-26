
DataSplit = function(x, perc, removeFirst, targetVar){

  #Number of data points in the training set
  nTrain = ceiling(dim(x)[1] * perc)
  
  #Random data points for training set
  idxTrain = sample(x=as.integer(rownames(x)), size=nTrain)
  
  #Get Training and Validation sets
  finalData = {}
  finalData$Train = x[idxTrain,]
  finalData$Valid = x[-idxTrain,]
  
  #Remove index variable
  #Get column index of index variable
  if (removeFirst){
    #Move index values to row names and delete index variable
    rownames(finalData$Train) = finalData$Train[,1]
    finalData$Train = finalData$Train[, -1]
    
    rownames(finalData$Valid) = finalData$Valid[,1]
    finalData$Valid = finalData$Valid[, -1]
  }

  #Get index of Target Variable
  finalData$targetIdx = which(colnames(finalData$Train)==targetVar)
  
  return(finalData)
}

